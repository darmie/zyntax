package zyntax;

#if (macro || zyntax_runtime)

import zyntax.HIR;
import zyntax.AST;

/**
 * Builder for constructing HIR from Zyntax AST
 *
 * AST types (Module, Function, Expr, ZType) are imported from AST.hx
 */
class HirBuilder {
    var module: HirModule;
    var currentFunction: Null<HirFunction>;
    var currentBlock: Null<HirId>;
    var functionRegistry: Map<String, HirId>;
    var valueCounter: Int = 0;
    var variableScope: Map<String, HirId>; // Maps variable names to their value IDs

    public function new(moduleName: String) {
        module = {
            id: generateUuid(),
            name: moduleName,
            functions: new Map(),
            globals: new Map(),
            types: new Map(),
            imports: [],
            exports: [],
            version: 1
        };
        functionRegistry = new Map();
        variableScope = new Map();
    }

    public function buildFromModule(ast: zyntax.AST.Module): HirModule {
        // First pass: declare all functions to build symbol table
        for (func in ast.functions) {
            var funcId = generateUuid();
            functionRegistry.set(func.name, funcId);

            // Create function signature
            var params = func.params.map(p -> ({
                id: generateUuid(),
                name: p.name,
                ty: convertType(p.ty),
                attributes: {
                    "by_ref": false,
                    "sret": false,
                    "zext": false,
                    "sext": false,
                    "noalias": false,
                    "nonnull": false,
                    "readonly": false
                }
            }: HirParameter));

            var signature: HirSignature = {
                params: params,
                returns: [convertType(func.return_type)],
                is_variadic: false,
                is_async: false
            };

            var hirFunc: HirFunction = {
                id: funcId,
                name: func.name,
                signature: signature,
                calling_convention: func.is_external ? CallingConvention.Cdecl : CallingConvention.Rust,
                is_external: func.is_external,
                entry_block: generateUuid(),
                blocks: new Map(),
                values: new Map()
            };

            module.functions.set(funcId, hirFunc);
        }

        // Add runtime functions to registry
        addRuntimeFunction("$String$println", [HirType.Ptr(HirType.I8)], [HirType.Void]);
        addRuntimeFunction("$Int$println", [HirType.I32], [HirType.Void]);
        addRuntimeFunction("$Float$println", [HirType.F64], [HirType.Void]);
        addRuntimeFunction("$Bool$println", [HirType.Bool], [HirType.Void]);

        // Add toString functions
        addRuntimeFunction("$Int$toString", [HirType.I32], [HirType.Ptr(HirType.I8)]);
        addRuntimeFunction("$Float$toString", [HirType.F64], [HirType.Ptr(HirType.I8)]);
        addRuntimeFunction("$Bool$toString", [HirType.Bool], [HirType.Ptr(HirType.I8)]);

        // Add string concatenation
        addRuntimeFunction("$String$concat", [HirType.Ptr(HirType.I8), HirType.Ptr(HirType.I8)], [HirType.Ptr(HirType.I8)]);

        // Second pass: build function bodies
        for (func in ast.functions) {
            if (func.body != null && !func.is_external) {
                buildFunctionBody(func);
            }
        }

        return module;
    }

    function addRuntimeFunction(name: String, paramTypes: Array<HirType>, returnTypes: Array<HirType>) {
        var funcId = generateUuid();
        functionRegistry.set(name, funcId);

        var params = paramTypes.map(ty -> ({
            id: generateUuid(),
            name: "arg",
            ty: ty,
            attributes: {
                "by_ref": false,
                "sret": false,
                "zext": false,
                "sext": false,
                "noalias": false,
                "nonnull": false,
                "readonly": false
            }
        }: HirParameter));

        var signature: HirSignature = {
            params: params,
            returns: returnTypes,
            is_variadic: false,
            is_async: false
        };

        var hirFunc: HirFunction = {
            id: funcId,
            name: name,
            signature: signature,
            calling_convention: CallingConvention.Cdecl,
            is_external: true,
            entry_block: generateUuid(),
            blocks: new Map(),
            values: new Map()
        };

        module.functions.set(funcId, hirFunc);
    }

    function buildFunctionBody(func: zyntax.AST.Function) {
        var funcId = functionRegistry.get(func.name);
        if (funcId == null) return;

        currentFunction = module.functions.get(funcId);
        if (currentFunction == null) return;

        // Clear variable scope for new function
        variableScope = new Map();

        // Create entry block
        var entryBlockId = currentFunction.entry_block;
        var entryBlock: HirBasicBlock = {
            id: entryBlockId,
            label: "entry",
            phis: [],
            instructions: [],
            terminator: HirTerminator.Unreachable
        };

        currentBlock = entryBlockId;
        currentFunction.blocks.set(entryBlockId, entryBlock);

        // Add parameters as values and to variable scope
        var paramIndex = 0;
        for (param in currentFunction.signature.params) {
            currentFunction.values.set(param.id, {
                id: param.id,
                ty: param.ty,
                kind: HirValueKind.Parameter(paramIndex),
                uses: [],
                span: null
            });
            // Add parameter to variable scope using the original parameter name
            var paramName = func.params[paramIndex].name;
            variableScope.set(paramName, param.id);
            paramIndex++;
        }

        // Build function body
        if (func.body != null) {
            var returnValue = buildExpression(func.body.expr);

            // Add return terminator
            var block = currentFunction.blocks.get(currentBlock);
            if (block != null) {
                if (returnValue != null) {
                    block.terminator = HirTerminator.Return([returnValue]);
                } else {
                    block.terminator = HirTerminator.Return([]);
                }
                // Update the block back into the map (Haxe passes structs by value)
                currentFunction.blocks.set(currentBlock, block);
            }
        }

        // Update the function in the module
        module.functions.set(funcId, currentFunction);
        currentFunction = null;
        currentBlock = null;
    }

    function buildExpression(expr: zyntax.AST.Expr): Null<HirId> {
        return switch (expr) {
            case IntLiteral(value):
                createConstant(HirConstant.I32(value), HirType.I32);

            case BoolLiteral(value):
                createConstant(HirConstant.Bool(value), HirType.Bool);

            case StringLiteral(value):
                createStringGlobal(value);

            case BinaryOp(op, left, right):
                var leftVal = buildExpression(left.expr);
                var rightVal = buildExpression(right.expr);
                if (leftVal == null || rightVal == null) return null;

                var resultId = generateUuid();
                var hirOp = convertBinaryOp(op);
                var resultType = HirType.I32; // TODO: proper type inference

                addInstruction(HirInstruction.Binary(hirOp, resultId, resultType, leftVal, rightVal));
                createValue(resultId, resultType, HirValueKind.Instruction);

                resultId;

            case Call(callee, args):
                var argVals = args.map(a -> buildExpression(a.expr)).filter(a -> a != null);

                var calleeId: HirCallable = switch (callee.expr) {
                    case Variable(name):
                        // Check if it's a known function
                        var funcId = functionRegistry.get(name);
                        if (funcId != null) {
                            HirCallable.Function(funcId);
                        } else {
                            // Unknown function - could be external
                            var calleeVal = buildExpression(callee.expr);
                            calleeVal != null ? HirCallable.Indirect(calleeVal) : null;
                        }
                    default:
                        var calleeVal = buildExpression(callee.expr);
                        calleeVal != null ? HirCallable.Indirect(calleeVal) : null;
                };

                if (calleeId == null) return null;

                var resultId = generateUuid();
                addInstruction(HirInstruction.Call(resultId, calleeId, argVals, [], false));
                createValue(resultId, HirType.I32, HirValueKind.Instruction); // TODO: proper return type

                resultId;

            case Block(exprs):
                var lastValue: Null<HirId> = null;
                for (e in exprs) {
                    lastValue = buildExpression(e.expr);
                }
                lastValue;

            case Let(name, ty, init):
                // Evaluate the initializer and add variable to scope
                if (init != null) {
                    var initVal = buildExpression(init.expr);
                    if (initVal != null) {
                        variableScope.set(name, initVal);
                    }
                    initVal;
                } else {
                    null;
                }

            case Variable(name):
                // Look up variable in scope
                variableScope.get(name);

            case If(cond, thenExpr, elseExpr):
                // TODO: Implement control flow with PHI nodes
                null;

            case Return(value):
                // Return is handled at the function level
                if (value != null) {
                    buildExpression(value.expr);
                } else {
                    null;
                }

            case While(cond, body):
                // TODO: Implement loop control flow
                null;

            case FloatLiteral(value):
                createConstant(HirConstant.F64(value), HirType.F64);
        }
    }

    function createConstant(constant: HirConstant, ty: HirType): HirId {
        var id = generateUuid();
        createValue(id, ty, HirValueKind.Constant(constant));
        return id;
    }

    function createStringGlobal(str: String): HirId {
        var globalId = generateUuid();
        var valueId = generateUuid();

        // Create global with Haxe String format: [length:i32][utf8_bytes...]
        var global: HirGlobal = {
            id: globalId,
            name: "_str_" + globalId.substr(0, 8),
            ty: HirType.Ptr(HirType.I8),
            initializer: HirConstant.String(str),
            is_const: true,
            is_thread_local: false,
            linkage: Linkage.Private,
            visibility: Visibility.Default
        };

        module.globals.set(globalId, global);

        // Create value referencing the global
        createValue(valueId, HirType.Ptr(HirType.I8), HirValueKind.Global(globalId));
        return valueId;
    }

    function createValue(id: HirId, ty: HirType, kind: HirValueKind) {
        if (currentFunction != null) {
            currentFunction.values.set(id, {
                id: id,
                ty: ty,
                kind: kind,
                uses: [],
                span: null
            });
        }
    }

    function addInstruction(inst: HirInstruction) {
        if (currentBlock != null && currentFunction != null) {
            var block = currentFunction.blocks.get(currentBlock);
            if (block != null) {
                block.instructions.push(inst);
                // Update the block back into the map (Haxe passes structs by value)
                currentFunction.blocks.set(currentBlock, block);
            }
        }
    }

    function convertType(ty: zyntax.AST.ZType): HirType {
        return switch (ty) {
            case zyntax.AST.ZType.Primitive("Unit"): HirType.Void;
            case zyntax.AST.ZType.Primitive("Bool"): HirType.Bool;
            case zyntax.AST.ZType.Primitive("I8"): HirType.I8;
            case zyntax.AST.ZType.Primitive("I16"): HirType.I16;
            case zyntax.AST.ZType.Primitive("I32"): HirType.I32;
            case zyntax.AST.ZType.Primitive("I64"): HirType.I64;
            case zyntax.AST.ZType.Primitive("U8"): HirType.U8;
            case zyntax.AST.ZType.Primitive("U16"): HirType.U16;
            case zyntax.AST.ZType.Primitive("U32"): HirType.U32;
            case zyntax.AST.ZType.Primitive("U64"): HirType.U64;
            case zyntax.AST.ZType.Primitive("F32"): HirType.F32;
            case zyntax.AST.ZType.Primitive("F64"): HirType.F64;
            case zyntax.AST.ZType.Primitive("String"): HirType.Ptr(HirType.I8);
            case zyntax.AST.ZType.Function(params, ret):
                // For function types, we'd need a proper representation
                // For now, just use a pointer
                HirType.Ptr(HirType.I8);
            case zyntax.AST.ZType.Primitive(_):
                trace("Unknown primitive type: " + ty);
                HirType.Void;
        }
    }

    function convertBinaryOp(op: String): zyntax.HIR.BinaryOp {
        return switch (op) {
            case "+": zyntax.HIR.BinaryOp.Add;
            case "-": zyntax.HIR.BinaryOp.Sub;
            case "*": zyntax.HIR.BinaryOp.Mul;
            case "/": zyntax.HIR.BinaryOp.Div;
            case "%": zyntax.HIR.BinaryOp.Rem;
            case "&": zyntax.HIR.BinaryOp.And;
            case "|": zyntax.HIR.BinaryOp.Or;
            case "^": zyntax.HIR.BinaryOp.Xor;
            case "<<": zyntax.HIR.BinaryOp.Shl;
            case ">>": zyntax.HIR.BinaryOp.Shr;
            case "==": zyntax.HIR.BinaryOp.Eq;
            case "!=": zyntax.HIR.BinaryOp.Ne;
            case "<": zyntax.HIR.BinaryOp.Lt;
            case "<=": zyntax.HIR.BinaryOp.Le;
            case ">": zyntax.HIR.BinaryOp.Gt;
            case ">=": zyntax.HIR.BinaryOp.Ge;
            default:
                trace("Unknown binary op: " + op);
                zyntax.HIR.BinaryOp.Add;
        }
    }

    static var uuidCounter = 0;
    static function generateUuid(): String {
        // Simple UUID generation for Haxe
        // Format: xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx
        var chars = "0123456789abcdef";
        var uuid = "";
        for (i in 0...36) {
            if (i == 8 || i == 13 || i == 18 || i == 23) {
                uuid += "-";
            } else if (i == 14) {
                uuid += "4";
            } else if (i == 19) {
                uuid += chars.charAt(8 + Std.random(4));
            } else {
                uuid += chars.charAt(Std.random(16));
            }
        }
        return uuid;
    }

    public function finish(): HirModule {
        return module;
    }
}

#end
