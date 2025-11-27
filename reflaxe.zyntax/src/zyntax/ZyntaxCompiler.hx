package zyntax;

#if (macro || zyntax_runtime)

import haxe.macro.Type;
import haxe.macro.Expr;
import haxe.macro.Context;
import reflaxe.GenericCompiler;
import reflaxe.data.ClassFuncData;
import reflaxe.data.ClassVarData;
import reflaxe.data.EnumOptionData;
import reflaxe.output.DataAndFileInfo;
import reflaxe.output.StringOrBytes;
import zyntax.HirBuilder;
import zyntax.BytecodeEmitter;

/**
 * Zyntax Compiler - Converts Haxe Typed AST to Zyntax TypedAST JSON
 */
class ZyntaxCompiler extends GenericCompiler<AST.Module, AST.Enum, AST.Expr> {

    /**
     * Compile Haxe class to Zyntax AST module
     */
    public function compileClassImpl(classType: ClassType, varFields: Array<ClassVarData>, funcFields: Array<ClassFuncData>): Null<AST.Module> {
        // Check if this is an extern class
        var isExternClass = classType.isExtern;

        // For extern classes, check for @:native metadata for module prefix
        var classNativePrefix: Null<String> = null;
        if (classType.meta.has(":native")) {
            var nativeMeta = classType.meta.extract(":native");
            if (nativeMeta.length > 0 && nativeMeta[0].params != null && nativeMeta[0].params.length > 0) {
                switch (nativeMeta[0].params[0].expr) {
                    case EConst(CString(s, _)):
                        classNativePrefix = s;
                    default:
                }
            }
        }

        // For now, only process static functions as top-level functions
        var functions = [];

        for (func in funcFields) {
            if (func.isStatic) {
                var params = func.args.map(arg -> {
                    name: arg.getName(),
                    ty: convertType(arg.type),
                    pos: func.field.pos
                });

                var returnType = convertType(func.ret);
                var body = func.expr != null ? {
                    expr: compileExpressionOrError(func.expr),
                    ty: convertType(func.expr.t),
                    pos: func.expr.pos
                } : null;

                // Check if function is external
                var isExternal = isExternClass || func.expr == null;
                var linkName: Null<String> = null;

                // For extern class functions, use the function name directly
                // unless overridden by @:native on the function itself
                if (isExternal) {
                    linkName = func.field.name;
                }

                // Check for @:native metadata on the function to override the link name
                if (func.field.meta != null) {
                    for (meta in func.field.meta.get()) {
                        if (meta.name == ":native" && meta.params != null && meta.params.length > 0) {
                            switch (meta.params[0].expr) {
                                case EConst(CString(s, _)):
                                    linkName = s;
                                    isExternal = true;
                                default:
                            }
                        }
                    }
                }

                functions.push({
                    name: func.field.name,
                    params: params,
                    return_type: returnType,
                    body: body,
                    is_external: isExternal,
                    link_name: linkName,
                    pos: func.field.pos
                });
            }
        }

        if (functions.length == 0) return null;

        return {
            name: classType.name,
            functions: functions
        };
    }

    /**
     * Compile Haxe enum (skip for now)
     */
    public function compileEnumImpl(enumType: EnumType, constructs: Array<EnumOptionData>): Null<AST.Enum> {
        return null;
    }

    /**
     * Compile Haxe expressions to Zyntax expressions
     * Now returns just the Expr, wrapping happens in helper
     */
    public function compileExpressionImpl(expr: TypedExpr, topLevel: Bool): Null<AST.Expr> {
        return switch(expr.expr) {
            case TConst(TInt(i)):
                IntLiteral(i);

            case TConst(TFloat(f)):
                FloatLiteral(Std.parseFloat(f));

            case TConst(TString(s)):
                StringLiteral(s);

            case TConst(TBool(b)):
                BoolLiteral(b);

            case TConst(TNull):
                // Represent null as 0 pointer
                IntLiteral(0);

            case TLocal(v):
                Variable(v.name);

            case TField(_, fa):
                var fieldName = switch(fa) {
                    case FStatic(_, cf): cf.get().name;
                    case FInstance(_, _, cf): cf.get().name;
                    case FAnon(cf): cf.get().name;
                    case FClosure(_, cf): cf.get().name;
                    case FDynamic(s): s;
                    case FEnum(_, ef): ef.name;
                };
                Variable(fieldName);

            case TBinop(op, e1, e2):
                var left = wrapExpr(e1);
                var right = wrapExpr(e2);
                BinaryOp(convertBinop(op), left, right);

            case TCall(callee, args):
                // Check if this is a method call on a standard library type
                switch(callee.expr) {
                    case TField(obj, fa):
                        // Get the method name
                        var methodName = switch(fa) {
                            case FInstance(_, _, cf): cf.get().name;
                            case FAnon(cf): cf.get().name;
                            case FClosure(_, cf): cf.get().name;
                            default: null;
                        };

                        // Get the object type
                        var objType = switch(obj.t) {
                            case TInst(t, _): t.get().name;
                            default: null;
                        };

                        // Transform stdlib method calls: arr.push(x) -> $Array$push(arr, x)
                        if (methodName != null && objType != null) {
                            var stdlibFunc = "$" + objType + "$" + methodName; // e.g., "$Array$push"
                            var stdlibCallee: AST.ExprWithPos = {
                                expr: AST.Expr.Variable(stdlibFunc),
                                ty: convertType(expr.t),
                                pos: callee.pos
                            };
                            var objExpr = wrapExpr(obj);
                            var argExprs = args.map(a -> wrapExpr(a));
                            AST.Expr.Call(stdlibCallee, [objExpr].concat(argExprs));
                        } else {
                            // Not a stdlib method call, use regular call
                            var calleeExpr = wrapExpr(callee);
                            var argExprs = args.map(a -> wrapExpr(a));
                            AST.Expr.Call(calleeExpr, argExprs);
                        }

                    default:
                        // Regular function call
                        var calleeExpr = wrapExpr(callee);
                        var argExprs = args.map(a -> wrapExpr(a));
                        AST.Expr.Call(calleeExpr, argExprs);
                }

            case TBlock(exprs):
                Block(exprs.map(e -> wrapExpr(e)));

            case TReturn(e):
                Return(e != null ? wrapExpr(e) : null);

            case TVar(v, init):
                Let(v.name, convertType(v.t), init != null ? wrapExpr(init) : null);

            case TIf(cond, thenExpr, elseExpr):
                var condE = wrapExpr(cond);
                var thenE = wrapExpr(thenExpr);
                var elseE = elseExpr != null ? wrapExpr(elseExpr) : null;
                If(condE, thenE, elseE);

            case TWhile(cond, body, normalWhile):
                var condE = wrapExpr(cond);
                var bodyE = wrapExpr(body);
                While(condE, bodyE);

            case TFor(v, iter, body):
                // For loop: for (item in iterator) body
                // v is the iteration variable, iter is the iterator expression, body is the loop body
                var iterE = wrapExpr(iter);
                var bodyE = wrapExpr(body);
                For(v.name, convertType(v.t), iterE, bodyE);

            case TBreak:
                Break;

            case TContinue:
                Continue;

            case TArrayDecl(elements):
                // Transform [1, 2, 3] into $Array$create(1, 2, 3)
                var elementExprs = elements.map(e -> wrapExpr(e));
                var arrayCreateCallee: AST.ExprWithPos = {
                    expr: AST.Expr.Variable("$Array$create"),
                    ty: convertType(expr.t),
                    pos: expr.pos
                };
                AST.Expr.Call(arrayCreateCallee, elementExprs);

            case TArray(arrayExpr, indexExpr):
                // Transform arr[i] into $Array$get(arr, i)
                var arrE = wrapExpr(arrayExpr);
                var idxE = wrapExpr(indexExpr);
                var arrayGetCallee: AST.ExprWithPos = {
                    expr: AST.Expr.Variable("$Array$get"),
                    ty: convertType(expr.t),
                    pos: expr.pos
                };
                AST.Expr.Call(arrayGetCallee, [arrE, idxE]);

            case TParenthesis(e):
                // Parentheses are just for grouping, compile the inner expression
                compileExpressionImpl(e, topLevel);

            default:
                // Debug: Print unsupported expression type
                #if macro
                Context.warning("Unsupported expression type: " + expr.expr, expr.pos);
                #end
                null;
        }
    }

    /**
     * Helper to wrap TypedExpr into ExprWithPos with type information
     */
    function wrapExpr(expr: TypedExpr): AST.ExprWithPos {
        return {
            expr: compileExpressionOrError(expr),
            ty: convertType(expr.t),
            pos: expr.pos
        };
    }

    /**
     * Generate output files (TypedAST JSON)
     */
    public function generateOutputIterator(): Iterator<DataAndFileInfo<StringOrBytes>> {
        var index = 0;
        return {
            hasNext: function() {
                return index < classes.length;
            },
            next: function() {
                var cls = classes[index++];

                // Generate TypedAST JSON using Generator
                var jsonStr = Generator.generateModule(cls.data);
                if (jsonStr == null) {
                    jsonStr = "{}";
                }

                return cls.withOutput(StringOrBytes.fromString(jsonStr));
            }
        };
    }

    /**
     * Convert Haxe Type to Zyntax Type
     */
    function convertType(t: Type): AST.ZType {
        return switch(t) {
            case TInst(_.get() => c, params):
                switch(c.name) {
                    case "Int": Primitive("I32");
                    case "Float": Primitive("F64");
                    case "Bool": Primitive("Bool");
                    case "String": Primitive("String");
                    default: Primitive("I32");
                }

            case TAbstract(_.get() => a, params):
                switch(a.name) {
                    case "Int": Primitive("I32");
                    case "Float": Primitive("F64");
                    case "Bool": Primitive("Bool");
                    case "Void": Primitive("Unit");
                    default: Primitive("I32");
                }

            case TType(_.get() => dt, params):
                convertType(dt.type);

            case TFun(args, ret):
                // Function type: convert params and return type
                var paramTypes = args.map(arg -> convertType(arg.t));
                var returnType = convertType(ret);
                Function(paramTypes, returnType);

            default:
                Primitive("I32");
        }
    }

    /**
     * Convert Haxe binary operator to string
     */
    function convertBinop(op: Binop): String {
        return switch(op) {
            case OpAdd: "Add";
            case OpSub: "Sub";
            case OpMult: "Mul";
            case OpDiv: "Div";
            case OpMod: "Mod";
            case OpEq: "Eq";
            case OpNotEq: "Ne";
            case OpLt: "Lt";
            case OpLte: "Le";
            case OpGt: "Gt";
            case OpGte: "Ge";
            case OpBoolAnd: "And";
            case OpBoolOr: "Or";
            case OpAssign: "Assign";
            case OpAssignOp(innerOp):
                // For +=, -=, etc. we still want Assign for now
                // Could expand this to handle compound assignments properly
                "Assign";
            default: "Add";
        }
    }
}

#end
