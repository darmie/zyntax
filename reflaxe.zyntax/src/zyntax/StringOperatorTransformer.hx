package zyntax;

#if (macro || zyntax_runtime)

import haxe.macro.Context;
import haxe.macro.Expr;
import haxe.macro.Type;
import reflaxe.preprocessors.BasePreprocessor;
import reflaxe.data.ClassFuncData;
import reflaxe.BaseCompiler;

using reflaxe.helpers.SyntaxHelper;
using reflaxe.helpers.TypedExprHelper;
using reflaxe.helpers.TypeHelper;

/**
 * Transforms Haxe string concatenation operators into explicit runtime calls.
 *
 * This preprocessor detects binary `+` operations on String types and converts them
 * to calls to the `$String$concat` runtime function.
 *
 * Before:  str1 + str2
 * After:   $String$concat(str1, str2)
 *
 * This keeps the core Zyntax compiler language-agnostic while allowing
 * Haxe-specific operator handling at the frontend level.
 */
class StringOperatorTransformer extends BasePreprocessor {

    static var transformerRan: Bool = false;

    public function new() {}

    public function process(data: ClassFuncData, compiler: BaseCompiler): Void {
        if (data.expr == null) return;

        // Walk the expression tree and transform string concatenations
        var transformed = transformExpr(data.expr);
        data.setExpr(transformed);
    }

    /**
     * Recursively walk the expression tree and transform string concatenations
     */
    function transformExpr(expr: TypedExpr): TypedExpr {
        return switch(expr.expr) {
            case TBinop(OpAdd, e1, e2):
                // Check if the result type is String (this handles String + Int, etc.)
                if (isStringType(expr.t)) {
                    // Debug: check types
                    if (!isStringType(e1.t) || !isStringType(e2.t)) {
                        Context.warning("Concat with non-string: e1=" + Std.string(e1.t) + ", e2=" + Std.string(e2.t), expr.pos);
                    }

                    // This is string concatenation - convert non-string operands to strings FIRST
                    var left = e1;
                    var right = e2;

                    // Convert operands to strings if needed (before transforming)
                    if (!isStringType(e1.t)) {
                        left = convertToString(e1);
                    }
                    if (!isStringType(e2.t)) {
                        right = convertToString(e2);
                    }

                    // Now transform the (possibly converted) expressions
                    left = transformExpr(left);
                    right = transformExpr(right);

                    // Transform to runtime call
                    transformToRuntimeCall(expr, left, right);
                } else {
                    // Not a string concatenation, but still process children
                    {
                        expr: TBinop(OpAdd, transformExpr(e1), transformExpr(e2)),
                        t: expr.t,
                        pos: expr.pos
                    };
                }

            case TBlock(exprs):
                // Process block expressions
                {
                    expr: TBlock(exprs.map(e -> transformExpr(e))),
                    t: expr.t,
                    pos: expr.pos
                };

            case TCall(e, params):
                // Process call expressions
                {
                    expr: TCall(transformExpr(e), params.map(p -> transformExpr(p))),
                    t: expr.t,
                    pos: expr.pos
                };

            case TIf(econd, eif, eelse):
                // Process if expressions
                {
                    expr: TIf(
                        transformExpr(econd),
                        transformExpr(eif),
                        eelse != null ? transformExpr(eelse) : null
                    ),
                    t: expr.t,
                    pos: expr.pos
                };

            case TVar(v, init):
                // Process variable declarations
                {
                    expr: TVar(v, init != null ? transformExpr(init) : null),
                    t: expr.t,
                    pos: expr.pos
                };

            default:
                // For other expression types, return as-is
                expr;
        }
    }

    /**
     * Check if a type is String
     */
    function isStringType(t: Type): Bool {
        return switch(t) {
            case TInst(_.get() => cl, _):
                cl.name == "String" && cl.pack.length == 0;
            case TType(_.get() => dt, _):
                // Follow type aliases
                isStringType(dt.type);
            default:
                false;
        }
    }

    /**
     * Get the appropriate toString function name for a type
     * Returns null if no conversion needed or type doesn't have toString
     */
    function getToStringFunction(t: Type): Null<String> {
        return switch(t) {
            case TAbstract(_.get() => ab, _):
                switch(ab.name) {
                    case "Int": "$Int$toString";
                    case "Float": "$Float$toString";
                    case "Bool": "$Bool$toString";
                    default:
                        // Check if the abstract has a toString method
                        if (hasToStringMethod(t)) {
                            // Could call the instance toString, but for now return null
                            null;
                        } else {
                            null;
                        }
                }
            case TInst(_.get() => cl, _):
                // Check if class has toString method
                if (hasToStringMethod(t)) {
                    // For classes with toString, we could generate a call
                    // For now, return null (let Haxe handle it)
                    null;
                } else {
                    null;
                }
            default:
                null;
        };
    }

    /**
     * Check if a type has a toString() method using macro reflection
     */
    function hasToStringMethod(t: Type): Bool {
        try {
            // Follow type to get the actual type (unwrap typedefs, etc.)
            var followedType = Context.follow(t);

            switch(followedType) {
                case TInst(_.get() => cl, _):
                    // Check class fields for toString
                    for (field in cl.fields.get()) {
                        if (field.name == "toString" && field.kind.match(FMethod(_))) {
                            return true;
                        }
                    }
                    // Check static fields
                    for (field in cl.statics.get()) {
                        if (field.name == "toString" && field.kind.match(FMethod(_))) {
                            return true;
                        }
                    }
                case TAbstract(_.get() => ab, _):
                    // Check if abstract implements toString
                    if (ab.impl != null) {
                        for (field in ab.impl.get().statics.get()) {
                            if (field.name == "toString" && field.kind.match(FMethod(_))) {
                                return true;
                            }
                        }
                    }
                default:
            }
        } catch(e: Dynamic) {
            // If type inspection fails, assume no toString
        }
        return false;
    }

    /**
     * Convert a value to String by calling the appropriate toString function
     */
    function convertToString(expr: TypedExpr): TypedExpr {
        // Check if already a string
        if (isStringType(expr.t)) {
            return expr;
        }

        // Determine which toString function to call based on the type
        var toStringFunc = getToStringFunction(expr.t);
        if (toStringFunc == null) {
            // No conversion available
            Context.warning("No toString for type: " + Std.string(expr.t), expr.pos);
            return expr;
        }

        // Create function type: (T) -> String
        var stringType = Context.getType("String");
        var funcType = TFun([
            {name: "value", opt: false, t: expr.t}
        ], stringType);

        var tvar: TVar = {
            id: -1,
            name: toStringFunc,
            t: funcType,
            capture: false,
            extra: null,
            meta: null,
            isStatic: false
        };

        // Return call to toString function
        return {
            expr: TCall(
                {
                    expr: TLocal(tvar),
                    t: funcType,
                    pos: expr.pos
                },
                [expr]
            ),
            t: stringType,
            pos: expr.pos
        };
    }

    /**
     * Transform string concatenation to runtime call
     */
    function transformToRuntimeCall(originalExpr: TypedExpr, left: TypedExpr, right: TypedExpr): TypedExpr {
        // Create reference to runtime function $String$concat
        // We use a special naming convention that the Zyntax backend recognizes

        var runtimeFuncName = "$String$concat";

        // Create a call expression
        // The TypedExpr for a call to: $String$concat(left, right)
        return {
            expr: TCall(
                // Callee: reference to $String$concat
                createRuntimeFunctionRef(runtimeFuncName, originalExpr.pos),
                // Arguments
                [left, right]
            ),
            t: originalExpr.t,  // Result type is String
            pos: originalExpr.pos
        };
    }

    /**
     * Create a typed expression referencing a runtime function by name
     */
    function createRuntimeFunctionRef(funcName: String, pos: Position): TypedExpr {
        // Create a local variable reference that will resolve to the runtime function
        // The backend will recognize the $ prefix and resolve it appropriately

        // Create function type: (String, String) -> String
        var stringType = Context.getType("String");
        var funcType = TFun([
            {name: "str1", opt: false, t: stringType},
            {name: "str2", opt: false, t: stringType}
        ], stringType);

        // Create a TLocal with a synthetic TVar
        // This will be compiled to AST.Expr.Variable(funcName) which is what we want
        var tvar: TVar = {
            id: -1,  // Synthetic ID
            name: funcName,
            t: funcType,
            capture: false,
            extra: null,
            meta: null,
            isStatic: false
        };

        return {
            expr: TLocal(tvar),
            t: funcType,
            pos: pos
        };
    }
}

#end
