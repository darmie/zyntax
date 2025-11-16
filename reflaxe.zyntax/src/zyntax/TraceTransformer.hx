package zyntax;

#if (macro || zyntax_runtime)

import haxe.macro.Context;
import haxe.macro.Expr;
import haxe.macro.Type;
import reflaxe.preprocessors.BasePreprocessor;
import reflaxe.data.ClassFuncData;
import reflaxe.BaseCompiler;

using reflaxe.helpers.TypedExprHelper;

/**
 * Transforms Haxe trace() calls into $String$println() calls.
 * 
 * This allows standard Haxe trace() to work with Zyntax runtime.
 */
class TraceTransformer extends BasePreprocessor {

    public function new() {}

    public function process(data: ClassFuncData, compiler: BaseCompiler): Void {
        if (data.expr == null) return;

        var transformed = transformExpr(data.expr);
        data.setExpr(transformed);
    }

    function transformExpr(expr: TypedExpr): TypedExpr {
        return switch(expr.expr) {
            case TCall(callee, args):
                // Check if this is a call to haxe.Log.trace
                var isTrace = switch(callee.expr) {
                    case TField(_, fa):
                        switch(fa) {
                            case FStatic(_.get() => cl, _.get() => cf):
                                cl.name == "Log" && cf.name == "trace";
                            default:
                                false;
                        }
                    default:
                        false;
                };

                if (isTrace && args.length >= 1) {
                    // Transform to type-appropriate println call
                    // haxe.Log.trace passes the value and position info
                    // We only care about the first argument (the value to print)
                    var value = transformExpr(args[0]);

                    // Determine which println function to use based on type
                    var printlnInfo = getPrintlnForType(value.t);

                    var tvar: TVar = {
                        id: -1,
                        name: printlnInfo.name,
                        t: printlnInfo.funcType,
                        capture: false,
                        extra: null,
                        meta: null,
                        isStatic: false
                    };

                    return {
                        expr: TCall(
                            {
                                expr: TLocal(tvar),
                                t: printlnInfo.funcType,
                                pos: expr.pos
                            },
                            [value]
                        ),
                        t: Context.getType("Void"),
                        pos: expr.pos
                    };
                } else {
                    // Not a trace call, process children
                    {
                        expr: TCall(transformExpr(callee), args.map(a -> transformExpr(a))),
                        t: expr.t,
                        pos: expr.pos
                    };
                }

            case TBlock(exprs):
                {
                    expr: TBlock(exprs.map(e -> transformExpr(e))),
                    t: expr.t,
                    pos: expr.pos
                };

            case TIf(econd, eif, eelse):
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
                {
                    expr: TVar(v, init != null ? transformExpr(init) : null),
                    t: expr.t,
                    pos: expr.pos
                };

            case TBinop(op, e1, e2):
                {
                    expr: TBinop(op, transformExpr(e1), transformExpr(e2)),
                    t: expr.t,
                    pos: expr.pos
                };

            default:
                expr;
        }
    }

    /**
     * Get the appropriate println function for a given type
     */
    function getPrintlnForType(t: Type): {name: String, funcType: Type} {
        return switch(t) {
            // String type
            case TInst(_.get() => cl, _) if (cl.name == "String" && cl.pack.length == 0):
                {
                    name: "$String$println",
                    funcType: TFun([{name: "str", opt: false, t: t}], Context.getType("Void"))
                };

            // Int type
            case TAbstract(_.get() => ab, _) if (ab.name == "Int"):
                {
                    name: "$Int$println",
                    funcType: TFun([{name: "value", opt: false, t: t}], Context.getType("Void"))
                };

            // Float type
            case TAbstract(_.get() => ab, _) if (ab.name == "Float"):
                {
                    name: "$Float$println",
                    funcType: TFun([{name: "value", opt: false, t: t}], Context.getType("Void"))
                };

            // Bool type
            case TAbstract(_.get() => ab, _) if (ab.name == "Bool"):
                {
                    name: "$Bool$println",
                    funcType: TFun([{name: "value", opt: false, t: t}], Context.getType("Void"))
                };

            // Default: convert to string (will fail at runtime if unsupported)
            default:
                {
                    name: "$String$println",
                    funcType: TFun([{name: "str", opt: false, t: Context.getType("String")}], Context.getType("Void"))
                };
        };
    }
}

#end
