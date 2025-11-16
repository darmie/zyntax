package zyntax.runtime;

#if macro
import haxe.macro.Compiler;
import haxe.macro.Context;
import haxe.macro.Expr;
import haxe.macro.Type;

using haxe.macro.Tools;

/**
 * Build macro that transforms standard Haxe types to Zyntax runtime types.
 *
 * This allows users to write normal Haxe code while getting Zyntax runtime integration.
 *
 * Usage:
 *   haxe --macro zyntax.runtime.TypeReplacer.initialize()
 */
class TypeReplacer {

    /**
     * Initialize the type replacement system.
     * Called automatically by the reflaxe.zyntax compiler.
     */
    public static function initialize() {
        // Add global metadata to intercept all class builds
        Compiler.addGlobalMetadata(
            "",  // Empty string means all classes
            "@:build(zyntax.runtime.TypeReplacer.buildClass())",
            true,   // recursive
            true,   // to types
            false   // to fields
        );
    }

    /**
     * Build macro applied to all classes.
     * Transforms String and Array<T> types to Zyntax runtime types.
     */
    public static macro function buildClass():Array<Field> {
        var fields = Context.getBuildFields();
        var localClass = Context.getLocalClass();

        if (localClass == null) return fields;

        var className = localClass.get().name;

        // Don't transform the runtime types themselves
        if (isRuntimeClass(className)) {
            return fields;
        }

        // Transform all fields
        for (field in fields) {
            transformField(field);
        }

        return fields;
    }

    /**
     * Check if this is a runtime class that shouldn't be transformed
     */
    static function isRuntimeClass(name:String):Bool {
        return name == "ZyntaxString"
            || name == "ZyntaxArray"
            || name == "ZyntaxStringExtern"
            || name == "ZyntaxArrayExtern"
            || name == "Zyntax"
            || name == "ZyntaxRuntime";
    }

    /**
     * Transform a single field's type
     */
    static function transformField(field:Field):Void {
        switch (field.kind) {
            case FVar(t, e):
                if (t != null) {
                    var newType = transformComplexType(t);
                    if (newType != t) {
                        field.kind = FVar(newType, e);
                    }
                }

            case FProp(get, set, t, e):
                if (t != null) {
                    var newType = transformComplexType(t);
                    if (newType != t) {
                        field.kind = FProp(get, set, newType, e);
                    }
                }

            case FFun(f):
                // Transform function arguments
                for (arg in f.args) {
                    if (arg.type != null) {
                        arg.type = transformComplexType(arg.type);
                    }
                }

                // Transform return type
                if (f.ret != null) {
                    f.ret = transformComplexType(f.ret);
                }

            default:
        }
    }

    /**
     * Transform a ComplexType, replacing String and Array with Zyntax versions
     */
    static function transformComplexType(t:ComplexType):ComplexType {
        return switch (t) {
            // Replace String with ZyntaxString
            case TPath({ pack: [], name: "String", params: null | [] }):
                // Try to get the type and convert to ComplexType
                try {
                    var zyntaxStringType = Context.getType("zyntax.runtime.ZyntaxString");
                    zyntaxStringType.toComplexType();
                } catch (e:Dynamic) {
                    // If ZyntaxString isn't available yet, keep original
                    Context.warning("ZyntaxString not available yet, keeping String", Context.currentPos());
                    t;
                }

            // Replace Array<T> with ZyntaxArray<T>
            case TPath({ pack: [], name: "Array", params: params }):
                // For now, just keep Array - we'll handle this differently
                t;

            // Handle other TPath cases (check nested types)
            case TPath(p):
                if (p.params != null && p.params.length > 0) {
                    var newParams = transformTypeParams(p.params);
                    TPath({
                        pack: p.pack,
                        name: p.name,
                        params: newParams,
                        sub: p.sub
                    });
                } else {
                    t;
                }

            // Function types
            case TFunction(args, ret):
                var newArgs = [for (arg in args) transformComplexType(arg)];
                var newRet = transformComplexType(ret);
                TFunction(newArgs, newRet);

            // Anonymous object types
            case TAnonymous(fields):
                for (field in fields) {
                    transformField(field);
                }
                t;

            // Parent/extension types
            case TExtend(p, fields):
                for (field in fields) {
                    transformField(field);
                }
                t;

            // Optional type
            case TOptional(inner):
                TOptional(transformComplexType(inner));

            default:
                t;
        }
    }

    /**
     * Transform type parameters recursively
     */
    static function transformTypeParams(params:Array<TypeParam>):Array<TypeParam> {
        return [for (param in params) {
            switch (param) {
                case TPType(ct):
                    TPType(transformComplexType(ct));
                case TPExpr(e):
                    param; // Don't transform expressions
            }
        }];
    }
}
#end
