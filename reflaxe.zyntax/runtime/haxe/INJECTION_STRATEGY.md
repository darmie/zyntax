# Build-Time Type Injection Strategy

## Problem

Users should write standard Haxe code:
```haxe
class Main {
    static function main() {
        var str = "Hello" + " World";  // Standard Haxe
        var arr = [1, 2, 3];
        arr.push(4);
    }
}
```

NOT have to import special abstracts:
```haxe
import zyntax.runtime.ZyntaxString;  // ❌ User shouldn't need this
var str: ZyntaxString = "Hello";
```

## Solution: Compiler-Injected Type Replacements

The reflaxe.zyntax compiler should inject type replacements at build time, BEFORE Haxe processes the code.

### Approach 1: Inject Init Macro (Recommended)

The reflaxe.zyntax compiler prepends an initialization macro to the build:

```haxe
// Automatically injected by reflaxe.zyntax compiler
--macro zyntax.runtime.TypeReplacer.initialize()
```

This macro uses Haxe's compiler API to:

1. **Replace String type references** with ZyntaxString
2. **Replace Array<T> type references** with ZyntaxArray<T>
3. **Inject implicit imports** into all modules

### Approach 2: Extra File Injection

The compiler creates a temporary file that's automatically included:

```haxe
// _ZyntaxTypeReplacements.hx (auto-generated and injected)
package;

import zyntax.runtime.ZyntaxString;
import zyntax.runtime.ZyntaxArray;

// Make these available globally
typedef String = ZyntaxString;
typedef Array<T> = ZyntaxArray<T>;
```

Then inject via `--macro include('_ZyntaxTypeReplacements')` or similar.

### Approach 3: AST Transformation

The reflaxe.zyntax compiler processes the TypedAST and transforms it:

```
User writes:     var s = "Hello" + " World";
TypedAST shows:  Binary(OpAdd, "Hello", " World")
Transform to:    Call("$String$concat", ["Hello", " World"])
```

This is what we were trying to do originally, but it's complex.

## Recommended Implementation

### Step 1: Create Build Macro

Create `reflaxe.zyntax/src/zyntax/runtime/TypeReplacer.hx`:

```haxe
package zyntax.runtime;

#if macro
import haxe.macro.Compiler;
import haxe.macro.Context;
import haxe.macro.Expr;

class TypeReplacer {
    public static function initialize() {
        // Add global typedef replacements
        Compiler.define("zyntax_runtime_active");

        // Inject the runtime abstracts
        Compiler.addClassPath("path/to/runtime/haxe");

        // Use a global metadata to intercept all types
        Compiler.addGlobalMetadata("", "@:build(zyntax.runtime.TypeReplacer.buildClass())", true, true, false);
    }

    public static macro function buildClass():Array<Field> {
        var fields = Context.getBuildFields();

        // Transform all String fields to ZyntaxString
        // Transform all Array<T> fields to ZyntaxArray<T>
        for (field in fields) {
            transformFieldType(field);
        }

        return fields;
    }

    static function transformFieldType(field:Field) {
        switch (field.kind) {
            case FVar(t, _) | FProp(_, _, t, _):
                if (t != null) {
                    field.kind = replaceType(field.kind, transformComplexType(t));
                }
            case FFun(f):
                // Transform function arguments and return type
                for (arg in f.args) {
                    if (arg.type != null) {
                        arg.type = transformComplexType(arg.type);
                    }
                }
                if (f.ret != null) {
                    f.ret = transformComplexType(f.ret);
                }
            default:
        }
    }

    static function transformComplexType(t:ComplexType):ComplexType {
        return switch (t) {
            case TPath({ pack: [], name: "String" }):
                // Replace String with ZyntaxString
                TPath({ pack: ["zyntax", "runtime"], name: "ZyntaxString" });

            case TPath({ pack: [], name: "Array", params: params }):
                // Replace Array<T> with ZyntaxArray<T>
                TPath({ pack: ["zyntax", "runtime"], name: "ZyntaxArray", params: params });

            case TPath(p):
                // Check params recursively
                if (p.params != null) {
                    p.params = [for (param in p.params) {
                        switch (param) {
                            case TPType(pt): TPType(transformComplexType(pt));
                            default: param;
                        }
                    }];
                }
                TPath(p);

            default:
                t;
        }
    }
}
#end
```

### Step 2: Modify reflaxe.zyntax Compiler

In the reflaxe.zyntax compiler code, when building the Haxe command:

```rust
// In reflaxe.zyntax/src/compiler.rs or similar

pub fn compile_haxe_to_typed_ast(input: &Path, output: &Path) -> Result<()> {
    let mut cmd = Command::new("haxe");

    // Inject the type replacer macro
    cmd.arg("--macro")
       .arg("zyntax.runtime.TypeReplacer.initialize()");

    // Add runtime classpath
    cmd.arg("-cp")
       .arg("path/to/reflaxe.zyntax/runtime/haxe");

    // Rest of compilation
    cmd.arg("-main").arg("Main");
    cmd.arg("--interp");

    cmd.output()?;
    Ok(())
}
```

### Step 3: User Experience

User writes completely normal Haxe:

```haxe
class Main {
    static function main() {
        var message = "Hello" + " World";  // Automatic ZyntaxString
        trace(message.length);              // Works!

        var nums = [1, 2, 3];              // Automatic ZyntaxArray<Int>
        nums.push(4);                       // Handles pointer reallocation
    }
}
```

The compiler automatically:
1. Injects the type replacement macro
2. Transforms all `String` → `ZyntaxString`
3. Transforms all `Array<T>` → `ZyntaxArray<T>`
4. User code remains pure Haxe!

## Alternative: Simpler AST Post-Processing

Instead of type replacement, just detect extern calls in TypedAST:

1. Keep the abstracts as-is with `@:native` metadata
2. In reflaxe.zyntax TypedAST processor, detect calls to functions with `@:native("$...")` metadata
3. Lower these directly to runtime calls
4. For binary operators like `+`, detect them in TypedAST and map to runtime calls based on operand types

This is cleaner but requires the TypedAST processor to be smart about operator detection.

## Recommendation

Use **Approach 1 (Build Macro)** because:
- ✅ Zero user code changes
- ✅ Type-safe at Haxe level
- ✅ Operator overloading works automatically
- ✅ Abstracts handle pointer reallocation correctly
- ✅ Can be incrementally adopted (start with String, add Array later)

The reflaxe.zyntax compiler just needs to:
1. Inject `--macro zyntax.runtime.TypeReplacer.initialize()`
2. Recognize `@:native` extern calls in TypedAST
3. Lower them to HIR runtime calls
