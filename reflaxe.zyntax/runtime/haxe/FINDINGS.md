# Findings: Build-Time Type Injection Implementation

## Summary

After implementing and testing the build-time type replacement macro, we discovered a **fundamental limitation** in Haxe's macro system that prevents automatic type replacement at build time.

## The Problem

**Goal**: Transform `String` → `ZyntaxString` and `Array<T>` → `ZyntaxArray<T>` automatically so users can write standard Haxe code.

**Limitation**: Haxe build macros (`@:build`) run **during type checking**, creating a circular dependency:

1. User code references `String`
2. TypeReplacer macro tries to replace `String` with `ZyntaxString`
3. To create `ZyntaxString` type reference, Haxe needs to type-check `ZyntaxString.hx`
4. But `ZyntaxString.hx` also triggers the `@:build` macro (global metadata)
5. **Circular dependency**: Can't reference `ZyntaxString` type until it's checked, but checking it triggers the macro that needs the type!

## What We Tried

1. ✅ **Build macro detection** - Successfully detects String and Array types
2. ❌ **TPath creation** - `ComplexType.TPath({pack: ["zyntax", "runtime"], name: "ZyntaxString"})` fails with "Type not found"
3. ❌ **Macro type hints** - `macro : zyntax.runtime.ZyntaxString` also requires type to exist
4. ❌ **Force include** - `--macro include('zyntax.runtime.ZyntaxString')` doesn't help - still circular
5. ❌ **Context.getType + toComplexType** - Type not available during build macro execution
6. ❌ **Explicit imports** - Creates circular dependency in TypeReplacer itself

## Test Results

```bash
$ haxe build_auto_string.hxml
Warning: Found String type - would transform to ZyntaxString  # ✅ Detection works!
Warning: Found Array type - would transform to ZyntaxArray   # ✅ Detection works!
Error: Type not found : zyntax.runtime.ZyntaxString          # ❌ Transformation fails
```

The macro successfully **detects** types that need transformation, but **cannot create references** to the replacement types.

## Root Cause

Haxe's type system is **strictly ordered** during compilation:
- Build macros run during **typing phase**
- Types must be fully resolved before they can be referenced
- Can't reference a type that's currently being typed (circular dependency)

## Recommended Solutions

### Option 1: TypedAST Transformation (Recommended for MVP)

**Instead of Haxe-level transformation, transform in the reflaxe.zyntax compiler:**

```rust
// In reflaxe.zyntax TypedAST processor:

match typed_ast {
    // Detect binary string concatenation
    TBinop(OpAdd, left, right) if both_are_strings(left, right) => {
        // Lower to runtime call
        HIR::Call {
            func: "$String$concat",
            args: vec![lower_expr(left), lower_expr(right)]
        }
    }

    // Detect array method calls
    TCall(TField(array, "push"), [elem]) => {
        HIR::Call {
            func: "$Array$push",
            args: vec![lower_expr(array), lower_expr(elem)]
        }
    }
}
```

**Benefits**:
- ✅ No circular dependencies
- ✅ Full access to type information
- ✅ Users write standard Haxe
- ✅ Clean separation of concerns

**Drawbacks**:
- Need to detect all operations in TypedAST (but this is manageable)
- Slightly more backend complexity

### Option 2: Explicit Imports (Simplest for MVP)

**Users explicitly import and use the abstracts:**

```haxe
import zyntax.runtime.ZyntaxString;

class Main {
    static function main() {
        var msg: ZyntaxString = "Hello";
        var msg2: ZyntaxString = " World";
        var result = msg + msg2;  // Uses ZyntaxString.concat
    }
}
```

**Benefits**:
- ✅ Works immediately with existing code
- ✅ No circular dependencies
- ✅ Explicit and clear

**Drawbacks**:
- ❌ Users need to change their code
- ❌ Not seamless

### Option 3: Initialization Macro (Future Enhancement)

**Use `Context.onAfterTyping()` or `Context.onGenerate()` to transform AFTER all types are resolved:**

```haxe
Context.onGenerate(function(types) {
    for (type in types) {
        // Transform at the Type level (not ComplexType)
        // This runs AFTER type checking is complete
    }
});
```

**Benefits**:
- ✅ All types available
- ✅ Can transform existing code

**Drawbacks**:
- ❌ More complex - transforming Type is harder than ComplexType
- ❌ May require AST rewriting

## Recommendation for Zyntax MVP

**Use Option 1 (TypedAST Transformation) as the primary approach:**

1. Keep the Haxe abstracts (`ZyntaxString`, `ZyntaxArray`) for their operator overloading
2. **Don't use build macros** for automatic transformation
3. Instead, detect and transform operations in the **reflaxe.zyntax TypedAST processor**:
   - Binary `+` on strings → `$String$concat`
   - `.length` property access → `$String$length` or `$Array$length`
   - `.push()` method calls → `$Array$push`
   - etc.

4. Use the **method mapping manifest** (already generated) to guide transformations

This approach:
- Keeps users writing standard Haxe
- Avoids Haxe macro limitations
- Gives reflaxe.zyntax full control over lowering
- Matches the existing architecture (TypedAST → HIR → Cranelift)

## Files Created

- ✅ `TypeReplacer.hx` - Build macro (detection works, transformation blocked by Haxe limitations)
- ✅ `ZyntaxString.hx` - Abstract with operator overloading
- ✅ `ZyntaxArray.hx` - Abstract with operator overloading
- ✅ `method_mappings.json` - Manifest of all runtime functions
- ✅ This findings document

## Next Steps

1. **Implement TypedAST transformation** in reflaxe.zyntax compiler
2. **Detect binary operators** (`+` for strings)
3. **Detect method calls** (`.push()`, `.charAt()`, etc.)
4. **Use method mapping manifest** to map operations → runtime symbols
5. **Test full pipeline** with string concatenation

This will achieve the same goal (users write standard Haxe) without fighting Haxe's macro system.
