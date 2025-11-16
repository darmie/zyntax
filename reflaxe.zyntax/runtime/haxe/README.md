# Haxe Runtime Abstracts for Zyntax

This directory contains Haxe abstract types that provide operator overloading and automatic method mapping for Zyntax runtime integration.

## Overview

Instead of trying to map Haxe's built-in `String` and `Array` operations in the compiler backend, we use Haxe abstracts with operator overloading to intercept operations at the Haxe level before TypedAST generation.

## Files

- **`ZyntaxString.hx`** - Abstract wrapper for String with operator overloading
- **`ZyntaxArray.hx`** - Abstract wrapper for Array with operator overloading
- **`ZyntaxRuntime.hx`** - Runtime utilities and extern definitions

## How It Works

### String Concatenation Example

Instead of:
```haxe
var result = "Hello" + " World";  // Becomes complex Binary operation in AST
```

The reflaxe.zyntax compiler can inject type replacements so:
```haxe
var result = str1 + str2;  // Uses ZyntaxString abstract
// Compiles to: __zyntax__("$String$concat", str1, str2)
```

### Array Operations Example

Instead of:
```haxe
var arr = [1, 2, 3];
arr.push(4);  // May need special pointer handling
```

With ZyntaxArray:
```haxe
var arr = new ZyntaxArray<Int>();
arr.push(4);  // Automatically handles pointer reallocation
// Compiles to: arr = __zyntax__("$Array$push", arr, 4)
```

## Integration Approaches

### Approach 1: Build Macro Injection (Recommended)

The reflaxe.zyntax compiler automatically injects a build macro before Haxe compilation:

```bash
# Automatically added by reflaxe.zyntax compiler:
haxe --macro zyntax.runtime.TypeReplacer.initialize() \
     -cp reflaxe.zyntax/runtime/haxe \
     -main Main
```

The macro (`TypeReplacer.hx`) uses Haxe's compiler API to:

1. **Transform all type references**:
   - `String` → `ZyntaxString`
   - `Array<T>` → `ZyntaxArray<T>`

2. **Works at AST level** before type checking

3. **Result**: User writes normal Haxe, gets Zyntax runtime calls automatically!

**User Code** (pure Haxe):
```haxe
class Main {
    static function main() {
        var msg = "Hello" + " World";  // Auto-transforms to ZyntaxString
        var arr = [1, 2, 3];           // Auto-transforms to ZyntaxArray<Int>
        arr.push(4);
    }
}
```

**See [INJECTION_STRATEGY.md](INJECTION_STRATEGY.md) for implementation details.**

### Approach 2: Explicit Import

Users manually import and use the abstracts:

```haxe
import zyntax.runtime.*;

class Main {
    static function main() {
        var str1: ZyntaxString = "Hello";
        var str2: ZyntaxString = " World";
        var result = str1 + str2;  // Calls $String$concat

        var arr = new ZyntaxArray<Int>();
        arr.push(42);  // Calls $Array$push with pointer update
    }
}
```

### Approach 3: Build Macro

Use a build macro to automatically replace types:

```bash
haxe -lib reflaxe.zyntax \
     --macro zyntax.runtime.ZyntaxRuntime.initialize() \
     -main Main \
     --interp
```

## Implementation in reflaxe.zyntax Compiler

The reflaxe.zyntax compiler should:

1. **Detect `__zyntax__` calls** during TypedAST processing:
   ```haxe
   untyped __zyntax__("$String$concat", a, b)
   ```

2. **Lower to HIR runtime call**:
   ```rust
   Instruction::Call {
       func: "$String$concat",
       args: vec![a, b],
       dest: result
   }
   ```

3. **Handle pointer updates** for functions marked `returns_self = true`:
   ```haxe
   arr = untyped __zyntax__("$Array$push", arr, elem)
   ```

   Becomes:
   ```rust
   let new_ptr = call("$Array$push", arr, elem);
   arr = new_ptr;  // Update array reference
   ```

## Operator Overloading

The abstracts provide these operator overloads:

### ZyntaxString
- `+` (concat) → `$String$concat`
- `==` (equals) → `$String$equals`
- `!=` (not equals) → `!($ String$equals)`

### ZyntaxArray
- `[]` get (arrayAccess) → `$Array$get`
- `[]` set (arrayAccess) → `$Array$set`

## Method Mapping

All methods are inline and map directly to runtime calls using the method mapping manifest:

| Haxe Method | Runtime Symbol | Mutates | Returns Self |
|-------------|----------------|---------|--------------|
| `String.length` | `$String$length` | No | No |
| `String.charAt()` | `$String$charAt` | No | No |
| `String.toLowerCase()` | `$String$toLowerCase` | No | No |
| `Array.length` | `$Array$length` | No | No |
| `Array.push()` | `$Array$push` | Yes | Yes |
| `Array.pop()` | `$Array$pop` | Yes | No |

## Next Steps

1. **Implement `__zyntax__` call detection** in reflaxe.zyntax compiler
2. **Add type injection** to automatically use these abstracts
3. **Test string concatenation** with the abstract
4. **Document limitations** and edge cases

## Limitations

- Currently only supports `Int` element types for arrays
- String operations assume ASCII (not full Unicode)
- Some Haxe standard library methods not yet implemented
- Abstract may have overhead if not properly inlined

## Example Usage

See `../test/StringConcatTest.hx` for example usage with the abstracts.
