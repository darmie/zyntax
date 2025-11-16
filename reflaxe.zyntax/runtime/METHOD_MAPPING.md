# Dynamic Method Mapping System

## Update: Haxe Abstract Approach (Recommended)

**NEW**: Instead of mapping methods in the compiler backend, we now use **Haxe abstracts with operator overloading** to intercept operations at the source level.

### Key Files
- **[haxe/ZyntaxString.hx](haxe/ZyntaxString.hx)** - Abstract with `+` operator for string concatenation
- **[haxe/ZyntaxArray.hx](haxe/ZyntaxArray.hx)** - Abstract with array operations and pointer handling
- **[test/StringConcatTest.hx](../test/StringConcatTest.hx)** - Example usage

### How It Works

Instead of trying to map Haxe's binary `+` operator in the backend:
```haxe
var result = str1 + str2;  // Hard to detect and map in HIR lowering
```

We use an abstract with `@:op` metadata:
```haxe
import zyntax.runtime.ZyntaxString;

var str1: ZyntaxString = "Hello";
var str2: ZyntaxString = " World";
var result = str1 + str2;  // Compiles to: ZyntaxStringExtern.concat(str1, str2)
```

The abstract delegates to an `extern` class with `@:native` metadata:
```haxe
@:native("$String")
extern class ZyntaxStringExtern {
    @:native("$String$concat")
    public static function concat(a: Dynamic, b: Dynamic): Dynamic;
}
```

The reflaxe.zyntax compiler recognizes these extern calls and lowers them directly to the corresponding runtime symbols (e.g., `$String$concat`).

### Benefits
1. **Clean separation** - String operations handled at Haxe level, not in compiler backend
2. **Type safety** - Haxe type checker validates operations
3. **Zero backend complexity** - Compiler just needs to recognize `@:native` extern calls
4. **Explicit** - Developers can see exactly what runtime calls are being made

---

## Overview (Original System)

The Zyntax runtime uses a **dynamic method mapping system** that automatically discovers and registers the mappings between Haxe methods/properties and runtime function calls. This eliminates the need for manual configuration in the compiler.

## Architecture

```
┌────────────────────────────────────────────────────────┐
│  Runtime Functions (Rust)                              │
│  ┌──────────────────────────────────────────┐         │
│  │  #[runtime_method(                        │         │
│  │    symbol = "$Array$length",              │         │
│  │    haxe_type = "Array",                   │         │
│  │    haxe_property = "length"               │         │
│  │  )]                                        │         │
│  │  pub extern "C" fn Array_length(...) {...}│         │
│  └──────────────────────────────────────────┘         │
└────────────────────────────────────────────────────────┘
                          ↓
                  inventory::collect!()
                          ↓
┌────────────────────────────────────────────────────────┐
│  Method Mapping Registry (Compile-Time)                │
│  ┌──────────────────────────────────────────┐         │
│  │  MethodMapping {                          │         │
│  │    symbol: "$Array$length",               │         │
│  │    haxe_type: "Array",                    │         │
│  │    haxe_name: "length",                   │         │
│  │    is_property: true,                     │         │
│  │    mutates: false,                        │         │
│  │    returns_self: false                    │         │
│  │  }                                         │         │
│  └──────────────────────────────────────────┘         │
└────────────────────────────────────────────────────────┘
                          ↓
          get_method_mappings() or JSON manifest
                          ↓
┌────────────────────────────────────────────────────────┐
│  Haxe Compiler (reflaxe.zyntax)                        │
│  ┌──────────────────────────────────────────┐         │
│  │  arr.length                               │         │
│  │    ↓ (lookup: Array.length → property)   │         │
│  │  call $Array$length(arr)                  │         │
│  └──────────────────────────────────────────┘         │
└────────────────────────────────────────────────────────┘
```

## Macro Usage

### `#[runtime_method]` - Methods with Metadata

Use this macro to export a function AND register its method mapping:

```rust
#[runtime_method(
    symbol = "$Array$push",
    haxe_type = "Array",
    haxe_method = "push",
    mutates = true,
    returns_self = true
)]
pub extern "C" fn Array_push(arr: *mut i32, elem: i32) -> *mut i32 {
    // Implementation...
}
```

**Parameters**:
- `symbol`: Runtime symbol name (e.g., `"$Array$push"`)
- `haxe_type`: Haxe type this belongs to (e.g., `"Array"`, `"String"`)
- `haxe_method`: Method name (for methods)
- `haxe_property`: Property name (for properties)
- `mutates` (optional, default=false): Whether this mutates the receiver
- `returns_self` (optional, default=false): Whether this returns the receiver (for realloc)

### `#[runtime_export]` - Simple Export

Use this for functions that don't need method mapping (e.g., standalone functions):

```rust
#[runtime_export("$Array$fromElements")]
pub extern "C" fn Array_fromElements(elems: *const i32, count: i32) -> *mut i32 {
    // Implementation...
}
```

## Example: Array Implementation

### Property (`.length`)

```rust
#[runtime_method(
    symbol = "$Array$length",
    haxe_type = "Array",
    haxe_property = "length"
)]
pub extern "C" fn Array_length(array_ptr: *const i32) -> i32 {
    if array_ptr.is_null() {
        return 0;
    }
    unsafe { *array_ptr.offset(1) }
}
```

**Haxe Usage**:
```haxe
var arr = [1, 2, 3];
var len = arr.length;  // Compiled to: call $Array$length(arr)
```

### Method (`.push()`)

```rust
#[runtime_method(
    symbol = "$Array$push",
    haxe_type = "Array",
    haxe_method = "push",
    mutates = true,
    returns_self = true
)]
pub extern "C" fn Array_push(arr: *mut i32, elem: i32) -> *mut i32 {
    // May reallocate, returns new pointer
    // ...
}
```

**Haxe Usage**:
```haxe
var arr = [1, 2];
arr.push(3);  // Compiled to: arr = call $Array$push(arr, 3)
```

**Important**: Note `returns_self = true` tells the compiler to update the array reference since `push` may reallocate.

### Method without Mutation (`.indexOf()`)

```rust
#[runtime_method(
    symbol = "$Array$indexOf",
    haxe_type = "Array",
    haxe_method = "indexOf"
)]
pub extern "C" fn Array_indexOf(arr: *const i32, elem: i32) -> i32 {
    // Returns index, doesn't modify array
    // ...
}
```

**Haxe Usage**:
```haxe
var arr = [10, 20, 30];
var idx = arr.indexOf(20);  // Compiled to: call $Array$indexOf(arr, 20)
```

## Example: String Implementation

### Property

```rust
#[runtime_method(
    symbol = "$String$length",
    haxe_type = "String",
    haxe_property = "length"
)]
pub extern "C" fn String_length(str_ptr: *const i32) -> i32 {
    if str_ptr.is_null() {
        return 0;
    }
    unsafe { *str_ptr }
}
```

### Method

```rust
#[runtime_method(
    symbol = "$String$charAt",
    haxe_type = "String",
    haxe_method = "charAt"
)]
pub extern "C" fn String_charAt(str_ptr: *const i32, index: i32) -> u8 {
    // ...
}
```

**Haxe Usage**:
```haxe
var str = "Hello";
var ch = str.charAt(0);  // Compiled to: call $String$charAt(str, 0)
```

### Operator Overload (String Concatenation)

```rust
#[runtime_method(
    symbol = "$String$concat",
    haxe_type = "String",
    haxe_method = "+",  // Special: operator name
)]
pub extern "C" fn String_concat(str1: *const i32, str2: *const i32) -> *mut i32 {
    // ...
}
```

**Haxe Usage**:
```haxe
var str = "Hello" + " " + "World";
// Compiled to:
// tmp1 = call $String$concat("Hello", " ")
// tmp2 = call $String$concat(tmp1, "World")
```

## Generating Method Mapping Manifest

For compiler integration, generate a JSON manifest of all mappings:

```bash
cd reflaxe.zyntax/runtime
cargo run --example generate_manifest --features manifest > ../method_mappings.json
```

**Output** (`method_mappings.json`):
```json
{
  "version": "1.0.0",
  "mappings": [
    {
      "symbol": "$Array$length",
      "haxe_type": "Array",
      "haxe_name": "length",
      "kind": "property",
      "mutates": false,
      "returns_self": false
    },
    {
      "symbol": "$Array$push",
      "haxe_type": "Array",
      "haxe_name": "push",
      "kind": "method",
      "mutates": true,
      "returns_self": true
    },
    ...
  ]
}
```

## Compiler Integration

The reflaxe.zyntax compiler can use this manifest to automatically map Haxe code:

### 1. Load Manifest at Compile Time

```haxe
// In reflaxe.zyntax compiler initialization
var manifest = sys.io.File.getContent("method_mappings.json");
var mappings = haxe.Json.parse(manifest);

// Build lookup tables
for (mapping in mappings.mappings) {
    registerMapping(mapping);
}
```

### 2. Use Mappings During Compilation

```haxe
// When compiling property access
function compilePropertyAccess(obj: TypedExpr, propName: String): Expr {
    var objType = getTypeName(obj.t);  // e.g., "Array"

    // Lookup mapping
    var mapping = findMapping(objType, propName, "property");

    if (mapping != null) {
        // Generate runtime call
        return {
            expr: TCall(
                {expr: TIdent(mapping.symbol)},
                [compileExpr(obj)]
            )
        };
    }

    // Fallback to default behavior
    return compileFieldAccess(obj, propName);
}

// When compiling method call
function compileMethodCall(obj: TypedExpr, methodName: String, args: Array<TypedExpr>): Expr {
    var objType = getTypeName(obj.t);

    var mapping = findMapping(objType, methodName, "method");

    if (mapping != null) {
        var callArgs = [compileExpr(obj)].concat(args.map(compileExpr));

        var call = {
            expr: TCall(
                {expr: TIdent(mapping.symbol)},
                callArgs
            )
        };

        // Handle reallocation
        if (mapping.returns_self) {
            // Need to update reference: obj = call(...)
            return {
                expr: TAssign(obj, call)
            };
        }

        return call;
    }

    return compileRegularMethodCall(obj, methodName, args);
}
```

## Benefits

1. **Zero Manual Configuration**: Add a runtime function, compiler automatically discovers it
2. **Type-Safe**: Metadata validated at compile time
3. **Extensible**: Adding new types/methods requires zero compiler changes
4. **Maintainable**: All mapping logic lives with the runtime implementation
5. **Self-Documenting**: Metadata serves as inline documentation
6. **Versioned**: Manifest includes version for compatibility checking

## Adding a New Runtime Type

**Step 1**: Implement runtime functions with metadata:

```rust
#[runtime_method(
    symbol = "$Map$new",
    haxe_type = "Map",
    haxe_method = "new"
)]
pub extern "C" fn Map_new() -> *mut u8 { ... }

#[runtime_method(
    symbol = "$Map$set",
    haxe_type = "Map",
    haxe_method = "set",
    mutates = true
)]
pub extern "C" fn Map_set(map: *mut u8, key: *const i32, value: *const i32) { ... }

#[runtime_method(
    symbol = "$Map$get",
    haxe_type = "Map",
    haxe_method = "get"
)]
pub extern "C" fn Map_get(map: *const u8, key: *const i32) -> *const i32 { ... }
```

**Step 2**: Regenerate manifest:

```bash
cargo run --example generate_manifest --features manifest > ../method_mappings.json
```

**Step 3**: Done! The compiler now supports Map automatically.

## Metadata Fields Reference

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `symbol` | string | Yes | Runtime symbol name (e.g., "$Array$push") |
| `haxe_type` | string | Yes | Haxe type (e.g., "Array", "String", "Map") |
| `haxe_method` | string | Yes* | Method name if this is a method |
| `haxe_property` | string | Yes* | Property name if this is a property |
| `mutates` | bool | No (false) | Whether calling this mutates the receiver |
| `returns_self` | bool | No (false) | Whether this returns the receiver (for realloc) |

*: Exactly one of `haxe_method` or `haxe_property` must be specified.

## Best Practices

1. **Properties vs Methods**:
   - Use `haxe_property` for zero-arg accessors (e.g., `.length`)
   - Use `haxe_method` for everything else

2. **Mutations**:
   - Set `mutates = true` if the function modifies its receiver
   - Set `returns_self = true` if it may reallocate (compiler updates reference)

3. **Symbol Naming**:
   - Convention: `$Type$method`
   - Examples: `$Array$push`, `$String$concat`, `$Map$get`

4. **Operators**:
   - Use operator symbol as method name: `"+", "-", "*", "==", etc.`
   - Example: `haxe_method = "+"`for concatenation

## Future Enhancements

1. **Generic Type Parameters**: Support for `Array<T>`, `Map<K,V>`
2. **Overloading**: Multiple signatures for same method
3. **Static Methods**: Class-level methods
4. **Constructors**: Special handling for `new Type()`
5. **Extension Methods**: Add methods to existing types

## Complete Example

See the Haxe runtime implementation for full examples:
- [src/array.rs](src/array.rs) - Array methods and properties
- [src/string.rs](src/string.rs) - String methods and properties

The dynamic method mapping system makes adding new runtime functionality effortless!
