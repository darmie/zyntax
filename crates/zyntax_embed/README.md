# zyntax_embed

Rust SDK for embedding Zyntax as a JIT runtime with bidirectional value interop.

## License

Apache-2.0

## Overview

`zyntax_embed` provides a high-level Rust API for embedding the Zyntax compiler as a JIT runtime. It supports:

- **Language Grammar Interface**: Parse source code using any `.zyn` grammar
- **Multi-Language Runtime**: Register multiple grammars and compile from different languages
- **Compiler Integration**: Compile and execute Zyntax code at runtime
- **Multi-Tier JIT**: Automatic optimization of hot code paths
- **Async/Await**: Promise-based async operations with `.then()` and `.catch()`
- **Bidirectional Interop**: Convert between Rust and Zyntax types seamlessly
- **Hot Reloading**: Update functions without restarting
- **ZRTL Plugin Loading**: Load native runtime libraries dynamically

## Quick Start

### Using a Language Grammar

The easiest way to use zyntax_embed is with a `.zyn` grammar file:

```rust
use zyntax_embed::{ZyntaxRuntime, LanguageGrammar, ZyntaxValue};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Compile grammar from .zyn file (or load pre-compiled .zpeg)
    let grammar = LanguageGrammar::compile_zyn_file("grammars/zig.zyn")?;
    // Or: let grammar = LanguageGrammar::load("zig.zpeg")?;

    // Create runtime
    let mut runtime = ZyntaxRuntime::new()?;

    // Compile source code using the grammar
    runtime.compile_with_grammar(&grammar, r#"
        pub fn add(a: i32, b: i32) i32 {
            return a + b;
        }

        pub fn main() i32 {
            return add(10, 32);
        }
    "#)?;

    // Call functions with automatic type conversion
    let result: i32 = runtime.call("main", &[])?;
    println!("Result: {}", result); // Output: 42

    Ok(())
}
```

### Manual Pipeline (Advanced)

For more control over compilation:

```rust
use zyntax_embed::{ZyntaxRuntime, ZyntaxValue, FromZyntax, compile_to_hir};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Create runtime and compile HIR directly
    let mut runtime = ZyntaxRuntime::new()?;
    let hir = compile_to_hir(&typed_program, type_registry, config)?;
    runtime.compile_module(&hir)?;

    // Call functions with automatic type conversion
    let result: i32 = runtime.call("add", &[10.into(), 20.into()])?;
    Ok(())
}
```

## Language Grammar Interface

### Creating Grammars

```rust
use zyntax_embed::LanguageGrammar;

// From .zyn source string
let grammar = LanguageGrammar::compile_zyn(include_str!("my_lang.zyn"))?;

// From .zyn file
let grammar = LanguageGrammar::compile_zyn_file("grammars/my_lang.zyn")?;

// From pre-compiled .zpeg file (faster startup)
let grammar = LanguageGrammar::load("my_lang.zpeg")?;

// Save compiled grammar for distribution
grammar.save("my_lang.zpeg")?;
```

### Grammar Metadata

```rust
let grammar = LanguageGrammar::load("zig.zpeg")?;

println!("Language: {}", grammar.name());           // "Zig"
println!("Version: {}", grammar.version());         // "0.11"
println!("Extensions: {:?}", grammar.file_extensions()); // [".zig"]

if let Some(entry) = grammar.entry_point() {
    println!("Entry point: {}", entry);
}
```

### Parsing Without Compilation

```rust
// Parse to TypedProgram for custom processing
let program = grammar.parse(source_code)?;

// Parse to JSON for debugging
let json = grammar.parse_to_json(source_code)?;
println!("{}", json);
```

## Multi-Language Runtime

Register multiple language grammars and load modules by language name:

```rust
use zyntax_embed::{ZyntaxRuntime, LanguageGrammar};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut runtime = ZyntaxRuntime::new()?;

    // Register language grammars
    runtime.register_grammar("zig", LanguageGrammar::compile_zyn_file("grammars/zig.zyn")?);
    runtime.register_grammar("python", LanguageGrammar::compile_zyn_file("grammars/python.zyn")?);
    runtime.register_grammar("calc", LanguageGrammar::compile_zyn_file("grammars/calc.zyn")?);

    // Load modules by language name
    runtime.load_module("zig", r#"
        pub fn add(a: i32, b: i32) i32 {
            return a + b;
        }
    "#)?;

    runtime.load_module("calc", "def multiply(a, b) = a * b")?;

    // Call functions from any loaded module
    let sum: i32 = runtime.call("add", &[10.into(), 32.into()])?;
    println!("Sum: {}", sum); // 42

    Ok(())
}
```

### Auto-detection from File Extension

```rust
// Register grammars (extensions are auto-detected from grammar metadata)
runtime.register_grammar("zig", LanguageGrammar::compile_zyn_file("zig.zyn")?);

// Load files - language detected from extension
runtime.load_module_file("./src/math.zig")?;   // Uses "zig" grammar
runtime.load_module_file("./src/utils.py")?;   // Uses "python" grammar

// Query registered languages
println!("Languages: {:?}", runtime.languages());
println!("Language for .zig: {:?}", runtime.language_for_extension(".zig"));
```

### Cross-Language Interop

Functions from different languages can call each other via explicit exports:

```rust
// Load math utilities in Zig and EXPORT "square" for cross-module linking
runtime.load_module_with_exports("zig", r#"
    pub fn square(x: i32) i32 { return x * x; }
"#, &["square"])?;

// Load DSL that uses the exported Zig function via extern
runtime.load_module("calc", r#"
    extern fn square(x: i32) i32;
    def sum_of_squares(a, b) = square(a) + square(b)
"#)?;

// Call the DSL function that internally uses Zig
let result: i32 = runtime.call("sum_of_squares", &[3.into(), 4.into()])?;
assert_eq!(result, 25); // 9 + 16
```

#### Export Management

```rust
// Export functions after loading
runtime.load_module("zig", source)?;
runtime.export_function("my_func")?;

// Check for conflicts before exporting
if runtime.check_export_conflict("my_func").is_some() {
    println!("Warning: my_func already exported");
}

// List all exported symbols
for (name, ptr) in runtime.exported_symbols() {
    println!("Exported: {} at {:?}", name, ptr);
}
```

## Runtime Options

### ZyntaxRuntime (Simple JIT)

Basic single-tier JIT for simple use cases:

```rust
use zyntax_embed::ZyntaxRuntime;

let mut runtime = ZyntaxRuntime::new()?;

// With external FFI symbols
let symbols = &[("my_c_func", my_c_func as *const u8)];
let mut runtime = ZyntaxRuntime::with_symbols(symbols)?;
```

### TieredRuntime (Multi-Tier JIT)

Adaptive compilation with automatic optimization:

```rust
use zyntax_embed::{TieredRuntime, TieredConfig, OptimizationTier};

// Development: Fast startup, no background optimization
let mut runtime = TieredRuntime::development()?;

// Production: Full tiered optimization
let mut runtime = TieredRuntime::production()?;

// Production with LLVM for Tier 2 (requires llvm-backend feature)
#[cfg(feature = "llvm-backend")]
let mut runtime = TieredRuntime::production_llvm()?;
```

## Multi-Tier JIT Architecture

The tiered JIT automatically optimizes frequently-called functions:

| Tier | Name | Backend | Use Case |
|------|------|---------|----------|
| 0 | Baseline | Cranelift (fast) | Cold code, startup |
| 1 | Standard | Cranelift (optimized) | Warm code |
| 2 | Optimized | Cranelift/LLVM (aggressive) | Hot code |

### How It Works

1. All functions start at **Tier 0** (fast baseline compilation)
2. Execution counters track call frequency
3. Warm threshold crossed → recompile at **Tier 1**
4. Hot threshold crossed → recompile at **Tier 2**
5. Function pointers atomically swapped (no pause required)

### Manual Optimization

Pre-warm hot paths for latency-sensitive code:

```rust
// Force function to Tier 2
runtime.optimize_function("critical_path", OptimizationTier::Optimized)?;

// Get optimization statistics
let stats = runtime.statistics();
println!("{}", stats.format());
```

## Calling Functions

```rust
// Type-safe call (uses DynamicValue ABI)
let sum: i32 = runtime.call("add", &[5.into(), 3.into()])?;

// Native calling with explicit signature (for JIT-compiled functions)
use zyntax_embed::{NativeType, NativeSignature};

// Define signature: (i32, i32) -> i32
let sig = NativeSignature::new(&[NativeType::I32, NativeType::I32], NativeType::I32);
let result = runtime.call_native("add", &[5.into(), 3.into()], &sig)?;
assert_eq!(result.as_i32().unwrap(), 8);

// Or parse signature from string
let sig = NativeSignature::parse("(i32, i32) -> i32").unwrap();
let result = runtime.call_native("add", &[10.into(), 32.into()], &sig)?;

// Get raw ZyntaxValue
let result = runtime.call_raw("compute", &[data.into()])?;

match result {
    ZyntaxValue::Int(n) => println!("Got: {}", n),
    ZyntaxValue::String(s) => println!("Got: {}", s),
    ZyntaxValue::Struct { fields, .. } => {
        if let Some(x) = fields.get("x") {
            println!("x = {:?}", x);
        }
    }
    _ => {}
}
```

### Native Types

The following native types are supported for `call_native`:

| NativeType | Rust Type | Size |
|------------|-----------|------|
| `I32` | `i32` | 4 bytes |
| `I64` | `i64` | 8 bytes |
| `F32` | `f32` | 4 bytes |
| `F64` | `f64` | 8 bytes |
| `Bool` | `bool` | 1 byte |
| `Void` | `()` | 0 bytes |
| `Ptr` | `*mut u8` | pointer size |

## Async Functions and Promises

Async functions return `ZyntaxPromise`:

```rust
use zyntax_embed::{ZyntaxPromise, PromiseState};

let promise = runtime.call_async("fetch_user", &[user_id.into()])?;

// Block and wait
let user: User = promise.await_result()?;

// Or poll manually
loop {
    match promise.poll() {
        PromiseState::Pending => std::thread::yield_now(),
        PromiseState::Ready(value) => break,
        PromiseState::Failed(err) => return Err(err.into()),
    }
}

// Chain with .then() and .catch()
let processed = promise
    .then(|user| ZyntaxValue::String(format!("User: {:?}", user)))
    .catch(|err| ZyntaxValue::String(format!("Error: {}", err)));

// Use with async/await (implements Future)
let result = promise.await?;
```

## Value Conversion

### ZyntaxValue

```rust
use zyntax_embed::ZyntaxValue;

// Create from Rust types
let int_val: ZyntaxValue = 42i32.into();
let str_val: ZyntaxValue = "hello".into();
let vec_val: ZyntaxValue = vec![1, 2, 3].into();

// Create structs
let point = ZyntaxValue::new_struct("Point")
    .field("x", 10.0f64)
    .field("y", 20.0f64)
    .build();

// Access values
if let Some(n) = point.get_field("x").and_then(|v| v.as_float()) {
    println!("x = {}", n);
}
```

### FromZyntax / IntoZyntax Traits

```rust
use zyntax_embed::{FromZyntax, IntoZyntax};

// Rust → ZyntaxValue
let value = 42i32.into_zyntax();

// ZyntaxValue → Rust
let num: i32 = FromZyntax::from_zyntax(value)?;
let opt: Option<i32> = FromZyntax::from_zyntax(value)?;
```

### ZyntaxString (Zero-Copy)

```rust
use zyntax_embed::ZyntaxString;

let s = ZyntaxString::from_str("Hello!");

// Get raw pointer for FFI
let ptr = s.as_ptr();  // Memory: [i32 length][utf8_bytes...]

println!("{}", s.as_str().unwrap());
```

### ZyntaxArray (Zero-Copy)

```rust
use zyntax_embed::ZyntaxArray;

let mut arr: ZyntaxArray<i32> = [1, 2, 3].into();
arr.push(4);

// Get raw pointer for FFI
let ptr = arr.as_ptr();  // Memory: [i32 capacity][i32 length][elements...]
```

## Hot Reloading

```rust
let new_function = /* compile new version */;
runtime.hot_reload("my_function", &new_function)?;
// Next call uses new version
```

## Import Resolution

Register resolvers for handling import statements:

```rust
// Callback-based resolver
runtime.add_import_resolver(Box::new(|module_path| {
    match module_path {
        "std.math" => Ok(Some(include_str!("stdlib/math.zig").to_string())),
        _ => Ok(None), // Not found, try next resolver
    }
}));

// File-system resolver (looks for .zig files in ./src)
runtime.add_filesystem_resolver("./src", "zig");
```

## External Function Registration

Register Rust functions directly with the runtime:

```rust
// Define extern "C" functions
extern "C" fn native_add(a: i32, b: i32) -> i32 { a + b }
extern "C" fn native_print(x: i32) { println!("{}", x); }

// Register at construction
let symbols: &[(&str, *const u8)] = &[
    ("native_add", native_add as *const u8),
    ("native_print", native_print as *const u8),
];
let mut runtime = ZyntaxRuntime::with_symbols(symbols)?;

// Use from Zyntax code
runtime.compile_with_grammar(&grammar, r#"
    extern fn native_add(a: i32, b: i32) i32;
    extern fn native_print(x: i32) void;

    pub fn main() i32 {
        const result = native_add(10, 32);
        native_print(result);
        return result;
    }
"#)?;
```

### Supported Types

| Rust Type | Zyntax Type |
|-----------|-------------|
| `i32`, `i64` | `i32`, `i64` |
| `f32`, `f64` | `f32`, `f64` |
| `bool` | `bool` |
| `*const u8` | `[]const u8` (string pointer) |
| `*mut T` | `*T` (mutable pointer) |

### Working with Strings

```rust
extern "C" fn print_string(ptr: *const u8) {
    unsafe {
        let len = *(ptr as *const i32) as usize;
        let data = ptr.add(4);
        let bytes = std::slice::from_raw_parts(data, len);
        println!("{}", std::str::from_utf8(bytes).unwrap_or(""));
    }
}
```

## ZRTL Plugin Loading

Load native runtime libraries:

```rust
// Load single plugin
runtime.load_plugin("./my_runtime.zrtl")?;

// Load all plugins from directory
let count = runtime.load_plugins_from_directory("./plugins")?;
println!("Loaded {} plugins", count);
```

## Working with Opaque Values

```rust
if value.is_opaque() {
    if let Some(meta) = value.opaque_type_meta() {
        println!("Type: {:?}", meta.type_id);
    }

    unsafe {
        if let Some(data) = value.opaque_as_ref::<MyType>() {
            // Use data...
        }
    }
}
```

## Error Handling

```rust
use zyntax_embed::{RuntimeError, ConversionError};

match runtime.call::<i32>("compute", &args) {
    Ok(result) => println!("Result: {}", result),
    Err(RuntimeError::FunctionNotFound(name)) => {
        eprintln!("Function '{}' not found", name);
    }
    Err(RuntimeError::Conversion(ConversionError::TypeMismatch { expected, actual })) => {
        eprintln!("Type error: expected {:?}, got {:?}", expected, actual);
    }
    Err(e) => eprintln!("Error: {}", e),
}
```

## Complete Example

```rust
use zyntax_embed::{
    TieredRuntime, ZyntaxValue, FromZyntax,
    compile_to_hir, OptimizationTier,
};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Create tiered runtime
    let mut runtime = TieredRuntime::production()?;

    // Compile and load
    let hir = compile_to_hir(&typed_program, type_registry, config)?;
    runtime.compile_module(hir)?;

    // Pre-warm critical path
    runtime.optimize_function("hot_path", OptimizationTier::Optimized)?;

    // Sync call
    let greeting: String = runtime.call("greet", &["World".into()])?;
    println!("{}", greeting);

    // Async call with chaining
    let result = runtime
        .call_async("fetch_data", &["https://api.example.com".into()])?
        .then(|data| ZyntaxValue::String(format!("Got: {:?}", data)))
        .catch(|err| ZyntaxValue::String(format!("Error: {}", err)))
        .await_result::<String>()?;

    println!("{}", result);

    // Check stats
    println!("{}", runtime.statistics().format());

    Ok(())
}
```

## Memory Safety

- `ZyntaxValue` is fully owned with Rust ownership semantics
- `ZyntaxString` and `ZyntaxArray` track ownership for proper cleanup
- `Opaque` values preserve `TypeMeta` for correct memory management
- Promise state is thread-safe via internal mutex
- Function pointers atomically swapped during optimization

## Documentation

- **[The Zyn Book: Embedding SDK](../../book/12-embedding-sdk.md)** - Comprehensive tutorial with examples
- **[Architecture: Embedding SDK](../../docs/EMBEDDING_SDK.md)** - Technical architecture documentation
- **[Tiered Compilation](../../docs/tiered-compilation.md)** - Multi-tier JIT architecture
- **[Grammar Syntax](../../book/04-grammar-syntax.md)** - Writing `.zyn` grammar files
