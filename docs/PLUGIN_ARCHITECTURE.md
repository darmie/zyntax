# Zyntax Plugin Architecture

## Overview

Zyntax uses a plugin-based architecture for runtime symbol registration. This keeps the core compiler completely language-agnostic while allowing different language implementations to provide their runtime symbols.

**Note on Terminology**:

- **Haxe → Zyntax**: Uses `reflaxe.zyntax` convention (Reflaxe is a Haxe library for creating compiler targets)
- **Other language implementations**: Would use their own directory structure (e.g., `python_compiler/runtime/`, `js_compiler/runtime/`, etc.)

## Architecture

```
┌──────────────────────────────────────────────────────────┐
│  Stdlib Runtime (crates/runtime/)                        │
│  ┌────────────────────────────────────────────┐         │
│  │  Plugin: "stdlib"                           │         │
│  │  Symbols: print_i32, println_i32, etc.      │         │
│  └────────────────────────────────────────────┘         │
└──────────────────────────────────────────────────────────┘
                          ↓
┌──────────────────────────────────────────────────────────┐
│  Language Runtime (e.g., reflaxe.zyntax/runtime/)        │
│  ┌────────────────────────────────────────────┐         │
│  │  Plugin: "haxe"                             │         │
│  │  Symbols: $Array$create, $String$concat...  │         │
│  │  Note: Other languages use own structure    │         │
│  └────────────────────────────────────────────┘         │
└──────────────────────────────────────────────────────────┘
                          ↓
┌──────────────────────────────────────────────────────────┐
│  CLI (crates/zyntax_cli/)                                 │
│  ┌────────────────────────────────────────────┐         │
│  │  1. Creates PluginRegistry                  │         │
│  │  2. Registers stdlib plugin                 │         │
│  │  3. Registers frontend plugins              │         │
│  │  4. Collects all symbols                    │         │
│  │  5. Passes to CraneliftBackend              │         │
│  └────────────────────────────────────────────┘         │
└──────────────────────────────────────────────────────────┘
                          ↓
┌──────────────────────────────────────────────────────────┐
│  Core Compiler (crates/compiler/)                         │
│  ┌────────────────────────────────────────────┐         │
│  │  Provides: RuntimePlugin trait              │         │
│  │  Provides: PluginRegistry                   │         │
│  │  Provides: CraneliftBackend::with_symbols() │         │
│  │  ❌ NO RUNTIME-SPECIFIC CODE                │         │
│  └────────────────────────────────────────────┘         │
└──────────────────────────────────────────────────────────┘
```

## Key Design Principles

1. **Core compiler is 100% language-agnostic**
   - No Haxe, Python, or any frontend-specific code in `crates/compiler/`
   - No conditional compilation (`#[cfg(feature = "haxe_runtime")]`)
   - Provides generic APIs, not frontend implementations

2. **Language runtimes are standalone**
   - Haxe: Lives in `reflaxe.zyntax/runtime/` (reflaxe is Haxe-specific)
   - Other languages: Use their own directory structure
   - Not part of core Zyntax workspace
   - Can be developed independently

3. **CLI is the integration point**
   - Links frontend runtimes as dependencies
   - Registers plugins at startup
   - Zero hardcoded knowledge in backend

4. **Static linking**
   - Frontend runtimes statically linked into CLI binary
   - No dynamic library loading (dlopen/LoadLibrary)
   - Uses Rust's inventory crate for symbol collection

## Creating a New Language Runtime

**Important**: The `reflaxe.*` naming convention is specific to Haxe's Reflaxe library. Other language compilers targeting Zyntax should use their own appropriate directory structure.

### Step 1: Create the Runtime Crate

Example for a hypothetical Python compiler:

```bash
mkdir -p python_compiler/runtime/src
```

Create `python_compiler/runtime/Cargo.toml`:

```toml
[package]
name = "python_zyntax_runtime"
version = "0.1.0"
edition = "2021"

[lib]
crate-type = ["cdylib", "rlib"]

[dependencies]
libc = "0.2"
zyntax_compiler = { path = "../../zyntax/crates/compiler" }
zyntax_plugin_macros = { path = "../../zyntax/crates/zyntax_plugin_macros" }
inventory = "0.3"
```

### Step 2: Declare the Plugin

Create `python_compiler/runtime/src/lib.rs`:

```rust
use zyntax_plugin_macros::{runtime_plugin, runtime_export};

// Declare this as a Zyntax runtime plugin
runtime_plugin! {
    name: "python",
}
```

This macro generates:

- `RuntimeSymbol` struct for symbol registration
- `PythonPlugin` struct implementing `RuntimePlugin` trait
- `get_plugin()` function to retrieve the plugin instance

### Step 3: Export Runtime Functions

Use the `#[runtime_export]` attribute to mark functions for JIT linking:

```rust
/// Create an array from two elements
#[runtime_export("$Array$create")]
pub extern "C" fn Array_create(elem0: i32, elem1: i32) -> *mut i32 {
    // Implementation...
}

/// Push an element onto array
#[runtime_export("$Array$push")]
pub extern "C" fn Array_push(array_ptr: *mut i32, element: i32) -> *mut i32 {
    // Implementation...
}
```

**Symbol Naming Convention**: `$Type$method`
- `$Array$create` - Array constructor
- `$Array$push` - Array push method
- `$String$concat` - String concatenation
- etc.

### Step 4: Register in CLI

Add to `crates/zyntax_cli/Cargo.toml`:

```toml
[dependencies]
python_zyntax_runtime = { path = "../../python_compiler/runtime" }
```

Register in `crates/zyntax_cli/src/backends/cranelift_jit.rs`:

```rust
pub fn compile_jit(...) -> Result<...> {
    let mut registry = zyntax_compiler::plugin::PluginRegistry::new();

    // Register stdlib
    registry.register(zyntax_runtime::get_plugin())?;

    // Register Python plugin
    registry.register(python_zyntax_runtime::get_plugin())
        .map_err(|e| format!("Failed to register Python plugin: {}", e))?;

    // Collect symbols and pass to backend
    let runtime_symbols = registry.collect_symbols();
    let mut backend = CraneliftBackend::with_runtime_symbols(&runtime_symbols)?;

    // ... rest of compilation
}
```

## How It Works

### Macro-Based Registration

The `runtime_plugin!` macro uses Rust's `inventory` crate for compile-time registration:

```rust
runtime_plugin! {
    name: "haxe",
}
```

Expands to:

```rust
pub struct RuntimeSymbol {
    pub name: &'static str,
    pub ptr: FunctionPtr,  // Thread-safe wrapper around *const u8
}

inventory::collect!(RuntimeSymbol);

pub struct HaxePlugin;

impl zyntax_compiler::plugin::RuntimePlugin for HaxePlugin {
    fn name(&self) -> &str {
        "haxe"
    }

    fn runtime_symbols(&self) -> Vec<(&'static str, *const u8)> {
        inventory::iter::<RuntimeSymbol>
            .into_iter()
            .map(|sym| (sym.name, sym.ptr.as_ptr()))
            .collect()
    }
}

pub fn get_plugin() -> Box<dyn zyntax_compiler::plugin::RuntimePlugin> {
    Box::new(HaxePlugin)
}
```

### Symbol Export

The `#[runtime_export]` attribute:

```rust
#[runtime_export("$Array$create")]
pub extern "C" fn Array_create(elem0: i32, elem1: i32) -> *mut i32 {
    // ...
}
```

Expands to:

```rust
#[no_mangle]
pub extern "C" fn Array_create(elem0: i32, elem1: i32) -> *mut i32 {
    // ... original body
}

inventory::submit! {
    RuntimeSymbol {
        name: "$Array$create",
        ptr: FunctionPtr::new(Array_create as *const u8),
    }
}
```

## Example: Haxe Runtime

See `reflaxe.zyntax/runtime/src/lib.rs` for a complete example:

```rust
use zyntax_plugin_macros::{runtime_plugin, runtime_export};

runtime_plugin! {
    name: "haxe",
}

#[runtime_export("$Array$create")]
pub extern "C" fn Array_create(elem0: i32, elem1: i32) -> *mut i32 {
    unsafe {
        let size = 4 * std::mem::size_of::<i32>();
        let ptr = libc::malloc(size) as *mut i32;
        *ptr = 4;           // capacity
        *ptr.offset(1) = 2; // length
        *ptr.offset(2) = elem0;
        *ptr.offset(3) = elem1;
        ptr
    }
}

#[runtime_export("$Array$push")]
pub extern "C" fn Array_push(array_ptr: *mut i32, element: i32) -> *mut i32 {
    // ... push implementation with realloc
}

#[runtime_export("$Array$get")]
pub extern "C" fn Array_get(array_ptr: *const i32, index: i32) -> i32 {
    // ... bounds-checked get
}

#[runtime_export("$Array$length")]
pub extern "C" fn Array_length(array_ptr: *const i32) -> i32 {
    // ... length accessor
}
```

## Testing Plugins

```bash
# Build CLI with all plugins
cargo build --release

# Run with verbose output to see registered plugins
./target/release/zyntax compile input.json --run --verbose
```

Expected output:
```
info: Registered plugins: ["stdlib", "haxe"]
info: Compiling functions...
info: Running main function...
Hello from Haxe!
42
```

## Advanced: Plugin Lifecycle Hooks

The `RuntimePlugin` trait provides optional lifecycle hooks:

```rust
pub trait RuntimePlugin: Send + Sync {
    fn name(&self) -> &str;
    fn runtime_symbols(&self) -> Vec<(&'static str, *const u8)>;

    // Optional hooks
    fn on_load(&self) -> Result<(), String> {
        Ok(())
    }

    fn on_unload(&self) -> Result<(), String> {
        Ok(())
    }
}
```

Override these in your plugin implementation for initialization/cleanup:

```rust
impl RuntimePlugin for MyPlugin {
    fn name(&self) -> &str {
        "my_lang"
    }

    fn runtime_symbols(&self) -> Vec<(&'static str, *const u8)> {
        // ...
    }

    fn on_load(&self) -> Result<(), String> {
        println!("My language runtime initialized!");
        Ok(())
    }

    fn on_unload(&self) -> Result<(), String> {
        println!("Cleaning up...");
        Ok(())
    }
}
```

## Benefits of This Architecture

1. **Zero Backend Pollution**: Core compiler has no language-specific code
2. **Easy to Add Language Support**: Just implement plugin trait and register
3. **Type-Safe**: Rust's type system ensures correct symbol signatures
4. **Compile-Time Checks**: `inventory` validates symbols at compile time
5. **No Dynamic Loading**: Static linking means no runtime dlopen overhead
6. **Modular**: Each language runtime is independent, can be developed separately
7. **Scalable**: Adding new language compilers doesn't modify core Zyntax code

## Migration from Hardcoded Symbols

Before (hardcoded in CLI):

```rust
let haxe_symbols: &[(&str, *const u8)] = &[
    ("$Array$create", haxe_zyntax_runtime::Array_create as *const u8),
    ("$Array$push", haxe_zyntax_runtime::Array_push as *const u8),
    // ... manual list
];
```

After (plugin-based):

```rust
let mut registry = PluginRegistry::new();
registry.register(haxe_zyntax_runtime::get_plugin())?;
let symbols = registry.collect_symbols(); // Auto-collected!
```

**Advantages**:

- Language runtime authors don't modify CLI code
- Symbols auto-discovered via `inventory`
- Adding new runtime functions doesn't require CLI changes
- Multiple language runtimes can coexist cleanly

## Future Enhancements

Potential improvements to the plugin system:

1. **Dynamic Plugin Loading**: Load plugins from shared libraries at runtime
2. **Plugin Metadata**: Version checks, feature flags, dependencies
3. **Plugin Hot-Reload**: Reload plugins without restarting CLI
4. **Plugin Discovery**: Auto-discover plugins in filesystem
5. **Plugin Sandboxing**: Isolate plugins for security

Currently using static linking for simplicity and performance.
