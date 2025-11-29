# Runtime Plugins (ZRTL)

The Zyntax Runtime Library (ZRTL) provides native functionality to languages built with Zyntax. Rather than implementing I/O, networking, or threading in each language, ZRTL plugins provide a consistent, high-performance native layer that any Zyntax-based language can use.

## Architecture

```text
┌─────────────────────────────────────────────────────────┐
│                    Your Language                        │
│              (Haxe, Zig, Custom DSL)                    │
└─────────────────────────────────────────────────────────┘
                          │
                          ▼
┌─────────────────────────────────────────────────────────┐
│                   Zyntax Compiler                       │
│            (Grammar → TypedAST → HIR → Code)            │
└─────────────────────────────────────────────────────────┘
                          │
                          ▼
┌─────────────────────────────────────────────────────────┐
│                    ZRTL Plugins                         │
│  ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐       │
│  │  I/O    │ │   FS    │ │  Time   │ │  Net    │  ...  │
│  └─────────┘ └─────────┘ └─────────┘ └─────────┘       │
└─────────────────────────────────────────────────────────┘
                          │
                          ▼
┌─────────────────────────────────────────────────────────┐
│                   Operating System                      │
└─────────────────────────────────────────────────────────┘
```

## Plugin Types

ZRTL plugins are built as:

- **Dynamic libraries** (`.zrtl` files) - For JIT execution and runtime loading
- **Static libraries** (`.a` files) - For AOT compilation and standalone binaries

## Standard Plugins

### zrtl_io - Input/Output

Basic I/O operations for console interaction.

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$IO$print` | `(StringPtr) -> void` | Print string without newline |
| `$IO$println` | `(StringPtr) -> void` | Print string with newline |
| `$IO$print_int` | `(i64) -> void` | Print integer |
| `$IO$print_float` | `(f64) -> void` | Print float |
| `$IO$print_bool` | `(i32) -> void` | Print boolean |
| `$IO$read_line` | `() -> StringPtr` | Read line from stdin |
| `$IO$flush` | `() -> void` | Flush stdout |
| `$IO$eprint` | `(StringPtr) -> void` | Print to stderr |
| `$IO$eprintln` | `(StringPtr) -> void` | Print to stderr with newline |

### zrtl_fs - File System

File and directory operations.

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$FS$read_file` | `(StringPtr) -> StringPtr` | Read entire file to string |
| `$FS$write_file` | `(StringPtr, StringPtr) -> i32` | Write string to file |
| `$FS$append_file` | `(StringPtr, StringPtr) -> i32` | Append to file |
| `$FS$exists` | `(StringPtr) -> i32` | Check if path exists |
| `$FS$is_file` | `(StringPtr) -> i32` | Check if path is file |
| `$FS$is_dir` | `(StringPtr) -> i32` | Check if path is directory |
| `$FS$mkdir` | `(StringPtr) -> i32` | Create directory |
| `$FS$mkdir_all` | `(StringPtr) -> i32` | Create directory recursively |
| `$FS$remove` | `(StringPtr) -> i32` | Remove file |
| `$FS$remove_dir` | `(StringPtr) -> i32` | Remove empty directory |
| `$FS$remove_dir_all` | `(StringPtr) -> i32` | Remove directory recursively |
| `$FS$rename` | `(StringPtr, StringPtr) -> i32` | Rename/move file |
| `$FS$copy` | `(StringPtr, StringPtr) -> i32` | Copy file |
| `$FS$list_dir` | `(StringPtr) -> ArrayPtr` | List directory contents |
| `$FS$file_size` | `(StringPtr) -> i64` | Get file size in bytes |

### zrtl_time - Time & Duration

Time operations and sleeping.

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Time$now_secs` | `() -> i64` | Unix timestamp (seconds) |
| `$Time$now_millis` | `() -> i64` | Unix timestamp (milliseconds) |
| `$Time$now_micros` | `() -> i64` | Unix timestamp (microseconds) |
| `$Time$monotonic_nanos` | `() -> u64` | Monotonic time (nanoseconds) |
| `$Time$sleep_secs` | `(u64) -> void` | Sleep for seconds |
| `$Time$sleep_millis` | `(u64) -> void` | Sleep for milliseconds |
| `$Time$sleep_micros` | `(u64) -> void` | Sleep for microseconds |
| `$Time$instant_now` | `() -> u64` | Create timing instant |
| `$Time$instant_elapsed_nanos` | `(u64) -> u64` | Nanoseconds since instant |
| `$Time$instant_elapsed_millis` | `(u64) -> u64` | Milliseconds since instant |
| `$Time$instant_free` | `(u64) -> void` | Free instant handle |
| `$Time$format_iso8601` | `(i64) -> StringPtr` | Format as ISO 8601 |

### zrtl_thread - Threading & Atomics

Concurrency primitives.

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Thread$spawn` | `(fn(i64)->i64, i64) -> u64` | Spawn thread with function |
| `$Thread$spawn_closure` | `(*ZrtlClosure, i64) -> u64` | Spawn with closure |
| `$Thread$join` | `(u64) -> i64` | Wait for thread, get result |
| `$Thread$current_id` | `() -> u64` | Get current thread ID |
| `$Thread$yield_now` | `() -> void` | Yield to scheduler |
| `$Thread$park` | `() -> void` | Park current thread |
| `$Thread$unpark` | `(u64) -> i32` | Unpark thread by handle |
| `$Thread$available_parallelism` | `() -> u32` | Get CPU core count |
| `$Atomic$new` | `(i64) -> u64` | Create atomic i64 |
| `$Atomic$load` | `(u64) -> i64` | Load atomically |
| `$Atomic$store` | `(u64, i64) -> void` | Store atomically |
| `$Atomic$add` | `(u64, i64) -> i64` | Fetch-add |
| `$Atomic$compare_exchange` | `(u64, i64, i64) -> i32` | CAS operation |
| `$Mutex$new` | `() -> u64` | Create mutex |
| `$Mutex$lock` | `(u64) -> i32` | Lock mutex |
| `$Mutex$try_lock` | `(u64) -> i32` | Try lock (non-blocking) |
| `$Mutex$unlock` | `(u64) -> i32` | Unlock mutex |

### zrtl_net - Networking

TCP and UDP socket operations.

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Net$tcp_connect` | `(StringPtr) -> u64` | Connect to TCP server |
| `$Net$tcp_connect_timeout` | `(StringPtr, u64) -> u64` | Connect with timeout (ms) |
| `$Net$tcp_read` | `(u64, u32) -> ArrayPtr` | Read bytes from connection |
| `$Net$tcp_write` | `(u64, ArrayPtr) -> i64` | Write bytes |
| `$Net$tcp_write_string` | `(u64, StringPtr) -> i64` | Write string |
| `$Net$tcp_close` | `(u64) -> void` | Close connection |
| `$Net$tcp_listen` | `(StringPtr) -> u64` | Create TCP listener |
| `$Net$tcp_accept` | `(u64) -> u64` | Accept connection |
| `$Net$tcp_listener_close` | `(u64) -> void` | Close listener |
| `$Net$udp_bind` | `(StringPtr) -> u64` | Create UDP socket |
| `$Net$udp_send_to` | `(u64, StringPtr, ArrayPtr) -> i64` | Send UDP packet |
| `$Net$udp_recv` | `(u64, u32) -> ArrayPtr` | Receive UDP packet |
| `$Net$udp_close` | `(u64) -> void` | Close UDP socket |

### zrtl_env - Environment

Environment variables and process information.

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Env$get` | `(StringPtr) -> StringPtr` | Get env variable |
| `$Env$set` | `(StringPtr, StringPtr) -> i32` | Set env variable |
| `$Env$remove` | `(StringPtr) -> void` | Remove env variable |
| `$Env$has` | `(StringPtr) -> i32` | Check if env var exists |
| `$Env$args_count` | `() -> i32` | Get argument count |
| `$Env$arg` | `(i32) -> StringPtr` | Get argument at index |
| `$Env$exe_path` | `() -> StringPtr` | Get executable path |
| `$Env$exit` | `(i32) -> !` | Exit process |
| `$Env$pid` | `() -> u32` | Get process ID |
| `$Env$home_dir` | `() -> StringPtr` | Get home directory |
| `$Env$temp_dir` | `() -> StringPtr` | Get temp directory |
| `$Env$current_dir` | `() -> StringPtr` | Get working directory |
| `$Env$os` | `() -> StringPtr` | Get OS name |
| `$Env$arch` | `() -> StringPtr` | Get CPU architecture |

### zrtl_math - Mathematics

Mathematical functions for numerical computation.

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Math$sin` | `(f64) -> f64` | Sine (radians) |
| `$Math$cos` | `(f64) -> f64` | Cosine (radians) |
| `$Math$tan` | `(f64) -> f64` | Tangent (radians) |
| `$Math$asin` | `(f64) -> f64` | Arcsine |
| `$Math$acos` | `(f64) -> f64` | Arccosine |
| `$Math$atan` | `(f64) -> f64` | Arctangent |
| `$Math$atan2` | `(f64, f64) -> f64` | Two-argument arctangent |
| `$Math$sinh` | `(f64) -> f64` | Hyperbolic sine |
| `$Math$cosh` | `(f64) -> f64` | Hyperbolic cosine |
| `$Math$tanh` | `(f64) -> f64` | Hyperbolic tangent |
| `$Math$exp` | `(f64) -> f64` | e^x |
| `$Math$exp2` | `(f64) -> f64` | 2^x |
| `$Math$log` | `(f64) -> f64` | Natural logarithm |
| `$Math$log2` | `(f64) -> f64` | Base-2 logarithm |
| `$Math$log10` | `(f64) -> f64` | Base-10 logarithm |
| `$Math$pow` | `(f64, f64) -> f64` | Power (x^y) |
| `$Math$sqrt` | `(f64) -> f64` | Square root |
| `$Math$cbrt` | `(f64) -> f64` | Cube root |
| `$Math$hypot` | `(f64, f64) -> f64` | Hypotenuse |
| `$Math$floor` | `(f64) -> f64` | Round down |
| `$Math$ceil` | `(f64) -> f64` | Round up |
| `$Math$round` | `(f64) -> f64` | Round to nearest |
| `$Math$trunc` | `(f64) -> f64` | Truncate toward zero |
| `$Math$abs` | `(f64) -> f64` | Absolute value (float) |
| `$Math$abs_i64` | `(i64) -> i64` | Absolute value (int) |
| `$Math$min` | `(f64, f64) -> f64` | Minimum |
| `$Math$max` | `(f64, f64) -> f64` | Maximum |
| `$Math$clamp` | `(f64, f64, f64) -> f64` | Clamp to range |
| `$Math$random` | `() -> f64` | Random [0, 1) |
| `$Math$random_range` | `(f64, f64) -> f64` | Random in range |
| `$Math$random_int` | `(i64, i64) -> i64` | Random integer |
| `$Math$seed` | `(u64) -> void` | Seed RNG |
| `$Math$pi` | `() -> f64` | Pi constant |
| `$Math$e` | `() -> f64` | Euler's number |
| `$Math$tau` | `() -> f64` | Tau (2π) |
| `$Math$lerp` | `(f64, f64, f64) -> f64` | Linear interpolation |
| `$Math$to_radians` | `(f64) -> f64` | Degrees to radians |
| `$Math$to_degrees` | `(f64) -> f64` | Radians to degrees |

## Building Plugins

### Dynamic Libraries (.zrtl)

```bash
cd plugins
./build_zrtl.sh --release
```

Output in `plugins/target/zrtl/`:

```text
zrtl_io.zrtl
zrtl_fs.zrtl
zrtl_time.zrtl
zrtl_thread.zrtl
zrtl_net.zrtl
zrtl_env.zrtl
zrtl_math.zrtl
plugins.json
```

### Static Libraries (.a)

```bash
cd plugins
./build_static.sh --release

# Cross-compile for specific target
./build_static.sh --target aarch64-apple-darwin

# Build for all supported targets
./build_static.sh --all
```

Output in `plugins/target/static/<target>/`:

```text
libzrtl_io.a
libzrtl_fs.a
...
libs.json
```

## Using Plugins in Your Language

### Grammar Integration

Map your language's standard library to ZRTL symbols:

```zyn
@builtin {
    // I/O
    print: "$IO$print",
    println: "$IO$println",
    readLine: "$IO$read_line",

    // File System
    readFile: "$FS$read_file",
    writeFile: "$FS$write_file",

    // Time
    now: "$Time$now_millis",
    sleep: "$Time$sleep_millis",
}
```

### In Your Language Source

```zig
// Your custom language
fn main() {
    println("Hello from ZRTL!");

    let contents = readFile("data.txt");
    println(contents);

    sleep(1000);  // Sleep 1 second
}
```

### Loading at Runtime (JIT)

ZRTL plugins are loaded at runtime using the `ZyntaxRuntime` API:

```rust
use zyntax_embed::{ZyntaxRuntime, LanguageGrammar};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Step 1: Create a new runtime
    let mut runtime = ZyntaxRuntime::new()?;

    // Step 2: Load ZRTL plugins (dynamic libraries)
    // Option A: Load individual plugins
    runtime.load_plugin("plugins/target/zrtl/zrtl_io.zrtl")?;
    runtime.load_plugin("plugins/target/zrtl/zrtl_fs.zrtl")?;

    // Option B: Load all plugins from a directory
    let count = runtime.load_plugins_from_directory("plugins/target/zrtl")?;
    println!("Loaded {} plugins", count);

    // Step 3: Load your language grammar
    let grammar = LanguageGrammar::compile_zyn(include_str!("my_lang.zyn"))?;
    runtime.register_grammar("mylang", grammar.clone())?;
    runtime.map_extension("mylang", "ml")?;  // .ml files use this grammar

    // Step 4: Compile source code that uses plugin functions
    let source = r#"
        fn main() {
            // These calls resolve to $IO$println and $FS$read_file
            println("Hello from ZRTL!");
            let contents = readFile("data.txt");
            println(contents);
        }
    "#;

    runtime.compile_source(&grammar, source)?;

    // Step 5: Call the compiled function
    runtime.call::<()>("main", &[])?;

    Ok(())
}
```

### Loading Plugins with Custom Symbols

For advanced use cases, you can also register external symbols directly:

```rust
use zyntax_embed::ZyntaxRuntime;

// Define a native function
extern "C" fn my_custom_print(msg: i64) {
    println!("Custom: {}", msg);
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Create runtime with custom symbols
    let symbols: &[(&str, *const u8)] = &[
        ("$Custom$print", my_custom_print as *const u8),
    ];
    let mut runtime = ZyntaxRuntime::with_symbols(symbols)?;

    // Now code can call $Custom$print
    // ...

    Ok(())
}
```

### Static Linking (AOT)

For standalone executables, link against static libraries:

```bash
# Compile your language to object file
zyntax compile --source main.mylang --output main.o --aot

# Link with ZRTL static libs
cc main.o \
    -L plugins/target/static/aarch64-apple-darwin \
    -lzrtl_io -lzrtl_fs -lzrtl_time \
    -o main
```

## Memory Management

ZRTL uses specific memory conventions:

### Strings

ZRTL strings have inline length: `[i32 length][utf8 bytes...]`

```rust
// Creating strings
let s: StringPtr = string_new("hello");

// Reading strings
let len = string_length(s);
let data = string_data(s);

// Freeing strings (caller responsibility for returned strings)
string_free(s);
```

### Arrays

ZRTL arrays: `[i32 capacity][i32 length][elements...]`

```rust
let arr: ArrayPtr = array_new::<i32>(10);  // capacity 10
array_push(arr, 42);
let val = array_get::<i32>(arr, 0);
array_free(arr);
```

### Handle-based Resources

File handles, sockets, threads, etc. return `u64` handles:

```rust
let handle = tcp_connect(addr);  // Returns handle
// ... use handle ...
tcp_close(handle);  // Must close when done
```

## Creating Custom Plugins

### Plugin Structure

```rust
// my_plugin/src/lib.rs
use zrtl::{zrtl_plugin, StringPtr, string_new};

#[no_mangle]
pub extern "C" fn my_greet(name: StringPtr) -> StringPtr {
    let name_str = unsafe { zrtl::string_as_str(name) }.unwrap_or("World");
    string_new(&format!("Hello, {}!", name_str))
}

zrtl_plugin! {
    name: "my_plugin",
    symbols: [
        ("$My$greet", my_greet),
    ]
}
```

### Cargo.toml

```toml
[package]
name = "my_plugin"
version = "0.1.0"
edition = "2021"

[lib]
crate-type = ["cdylib", "staticlib", "rlib"]

[dependencies]
zrtl = { path = "../../sdk/zrtl" }
```

### Adding to Workspace

Add your plugin directory to `plugins/Cargo.toml`:

```toml
[workspace]
members = [
    "zrtl_io",
    "zrtl_fs",
    # ... existing plugins
    "my_plugin",  # Your new plugin
]
```

The build scripts will automatically discover and build it.

## Symbol Naming Convention

ZRTL uses a hierarchical naming scheme:

```text
$Module$function_name
```

Examples:
- `$IO$println` - I/O module, println function
- `$FS$read_file` - File system module, read_file function
- `$Net$tcp_connect` - Network module, tcp_connect function

This allows:
1. Clear organization by domain
2. No conflicts between plugins
3. Easy mapping in grammar `@builtin` blocks
