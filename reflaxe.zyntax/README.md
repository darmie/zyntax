# reflaxe.zyntax

Compile Haxe to native code via the Zyntax compiler backend.

## Overview

This is a [Reflaxe](https://github.com/SomeRanDev/reflaxe) backend that enables compiling Haxe code to native executables using the Zyntax compiler infrastructure. It translates Haxe's typed AST into Zyntax TypedAST JSON format, which is then compiled to native code via Cranelift JIT or LLVM AOT.

## Architecture

```
Haxe Source Code
      ↓
Haxe Typed AST (via Reflaxe)
      ↓
Zyntax TypedAST JSON (this project)
      ↓
Zyntax HIR (High-level IR)
      ↓
Cranelift/LLVM Backend
      ↓
Native Binary
```

## Installation

### Prerequisites

1. Install Haxe (4.0 or later)
2. Install Reflaxe framework:
   ```bash
   haxelib install reflaxe 4.0.0-beta
   ```
3. Build the Zyntax compiler (see main project README)

### Installing reflaxe.zyntax

From the `reflaxe.zyntax` directory:

```bash
haxelib dev reflaxe.zyntax .
```

Or install from haxelib (when published):
```bash
haxelib install reflaxe.zyntax
```

## Usage

### Basic Compilation

Add to your Haxe build file (`.hxml`):

```hxml
-lib reflaxe.zyntax
-D zyntax-output=output
-main Main
```

Then compile:

```bash
haxe build.hxml
```

This generates TypedAST JSON files in the `output/` directory.

### Compiling to Native Binary

After generating JSON files, use the Zyntax compiler:

```bash
zyntax compile output/*.json -o my_program
```

## Example

**Main.hx:**
```haxe
class Main {
    static function main() {
        trace("Hello from Haxe via Zyntax!");

        var result = fibonacci(10);
        trace('Fibonacci(10) = $result');
    }

    static function fibonacci(n: Int): Int {
        if (n <= 1) return n;
        return fibonacci(n - 1) + fibonacci(n - 2);
    }
}
```

**Compile:**
```bash
haxe -lib reflaxe.zyntax -D zyntax-output=out -main Main
zyntax compile out/*.json -o fibonacci
./fibonacci
```

**Output:**
```
Hello from Haxe via Zyntax!
Fibonacci(10) = 55
```

## Features

### Supported Haxe Features

- ✅ Classes and interfaces
- ✅ Enums and pattern matching
- ✅ Functions and methods
- ✅ Closures/lambdas
- ✅ Generics/type parameters
- ✅ Static typing
- ✅ Basic operators
- ✅ Control flow (if, while, for)
- ✅ Arrays and collections
- ✅ String operations
- ⚠️ Async/await (partial - via async runtime)
- ⚠️ Macros (compile-time only, not runtime)
- ❌ Reflection (not yet supported)
- ❌ Dynamic typing (limited support)

### Type Mapping

| Haxe Type | Zyntax Type |
|-----------|-------------|
| `Int` | `i32` |
| `Float` | `f64` |
| `Bool` | `bool` |
| `String` | `String` |
| `Void` | `()` (unit) |
| `Array<T>` | `Array<T>` |
| `Class<T>` | Nominal type |
| `Enum<T>` | Enum |
| `Function` | Function type |
| `Dynamic` | Dynamic (gradual typing) |

## Configuration

### Compiler Defines

- `-D zyntax-output=<dir>` - Output directory for JSON files (default: `zyntax_output`)
- `-D zyntax-backend=<jit|aot>` - Compilation backend (default: `jit`)
- `-D zyntax-opt-level=<0-3>` - Optimization level (default: `2`)
- `-D zyntax-target=<triple>` - Target triple for cross-compilation

### Reserved Keywords

The compiler automatically renames Haxe identifiers that conflict with Rust/Zyntax keywords:
`as`, `async`, `await`, `const`, `fn`, `impl`, `let`, `match`, `mod`, `mut`, `pub`, `return`, `self`, `struct`, `trait`, `type`, `use`, `where`, `while`, etc.

## Development

### Project Structure

```
reflaxe.zyntax/
├── src/
│   └── zyntax/
│       ├── ZyntaxCompiler.hx        # Main compiler logic
│       └── ZyntaxCompilerInit.hx    # Initialization macro
├── std/                             # Haxe standard library overrides
├── extraParams.hxml                 # Auto-included compiler settings
├── haxelib.json                     # Package metadata
└── README.md
```

### Building from Source

1. Clone the repository
2. Set up development mode:
   ```bash
   haxelib dev reflaxe.zyntax .
   ```
3. Run tests:
   ```bash
   cd tests
   haxe test.hxml
   ```

## Limitations

1. **No Runtime Reflection**: Haxe's runtime reflection is not supported
2. **Limited Dynamic**: Dynamic typing has performance overhead
3. **Macros**: Only compile-time macros work; runtime code generation is not supported
4. **Platform-Specific APIs**: Some Haxe standard library APIs may not be available

## Performance

Zyntax uses a tiered JIT compilation strategy:

1. **Baseline Tier** - Fast compilation, basic optimization
2. **Standard Tier** - Moderate compilation time, good performance
3. **Optimized Tier** - Aggressive optimization via LLVM

For production builds, use AOT compilation with LLVM backend for maximum performance.

## Contributing

Contributions are welcome! Please see the main Zyntax project for contribution guidelines.

## License

MIT License - See LICENSE file

## See Also

- [Zyntax Compiler](../README.md) - Main compiler project
- [Reflaxe Framework](https://github.com/SomeRanDev/reflaxe) - Backend framework
- [TypedAST Documentation](../crates/typed_ast/README.md) - IR format specification
