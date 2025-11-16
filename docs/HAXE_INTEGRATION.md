# Haxe Integration Guide

This document describes how Haxe code is compiled to native executables via the Zyntax compiler.

## Architecture

```
┌─────────────┐
│ Haxe Source │
└──────┬──────┘
       │
       │ Haxe Compiler
       ▼
┌─────────────────┐
│ Haxe Typed AST  │
└──────┬──────────┘
       │
       │ Reflaxe.Zyntax
       ▼
┌──────────────────────┐
│ TypedAST JSON Files  │
└──────┬───────────────┘
       │
       │ Zyntax CLI
       ▼
┌─────────────┐
│ Zyntax HIR  │
└──────┬──────┘
       │
       │ Cranelift/LLVM
       ▼
┌──────────────────┐
│ Native Executable│
└──────────────────┘
```

## Components

### 1. reflaxe.zyntax (Haxe Library)

A [Reflaxe](https://github.com/SomeRanDev/reflaxe) backend that translates Haxe's typed AST into Zyntax TypedAST format as JSON.

**Location:** `reflaxe.zyntax/`

**Key Files:**
- `src/zyntax/ZyntaxCompiler.hx` - Main AST translator
- `src/zyntax/ZyntaxCompilerInit.hx` - Compiler initialization
- `extraParams.hxml` - Auto-included compiler settings
- `haxelib.json` - Package metadata

### 2. Zyntax CLI (Rust Binary)

Command-line tool that consumes TypedAST JSON and compiles to native code.

**Location:** `crates/zyntax_cli/`

**Key Files:**
- `src/main.rs` - CLI implementation with subcommands

## Compilation Pipeline

### Phase 1: Haxe → TypedAST JSON

The Reflaxe.Zyntax backend runs as a Haxe compiler macro and outputs one JSON file per class/enum:

```haxe
// Input: HelloWorld.hx
class HelloWorld {
    static function main() {
        trace("Hello!");
    }
}
```

```json
// Output: HelloWorld.json
{
  "type": "class_declaration",
  "name": "HelloWorld",
  "methods": [
    {
      "type": "method",
      "name": "main",
      "params": [],
      "return_type": {"type": "primitive", "kind": "Unit"},
      "body": { ... },
      "is_static": true
    }
  ]
}
```

### Phase 2: TypedAST JSON → HIR

The Zyntax CLI deserializes JSON into TypedAST structs (via serde) and converts to HIR:

```rust
// Deserialize JSON
let program: TypedProgram = serde_json::from_str(&json_content)?;

// Convert to HIR (TODO: implement full conversion)
let hir_module = typed_ast_to_hir(&program)?;
```

### Phase 3: HIR → Native Code

The HIR is compiled using the tiered JIT/AOT system:

```rust
// JIT compilation
let mut backend = CraneliftBackend::new()?;
backend.compile_module(&hir_module)?;

// Or AOT compilation
// llvm_backend.compile_to_object(&hir_module, "output.o")?;
```

## Type Mapping

| Haxe Type | TypedAST JSON | Zyntax HIR Type |
|-----------|---------------|-----------------|
| `Int` | `{"type": "primitive", "kind": "I32"}` | `HirType::I32` |
| `Float` | `{"type": "primitive", "kind": "F64"}` | `HirType::F64` |
| `Bool` | `{"type": "primitive", "kind": "Bool"}` | `HirType::Bool` |
| `String` | `{"type": "primitive", "kind": "String"}` | `HirType::Pointer` |
| `Void` | `{"type": "primitive", "kind": "Unit"}` | `HirType::Unit` |
| `Array<T>` | `{"type": "array", "element": T}` | Custom struct |
| `Class` | `{"type": "nominal", "name": "..."}` | Custom struct |
| `Enum` | `{"type": "enum", "name": "..."}` | Tagged union |
| `Function` | `{"type": "function", "params": [...]}` | Function type |

## Expression Translation

### Binary Operations

```haxe
// Haxe
var sum = a + b;
```

```json
// TypedAST JSON
{
  "type": "binary",
  "operator": "add",
  "left": {"type": "variable", "name": "a"},
  "right": {"type": "variable", "name": "b"}
}
```

```rust
// HIR (pseudocode)
let sum = builder.add(a, b, HirType::I32);
```

### Function Calls

```haxe
// Haxe
var result = fibonacci(10);
```

```json
// TypedAST JSON
{
  "type": "call",
  "callee": {"type": "identifier", "name": "fibonacci"},
  "arguments": [
    {"type": "literal", "kind": "integer", "value": 10}
  ]
}
```

```rust
// HIR
let ten = builder.iconst(10, HirType::I32);
let result = builder.call(fibonacci_fn, vec![ten]);
```

### Control Flow

```haxe
// Haxe
if (x > 0) {
    trace("positive");
} else {
    trace("non-positive");
}
```

```json
// TypedAST JSON
{
  "type": "if",
  "condition": {
    "type": "binary",
    "operator": "gt",
    "left": {"type": "variable", "name": "x"},
    "right": {"type": "literal", "value": 0}
  },
  "then_branch": { ... },
  "else_branch": { ... }
}
```

```rust
// HIR
let cmp = builder.icmp(IcmpOp::Gt, x, zero);
let then_block = builder.create_block("then");
let else_block = builder.create_block("else");
let merge = builder.create_block("merge");
builder.br_if(cmp, then_block, else_block);
```

## Usage

### Installation

```bash
# Install Reflaxe
haxelib install reflaxe 4.0.0-beta

# Install reflaxe.zyntax (development mode)
cd reflaxe.zyntax
haxelib dev reflaxe.zyntax .

# Build Zyntax CLI
cd ..
cargo build --package zyntax_cli --release
```

### Compiling Haxe Code

```bash
# Create Haxe build file
cat > build.hxml <<EOF
-lib reflaxe.zyntax
-main Main
-D zyntax-output=output
EOF

# Compile Haxe to TypedAST JSON
haxe build.hxml

# Compile TypedAST JSON to native
cargo run --package zyntax_cli -- compile output/*.json -o myprogram

# Or with the binary directly
./target/release/zyntax compile output/*.json -o myprogram --run
```

### Example Project

See `reflaxe.zyntax/test/` for a complete example:

```bash
cd reflaxe.zyntax/test
./compile.sh
```

## Current Limitations

### Implemented

✅ **Type mapping** - Primitives, classes, enums, functions
✅ **Expression translation** - Literals, variables, binary ops, calls
✅ **Statement translation** - Var decls, assignments, returns
✅ **Control flow** - If/else, while loops
✅ **JSON serialization** - Complete TypedAST to JSON

### TODO

❌ **HIR conversion** - TypedAST JSON → HIR is stubbed
❌ **Standard library** - Haxe std lib → Zyntax runtime
❌ **Generics** - Type parameter instantiation
❌ **Closures** - Capture analysis and lifting
❌ **Pattern matching** - Switch/match translation
❌ **Async/await** - Async runtime integration
❌ **Reflection** - Runtime type information
❌ **Execution** - Actually running JIT-compiled code

## Next Steps

The immediate priority is implementing the **TypedAST JSON → HIR** conversion in `crates/zyntax_cli/src/main.rs`:

```rust
fn typed_ast_to_hir(program: &TypedProgram) -> Result<HirModule, Box<dyn std::error::Error>> {
    // TODO: Implement proper TypedAST -> HIR conversion
    // For now, create an empty module
    let mut arena = AstArena::new();
    let builder = HirBuilder::new("main", &mut arena);
    Ok(builder.finish())
}
```

This needs to:

1. Iterate through `program.declarations`
2. For each function declaration:
   - Create HIR function signature
   - Convert body expressions to HIR instructions
   - Handle parameters and return types
3. For each class/enum:
   - Register types in type system
   - Convert methods to HIR functions
4. Build complete `HirModule` with all functions

Once this is implemented, we'll have a complete Haxe → Native compilation pipeline!

## Testing

### Unit Tests

Haxe-side tests verify JSON output:

```bash
cd reflaxe.zyntax/test
haxe test.hxml
# Verify JSON files match expected structure
```

### Integration Tests

End-to-end tests compile and run programs:

```bash
cd reflaxe.zyntax/test
./compile.sh
# Should compile and execute HelloWorld
```

### Benchmarks

Performance comparisons with other Haxe backends:

```bash
# Haxe/C++
haxe -cpp out -main Benchmark
time ./out/Benchmark

# Haxe/Zyntax
haxe -lib reflaxe.zyntax -D zyntax-output=out -main Benchmark
zyntax compile out/*.json -o bench
time ./bench
```

## Resources

- [Reflaxe Framework](https://github.com/SomeRanDev/reflaxe)
- [Haxe Manual](https://haxe.org/manual/)
- [TypedAST Documentation](../crates/typed_ast/README.md)
- [HIR Builder Guide](../crates/compiler/README.md)
- [Architecture Overview](ARCHITECTURE.md)
