# Zyntax CLI Architecture

Clean, modular CLI architecture for the Zyntax compiler.

## Directory Structure

```
src/
├── main.rs                    # Entry point and error handling
├── cli.rs                     # CLI argument parsing (clap)
├── commands.rs                # Command execution logic
├── formats/                   # Input format handlers
│   ├── mod.rs                # Format detection and enum
│   ├── hir_bytecode.rs       # HIR bytecode (.zbc) loading
│   └── typed_ast_json.rs     # TypedAST JSON loading & conversion
└── backends/                  # Compilation backends
    ├── mod.rs                # Backend enum and dispatch
    ├── cranelift_jit.rs      # Cranelift JIT compilation
    └── llvm_aot.rs           # LLVM AOT compilation (TODO)
```

## Module Responsibilities

### main.rs (50 lines)
**Purpose:** Entry point with minimal logic

- Initialize logger
- Parse CLI arguments
- Dispatch to command handlers
- Handle top-level errors and exit codes

**No business logic** - just plumbing

### cli.rs (75 lines)
**Purpose:** CLI structure and argument definitions

- `Cli` struct with clap attributes
- `Commands` enum (Compile, Version)
- `CompileArgs` struct for type-safe argument passing
- Helper method to extract compile arguments

**Pure data structures** - no I/O or computation

### commands.rs (60 lines)
**Purpose:** Command execution orchestration

Functions:
- `compile()` - Orchestrates the compilation pipeline
  1. Validate inputs
  2. Detect format
  3. Load HIR module
  4. Parse backend
  5. Dispatch to backend
- `version()` - Display version info

**Orchestration only** - delegates to format and backend modules

### formats/mod.rs (85 lines)
**Purpose:** Input format detection and routing

- `InputFormat` enum (HirBytecode, TypedAst)
- `detect_format()` - Auto-detect or parse format string
- `auto_detect_format()` - Scan files/directories
- `scan_directory_for_format()` - Deep directory scanning

**Format abstraction** - no knowledge of specific formats

### formats/hir_bytecode.rs (65 lines)
**Purpose:** HIR bytecode loading

- `load()` - Main entry point
- `collect_bytecode_files()` - Find .zbc files
- Uses `zyntax_compiler::bytecode::deserialize_module`

**Single responsibility:** .zbc → HirModule

### formats/typed_ast_json.rs (110 lines)
**Purpose:** TypedAST JSON loading and HIR conversion

- `load()` - Main entry point
- `collect_json_files()` - Find .json files
- `parse_json_files()` - Deserialize JSON
- `merge_programs()` - Combine multiple files
- `typed_ast_to_hir()` - Convert to HIR (TODO: implement)

**Single responsibility:** .json → HirModule

### backends/mod.rs (40 lines)
**Purpose:** Backend abstraction

- `Backend` enum (CraneliftJit, LlvmAot)
- `Backend::from_str()` - Parse backend name
- `compile()` - Dispatch to appropriate backend

**Backend abstraction** - no compilation logic

### backends/cranelift_jit.rs (40 lines)
**Purpose:** Cranelift JIT compilation

- `compile_jit()` - JIT compilation with optional execution
- Uses `zyntax_compiler::cranelift_backend::CraneliftBackend`

**Single responsibility:** HIR → JIT native code

### backends/llvm_aot.rs (25 lines)
**Purpose:** LLVM AOT compilation (TODO)

- `compile_llvm()` - Stub for LLVM backend
- Will use `zyntax_compiler::llvm_backend`

**Single responsibility:** HIR → AOT object file

## Data Flow

```
┌─────────────┐
│   main.rs   │  Parse CLI args
└──────┬──────┘
       │
       ▼
┌─────────────┐
│ commands.rs │  Orchestrate compilation
└──────┬──────┘
       │
       ├──────────────────────┐
       │                      │
       ▼                      ▼
┌─────────────┐      ┌──────────────┐
│  formats/   │      │  backends/   │
│             │      │              │
│ - bytecode  │      │ - cranelift  │
│ - json      │      │ - llvm       │
└─────────────┘      └──────────────┘
```

## Design Principles

### 1. **Single Responsibility**
Each module has one clear purpose:
- `cli.rs` - argument parsing only
- `formats/` - input loading only
- `backends/` - compilation only

### 2. **No Circular Dependencies**
Dependency graph flows one direction:
```
main.rs
  ↓
commands.rs
  ↓
formats/ + backends/
```

### 3. **Easy Testing**
Each module can be tested independently:

```rust
// Test format detection
#[test]
fn test_auto_detect_zbc() {
    let inputs = vec![PathBuf::from("test.zbc")];
    assert_eq!(detect_format("auto", &inputs), Ok(InputFormat::HirBytecode));
}

// Test backend parsing
#[test]
fn test_backend_from_str() {
    assert_eq!(Backend::from_str("jit"), Ok(Backend::CraneliftJit));
}
```

### 4. **Clear Error Propagation**
All functions return `Result<T, Box<dyn Error>>` with descriptive messages:

```rust
// Good error context
Err(format!("Failed to read {}: {}", file.display(), e))

// Backend-specific errors
Err("LLVM backend not implemented".into())
```

### 5. **Verbose Output Control**
All I/O modules accept `verbose: bool` and use consistent formatting:

```rust
if verbose {
    println!("{} Loading bytecode from {}", "info:".blue(), path.display());
}
```

## Adding New Features

### Add a new input format (e.g., binary TypedAST)

1. Create `formats/typed_ast_binary.rs`
2. Add `TypedAstBinary` variant to `InputFormat` enum
3. Add detection logic to `detect_format()`
4. Update `commands::compile()` match statement

### Add a new backend (e.g., WebAssembly)

1. Create `backends/wasm.rs`
2. Add `Wasm` variant to `Backend` enum
3. Update `Backend::from_str()`
4. Update `backends::compile()` match statement

### Add a new command (e.g., analyze)

1. Add `Analyze { ... }` variant to `Commands` enum in `cli.rs`
2. Create `commands::analyze()` function
3. Update `main.rs` match statement

## File Size Guidelines

Keep modules focused:
- Main entry point: < 100 lines
- Command handlers: < 100 lines each
- Format loaders: < 150 lines each
- Backend wrappers: < 100 lines each

If a module exceeds these limits, consider splitting it further.

## Testing Strategy

### Unit Tests
- `cli.rs` - Argument parsing edge cases
- `formats/mod.rs` - Format detection logic
- `backends/mod.rs` - Backend string parsing

### Integration Tests
- End-to-end compilation with .zbc files
- End-to-end compilation with .json files
- Error handling for missing files
- Verbose output verification

### Example Test Structure

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_detect_format_zbc() {
        let inputs = vec![PathBuf::from("test.zbc")];
        let format = detect_format("auto", &inputs).unwrap();
        assert_eq!(format, InputFormat::HirBytecode);
    }

    #[test]
    fn test_detect_format_json() {
        let inputs = vec![PathBuf::from("test.json")];
        let format = detect_format("auto", &inputs).unwrap();
        assert_eq!(format, InputFormat::TypedAst);
    }

    #[test]
    fn test_detect_format_explicit() {
        let inputs = vec![PathBuf::from("anything")];
        let format = detect_format("typed-ast", &inputs).unwrap();
        assert_eq!(format, InputFormat::TypedAst);
    }
}
```

## Benefits of This Architecture

1. **Maintainability** - Each file has a single, clear purpose
2. **Testability** - Easy to write unit tests for each module
3. **Extensibility** - Adding features doesn't require changing core logic
4. **Readability** - New contributors can understand the structure quickly
5. **Debuggability** - Error messages include context about which module failed
6. **Performance** - No runtime overhead from the modular structure

## Migration Notes

The original 368-line `main.rs` has been split into 9 focused modules:

- **Before:** 1 file, 368 lines, mixed responsibilities
- **After:** 9 files, ~550 total lines (with docs), single responsibilities

Net increase in code is due to:
- Module boundaries and imports
- This architecture documentation
- Better error messages
- Type-safe argument passing

The tradeoff is worth it for improved maintainability.
