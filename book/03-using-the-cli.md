# Chapter 3: Using the Zyntax CLI

The `zyntax` command-line tool provides compilation, execution, and interactive testing capabilities for Zyn grammars.

## Installation

Build from source:

```bash
cd zyntax
cargo build --release

# The binary is at ./target/release/zyntax
# Optionally add to PATH:
export PATH="$PATH:$(pwd)/target/release"
```

Verify installation:

```bash
zyntax --help
zyntax version
```

## Commands Overview

| Command | Purpose |
|---------|---------|
| `compile` | Compile and optionally run source files |
| `repl` | Interactive Read-Eval-Print Loop |
| `version` | Display version information |

## The `compile` Command

### Basic Usage

Compile a source file using a Zyn grammar:

```bash
zyntax compile --grammar <grammar.zyn> --source <file> [OPTIONS]
```

### Compile and Run (JIT)

The most common workflow - JIT compile and execute immediately:

```bash
zyntax compile --grammar crates/zyn_peg/grammars/zig.zyn \
               --source examples/hello.zig \
               --jit
```

Output:
```
result: main() returned: 42
```

### Compilation Options

| Option | Short | Description | Default |
|--------|-------|-------------|---------|
| `--grammar` | `-g` | ZynPEG grammar file (.zyn) | Required |
| `--source` | `-s` | Source file to compile | Required |
| `--output` | `-o` | Output file path | None |
| `--backend` | `-b` | Backend: `cranelift` or `llvm` | `cranelift` |
| `--opt-level` | `-O` | Optimization: 0-3 | `2` |
| `--jit` | | JIT compile and execute immediately | Off |
| `--verbose` | `-v` | Show detailed output | Off |
| `--format` | `-f` | Input format (see below) | `auto` |

### Backends

Zyntax supports two compilation backends:

| Backend | Description | Use Case |
|---------|-------------|----------|
| `cranelift` | Fast compilation, good code quality | Development, JIT |
| `llvm` | Maximum optimization, slower compilation | Production, AOT |

### Input Formats

The `--format` option controls how input is interpreted:

| Format | Description |
|--------|-------------|
| `auto` | Auto-detect from file extension |
| `zyn` | ZynPEG grammar-based parsing |
| `typed-ast` | Pre-built TypedAST JSON |
| `hir-bytecode` | HIR bytecode (.zbc) |

### Examples

#### JIT Compile and Run

```bash
# Simple execution with Cranelift (fast)
zyntax compile -g zig.zyn -s test.zig --jit

# With verbose output
zyntax compile -g zig.zyn -s test.zig --jit -v

# JIT with LLVM backend (maximum optimization)
zyntax compile -g zig.zyn -s test.zig --backend llvm --jit
```

#### AOT Compile to Executable

```bash
# Compile to native executable with Cranelift
zyntax compile -g zig.zyn -s main.zig -o main

# Compile with LLVM for maximum optimization
zyntax compile -g zig.zyn -s main.zig --backend llvm -o main
```

#### Optimization Levels

```bash
# No optimization (fastest compile)
zyntax compile -g zig.zyn -s test.zig -O0 --jit

# Maximum optimization
zyntax compile -g zig.zyn -s test.zig -O3 --jit
```

#### Compile TypedAST JSON

If you have pre-built TypedAST:

```bash
zyntax compile program.json --jit
```

## The `repl` Command

The REPL (Read-Eval-Print Loop) provides interactive testing of your grammar.

### Starting the REPL

```bash
zyntax repl --grammar crates/zyn_peg/grammars/zig.zyn
```

### REPL Options

| Option | Short | Description | Default |
|--------|-------|-------------|---------|
| `--grammar` | `-g` | ZynPEG grammar file | Required |
| `--backend` | `-b` | Backend: `cranelift` or `llvm` | `cranelift` |
| `--opt-level` | `-O` | Optimization: 0-3 | `0` |
| `--verbose` | `-v` | Show parse details | Off |

### REPL Usage

Once started, you can enter expressions or statements:

```
Zyntax REPL - Zig grammar loaded
Type expressions to evaluate, or :help for commands

> 1 + 2
result: 3

> 10 * 5 + 2
result: 52

> :help
Available commands:
  :help     - Show this help
  :quit     - Exit the REPL
  :verbose  - Toggle verbose mode
  :clear    - Clear screen

> :quit
Goodbye!
```

### REPL Commands

| Command | Description |
|---------|-------------|
| `:help` | Show available commands |
| `:quit` or `:q` | Exit the REPL |
| `:verbose` | Toggle verbose output |
| `:clear` | Clear the screen |

### Testing Expressions

The REPL is ideal for testing grammar changes:

```
> 2 + 3 * 4
result: 14

> (2 + 3) * 4
result: 20

> -5 + 10
result: 5

> true and false
result: false
```

### Verbose Mode

Enable verbose mode to see parsing details:

```bash
zyntax repl -g zig.zyn -v
```

Or toggle within the REPL:

```
> :verbose
Verbose mode: ON

> 1 + 2
[Parse] expr -> logical_or -> ... -> addition
[AST] Binary(Add, Int(1), Int(2))
[Type] i32
result: 3
```

## Workflows

### Development Workflow

When developing a grammar:

1. **Edit grammar** (`mygrammar.zyn`)
2. **Test in REPL** for quick iteration
3. **Compile test files** to verify complete programs
4. **Use verbose mode** to debug issues

```bash
# Quick test cycle
zyntax repl -g mygrammar.zyn

# Test a complete program
zyntax compile -g mygrammar.zyn -s test.mylang --jit
```

### Testing Workflow

Create test files and run them:

```bash
# Create test file
cat > /tmp/test_arithmetic.zig << 'EOF'
fn main() i32 {
    return 2 + 3 * 4;
}
EOF

# Run and verify
zyntax compile -g zig.zyn -s /tmp/test_arithmetic.zig --jit
# Expected: result: main() returned: 14
```

### Batch Testing

Test multiple files:

```bash
#!/bin/bash
GRAMMAR="crates/zyn_peg/grammars/zig.zyn"

for file in tests/*.zig; do
    echo "Testing: $file"
    zyntax compile -g "$GRAMMAR" -s "$file" --jit
    echo "---"
done
```

### Debug Workflow

When something doesn't work:

```bash
# Step 1: Check parsing with verbose
zyntax compile -g mygrammar.zyn -s broken.mylang -v --jit

# Step 2: Simplify the input
echo "1 + 2" | zyntax repl -g mygrammar.zyn -v

# Step 3: Check specific rules
# (Add debug output to your grammar's semantic actions)
```

## Error Messages

### Parse Errors

```
error: Parse error at line 3, column 10
  |
3 |     return x +
  |              ^
  | expected expression after '+'
```

### Type Errors

```
error: Type mismatch at line 5, column 12
  |
5 |     const x: i32 = "hello";
  |              ^^^ expected i32, found string
```

### Runtime Errors

```
error: Division by zero at line 7
  |
7 |     return 10 / 0;
  |               ^^^ division by zero
```

## Tips and Best Practices

### 1. Start with the REPL

When developing a new grammar feature, test it in the REPL first:

```bash
zyntax repl -g mygrammar.zyn
> new_feature_syntax
```

### 2. Use Verbose Mode for Debugging

When parsing fails unexpectedly:

```bash
zyntax compile -g grammar.zyn -s file.src -v --jit
```

### 3. Create Minimal Test Cases

When reporting bugs or testing features:

```zig
// test_minimal.zig - Tests operator precedence
fn main() i32 {
    return 2 + 3 * 4;  // Should be 14, not 20
}
```

### 4. Keep Test Files Organized

```
project/
├── grammar/
│   └── mylang.zyn
├── tests/
│   ├── arithmetic.mylang
│   ├── functions.mylang
│   ├── structs.mylang
│   └── control_flow.mylang
└── examples/
    └── hello.mylang
```

### 5. Use Shell Scripts for Regression Testing

```bash
#!/bin/bash
# run_tests.sh

GRAMMAR="grammar/mylang.zyn"
PASS=0
FAIL=0

for test in tests/*.mylang; do
    expected="${test%.mylang}.expected"
    result=$(zyntax compile -g "$GRAMMAR" -s "$test" --jit 2>&1)

    if diff -q <(echo "$result") "$expected" > /dev/null; then
        echo "✓ $test"
        ((PASS++))
    else
        echo "✗ $test"
        ((FAIL++))
    fi
done

echo "Passed: $PASS, Failed: $FAIL"
```

## Environment Variables

| Variable | Description |
|----------|-------------|
| `RUST_LOG` | Control logging level (`error`, `warn`, `info`, `debug`) |
| `RUST_BACKTRACE` | Show backtraces on panic (`1` or `full`) |

```bash
# Debug logging
RUST_LOG=debug zyntax compile -g zig.zyn -s test.zig --jit

# Full backtrace on errors
RUST_BACKTRACE=1 zyntax compile -g zig.zyn -s test.zig --jit
```

## Next Steps

- [Chapter 4](./04-grammar-syntax.md): Learn the grammar syntax in detail
- [Chapter 8](./08-zig-example.md): See a complete grammar implementation
- [Chapter 9](./09-reference.md): Full CLI reference
