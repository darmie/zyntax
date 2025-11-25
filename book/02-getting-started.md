# Chapter 2: Getting Started

## Prerequisites

Before using Zyn, ensure you have:

- Rust toolchain (1.70+)
- The `zyntax` CLI tool

```bash
# Build zyntax from source
cargo build --release

# Verify installation
./target/release/zyntax --help
```

## Your First Grammar

Let's create a minimal calculator language that supports:
- Integer literals
- Addition and subtraction
- Parentheses for grouping

### Step 1: Create the Grammar File

Create `calc.zyn`:

```zyn
// Calculator Language Grammar
@language {
    name: "Calc",
    version: "1.0",
    file_extensions: [".calc"],
    entry_point: "main",
}

// Program structure
program = { SOI ~ expr ~ EOI }
  -> TypedProgram {
      "commands": [
          { "define": "program_expr", "args": { "expr": "$1" } }
      ]
  }

// Expression with addition/subtraction
expr = { term ~ ((add_op | sub_op) ~ term)* }
  -> TypedExpression {
      "fold_binary": { "operand": "term", "operator": "add_op|sub_op" }
  }

// Terms are atoms or parenthesized expressions
term = { integer | paren_expr }
  -> TypedExpression {
      "get_child": { "index": 0 }
  }

// Parenthesized expression (silent rule - doesn't create node)
paren_expr = _{ "(" ~ expr ~ ")" }

// Integer literal
integer = @{ ASCII_DIGIT+ }
  -> TypedExpression {
      "get_text": true,
      "parse_int": true,
      "define": "int_literal",
      "args": { "value": "$result" }
  }

// Operators
add_op = { "+" }
  -> String { "get_text": true }

sub_op = { "-" }
  -> String { "get_text": true }

// Whitespace handling
WHITESPACE = _{ " " | "\t" | "\n" }
```

### Step 2: Create a Test File

Create `test.calc`:

```
1 + 2 + 3
```

### Step 3: Compile and Run

```bash
zyntax compile --grammar calc.zyn --source test.calc --run
```

## Understanding the Grammar

### Language Metadata

```zyn
@language {
    name: "Calc",
    version: "1.0",
    file_extensions: [".calc"],
    entry_point: "main",
}
```

This block defines metadata about your language:
- `name`: Language identifier
- `version`: Grammar version
- `file_extensions`: Associated file types
- `entry_point`: The function to execute (for JIT compilation)

### Grammar Rules

Rules follow PEG syntax with semantic action blocks:

```zyn
rule_name = { pattern }
  -> ResultType {
      // JSON commands
  }
```

| Syntax | Meaning |
|--------|---------|
| `{ }` | Normal rule (creates parse node) |
| `@{ }` | Atomic rule (no whitespace handling) |
| `_{ }` | Silent rule (matches but creates no node) |

### Operators

| Operator | Meaning | Example |
|----------|---------|---------|
| `~` | Sequence | `a ~ b` matches a then b |
| `|` | Choice | `a | b` matches a or b |
| `*` | Zero or more | `a*` matches "", "a", "aa", ... |
| `+` | One or more | `a+` matches "a", "aa", ... |
| `?` | Optional | `a?` matches "" or "a" |
| `!` | Not predicate | `!a` succeeds if a fails |
| `&` | And predicate | `&a` succeeds if a matches (no consume) |

### Built-in Rules

| Rule | Matches |
|------|---------|
| `SOI` | Start of input |
| `EOI` | End of input |
| `ANY` | Any single character |
| `ASCII_DIGIT` | 0-9 |
| `ASCII_ALPHA` | a-z, A-Z |
| `ASCII_ALPHANUMERIC` | a-z, A-Z, 0-9 |
| `WHITESPACE` | Define whitespace handling |
| `COMMENT` | Define comment syntax |

## Semantic Actions

Each rule can have a semantic action block that describes how to build AST nodes:

```zyn
integer = @{ ASCII_DIGIT+ }
  -> TypedExpression {
      "get_text": true,      // Get matched text
      "parse_int": true,     // Parse as integer
      "define": "int_literal",
      "args": { "value": "$result" }
  }
```

The `->` arrow connects the grammar rule to its semantic action:
- `TypedExpression` is the expected result type
- The JSON block contains commands to execute

### Common Commands

| Command | Purpose |
|---------|---------|
| `get_text` | Extract matched text |
| `get_child` | Get a specific child node |
| `get_all_children` | Collect all child nodes |
| `parse_int` | Parse text as integer |
| `define` | Call an AST builder method |
| `fold_binary` | Build left-associative binary expressions |

## Project Structure

A typical Zyn project looks like:

```
my-language/
├── grammar/
│   └── mylang.zyn      # Grammar definition
├── examples/
│   ├── hello.mylang    # Example source files
│   └── test.mylang
└── tests/
    └── parser_tests.rs # Test cases
```

## Next Steps

Now that you have a working grammar:

1. [Chapter 4](./04-grammar-syntax.md): Learn advanced grammar patterns
2. [Chapter 5](./05-semantic-actions.md): Master semantic actions
3. [Chapter 8](./08-zig-example.md): Study a complete real-world example
