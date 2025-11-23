# ZynPEG Grammar Specification

**Version**: 2.0 (JSON Action Blocks)
**Last Updated**: November 23, 2025

## Overview

ZynPEG is a parser generator that extends pest PEG syntax with TypedAST action blocks. Version 2.0 introduces JSON-based action mappings that enable runtime interpretation without requiring Rust compilation.

## Architecture

```text
┌─────────────────────────────────────────────────────────────┐
│                    Compile Time                             │
├─────────────────────────────────────────────────────────────┤
│  .zyn grammar  →  ZynPEG Compiler  →  .zpeg module          │
│                                                              │
│  .zpeg contains:                                            │
│  - pest grammar (string)                                    │
│  - Rule command mappings (JSON)                             │
└─────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────┐
│                    Runtime                                  │
├─────────────────────────────────────────────────────────────┤
│  source.lang + .zpeg  →  pest_vm  →  Host Functions  →  AST │
│                                                              │
│  No Rust compilation required!                              │
└─────────────────────────────────────────────────────────────┘
```

## Grammar File Structure

A `.zyn` grammar file consists of:

1. **Directives** - Metadata and configuration
2. **Rule Definitions** - PEG patterns with action blocks

```zyn
// Directives (optional)
@language { ... }
@imports { ... }
@context { ... }

// Rule definitions
rule_name = { pattern }
  -> ReturnType {
      // JSON action block
  }
```

## Directives

### @language

Defines metadata about the language being parsed.

```zyn
@language {
    name: "MyLang",
    version: "1.0",
    file_extensions: [".mylang", ".ml"],
    entry_point: "main",
}
```

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `name` | string | Yes | Language name |
| `version` | string | Yes | Language version |
| `file_extensions` | string[] | Yes | File extensions to associate |
| `entry_point` | string | No | Function name to call with `--run` flag |

The `entry_point` field tells the CLI which function to execute when using the `--run` flag. The grammar is responsible for creating this function in the TypedAST.

### @imports (Legacy)

Used for Rust code generation mode. Ignored in JSON runtime mode.

```zyn
@imports {
    use zyntax_typed_ast::*;
}
```

### @context (Legacy)

Defines context variables for Rust code generation. Ignored in JSON runtime mode.

```zyn
@context {
    arena: &mut AstArena,
    type_registry: &mut TypeRegistry,
}
```

## Rule Definitions

### Basic Syntax

```zyn
rule_name = modifier? { pattern } action_block?
```

### Rule Modifiers

| Modifier | Name | Description |
|----------|------|-------------|
| `@` | Atomic | No whitespace skipping inside |
| `_` | Silent | Rule doesn't appear in parse tree |
| `$` | Compound | Atomic but keeps inner structure |
| `!` | Non-atomic | Force whitespace skipping |

### PEG Pattern Syntax

ZynPEG uses pest-compatible PEG syntax:

```zyn
// Sequence
a ~ b ~ c

// Choice
a | b | c

// Repetition
a*      // Zero or more
a+      // One or more
a?      // Optional
a{n}    // Exactly n times
a{n,}   // At least n times
a{n,m}  // Between n and m times

// Predicates
&a      // Positive lookahead
!a      // Negative lookahead

// Grouping
(a ~ b) | c

// Literals
"keyword"
'a'..'z'

// Built-in rules
SOI             // Start of input
EOI             // End of input
ANY             // Any character
ASCII_DIGIT     // 0-9
ASCII_ALPHA     // a-z, A-Z
ASCII_ALPHANUMERIC
ASCII_HEX_DIGIT
NEWLINE
WHITESPACE      // Auto-skipped between tokens
COMMENT         // Auto-skipped between tokens
```

## Action Blocks (JSON Format)

Action blocks define how to construct TypedAST nodes from parsed content.

### Basic Structure

Single command:

```zyn
rule_name = { pattern }
  -> ReturnType {
      "define": "int_literal",
      "args": { "value": "$result" }
  }
```

Multiple sequential commands using the `commands` wrapper:

```zyn
rule_name = { pattern }
  -> ReturnType {
      "commands": [
          { "define": "return_stmt", "args": { "value": "$1" }, "store": "ret" },
          { "define": "function", "args": { "name": "main", "params": [], "body": "$ret" } },
          { "define": "program", "args": { "declarations": ["$result"] } }
      ]
  }
```

### Command Reference

#### `define` - Create AST Node

Defines an AST node by invoking a host function with named arguments. Optionally stores the result.

```json
{
    "define": "node_type",
    "args": {
        "param_name": "$1",
        "other_param": "literal_value"
    },
    "store": "variable_name"
}
```

Using named arguments makes grammars self-documenting. The `store` field saves the result to a named variable (accessible as `$variable_name`).

**Available node types:**

| Node Type | Arguments | Description |
|-----------|-----------|-------------|
| `int_literal` | value: i64 | Create integer literal |
| `float_literal` | value: f64 | Create float literal |
| `string_literal` | value: string | Create string literal |
| `bool_literal` | value: bool | Create boolean literal |
| `identifier` | name: string | Create identifier expression |
| `binary_op` | op, left, right | Create binary operation |
| `unary_op` | op, operand | Create unary operation |
| `call_expr` | callee, args[] | Create function call expression |
| `index` | array, index | Create array index |
| `field_access` | object, field | Create field access |
| `var_decl` | name, type?, init?, is_const | Create variable declaration |
| `assignment` | target, value | Create assignment |
| `return_stmt` | value? | Create return statement |
| `if` | condition, then, else? | Create if statement |
| `while` | condition, body | Create while loop |
| `for` | iterator, iterable, body | Create for loop |
| `block` | statements[] | Create block |
| `function` | name, params[], body | Create function declaration |
| `param` | name, type | Create parameter |
| `primitive_type` | name | Create primitive type |
| `pointer_type` | pointee | Create pointer type |
| `array_type` | element, size? | Create array type |
| `named_type` | name | Create named type |
| `program` | declarations[] | Create program with declarations |

#### `get_child` - Access Child Node

Gets a child node by index (0-based) or name.

```json
{
    "get_child": { "index": 0 }
}
// or
{
    "get_child": { "name": "expr" }
}
```

#### `get_text` - Get Matched Text

Gets the text content of the current match.

```json
{
    "get_text": true
}
```

#### `parse_int` / `parse_float`

Parses the current text as a number.

```json
{
    "parse_int": true
}
```

#### `fold_binary` - Left-Associative Binary Operations

Folds a sequence of binary operations left-to-right. The operator rules must return their text (using `"get_text": true`).

```json
{
    "fold_binary": {
        "operand": "term",
        "operator": "add_op|sub_op"
    }
}
```

The `operator` field specifies which rules are operators (pipe-separated). The runtime extracts the operator text (e.g., "+", "-") and creates `binary_op` nodes automatically.

Example with a typical expression grammar:

```zyn
expr = { term ~ ((add_op | sub_op) ~ term)* }
  -> TypedExpression {
      "fold_binary": { "operand": "term", "operator": "add_op|sub_op" }
  }

add_op = { "+" }
  -> String { "get_text": true }

sub_op = { "-" }
  -> String { "get_text": true }
```

#### `map_children` - Process All Children

Processes all children matching a rule.

```json
{
    "map_children": {
        "rule": "statement",
        "commands": [...]
    }
}
```

#### `match_rule` - Conditional Processing

Branches based on which rule matched.

```json
{
    "match_rule": {
        "number": [...],
        "identifier": [...],
        "call_expr": [...]
    }
}
```

#### `store` / `load` - Temporary Variables

Stores and retrieves intermediate values.

```json
{
    "store": { "name": "left_operand" }
}
// later
{
    "load": { "name": "left_operand" }
}
```

### Capture References

Use `$N` syntax to reference captured children (1-based indexing):

| Reference | Meaning |
|-----------|---------|
| `"$1"` | First captured child |
| `"$2"` | Second captured child |
| `"$name"` | Named variable (from `store` field) |
| `"$text"` | Text content of current match |
| `"$result"` | Result of previous command in sequence |

The `$result` variable is automatically set after each command in a `commands` array, allowing you to chain operations:

```zyn
program = { SOI ~ expr ~ EOI }
  -> TypedProgram {
      "commands": [
          { "define": "return_stmt", "args": { "value": "$1" }, "store": "ret" },
          // $ret now contains the return statement
          { "define": "function", "args": { "name": "main", "params": [], "body": "$ret" } },
          // $result now contains the function
          { "define": "program", "args": { "declarations": ["$result"] } }
      ]
  }
```

## Complete Examples

### Calculator Grammar

This example demonstrates a complete calculator grammar with proper entry point declaration and the `commands` wrapper for sequential AST construction.

```zyn
@language {
    name: "Calculator",
    version: "1.0",
    file_extensions: [".calc"],
    entry_point: "main",
}

// Build a program with a main function that returns the expression
// Sequential commands: return_stmt -> function -> program
program = { SOI ~ expr ~ EOI }
  -> TypedProgram {
      "commands": [
          { "define": "return_stmt", "args": { "value": "$1" }, "store": "ret" },
          { "define": "function", "args": { "name": "main", "params": [], "body": "$ret" } },
          { "define": "program", "args": { "declarations": ["$result"] } }
      ]
  }

expr = { term ~ ((add_op | sub_op) ~ term)* }
  -> TypedExpression {
      "fold_binary": { "operand": "term", "operator": "add_op|sub_op" }
  }

term = { factor ~ ((mul_op | div_op) ~ factor)* }
  -> TypedExpression {
      "fold_binary": { "operand": "factor", "operator": "mul_op|div_op" }
  }

// factor: pass through the child node directly
factor = { number | paren_expr }
  -> TypedExpression {
      "get_child": { "index": 0 }
  }

// Silent rule - parentheses don't appear in parse tree
paren_expr = _{ "(" ~ expr ~ ")" }

number = @{ ASCII_DIGIT+ }
  -> TypedExpression {
      "get_text": true,
      "parse_int": true,
      "define": "int_literal",
      "args": { "value": "$result" }
  }

// Operator rules return their text for fold_binary
add_op = { "+" }
  -> String { "get_text": true }

sub_op = { "-" }
  -> String { "get_text": true }

mul_op = { "*" }
  -> String { "get_text": true }

div_op = { "/" }
  -> String { "get_text": true }

WHITESPACE = _{ " " | "\t" | "\n" | "\r" }
```

### Function Declaration

```zyn
fn_decl = {
    "fn" ~ identifier ~ "(" ~ fn_params? ~ ")" ~ type_expr ~ block
}
  -> TypedDeclaration {
      "define": "function",
      "args": {
          "name": "$1",
          "params": "$2",
          "return_type": "$3",
          "body": "$4"
      }
  }

fn_params = { fn_param ~ ("," ~ fn_param)* }
  -> Vec<TypedParameter> {
      "map_children": {
          "rule": "fn_param",
          "commands": [{ "get_child": { "index": 0 } }]
      }
  }

fn_param = { identifier ~ ":" ~ type_expr }
  -> TypedParameter {
      "define": "param",
      "args": { "name": "$1", "type": "$2" }
  }
```

## ZPEG Module Format

The compiled `.zpeg` format is a JSON file:

```json
{
    "metadata": {
        "name": "Calculator",
        "version": "1.0",
        "file_extensions": [".calc"],
        "entry_point": "main",
        "zpeg_version": "0.1.0"
    },
    "pest_grammar": "program = { SOI ~ expr ~ EOI }\n...",
    "rules": {
        "program": {
            "return_type": "TypedProgram",
            "commands": [
                { "type": "define", "node": "return_stmt", "args": { "value": "$1" }, "store": "ret" },
                { "type": "define", "node": "function", "args": { "name": "main", "params": [], "body": "$ret" } },
                { "type": "define", "node": "program", "args": { "declarations": ["$result"] } }
            ]
        },
        "number": {
            "return_type": "TypedExpression",
            "commands": [
                { "type": "get_text" },
                { "type": "parse_int" },
                { "type": "define", "node": "int_literal", "args": { "value": "$result" } }
            ]
        }
    }
}
```

## CLI Usage

```bash
# Compile and run with grammar
zyntax compile --source my_code.lang --grammar my_lang.zyn --format zyn --run

# Verbose mode shows compilation steps
zyntax compile -v --source code.zig --grammar zig.zyn --format zyn -o output
```

## Migration from Legacy Format

### Legacy (Rust Code)

```zyn
number = @{ ASCII_DIGIT+ }
  -> TypedExpression {
      expr: IntLiteral($1.parse()),
      ty: Type::I32,
      span: $1.span,
  }
```

### New (JSON Commands)

```zyn
number = @{ ASCII_DIGIT+ }
  -> TypedExpression {
      "get_text": true,
      "parse_int": true,
      "define": "int_literal",
      "args": { "value": "$result" }
  }
```

## Best Practices

1. **Use atomic rules for tokens**: Mark lexical rules with `@` to prevent whitespace interference
2. **Use silent rules for delimiters**: Mark punctuation rules with `_` to keep parse trees clean
3. **Use fold_binary for operators**: Handles left-associativity correctly
4. **Name your captures**: Use named rules for clarity in complex patterns
5. **Test incrementally**: Build and test grammar rules one at a time

## Error Handling

Common errors and solutions:

| Error | Cause | Solution |
|-------|-------|----------|
| "Failed to parse pest grammar" | Invalid PEG syntax | Check pattern syntax |
| "Unknown host function" | Misspelled function name | Check function reference |
| "Invalid child reference" | `$N` out of bounds | Verify pattern captures |
| "Type mismatch" | Wrong argument types | Check host function signature |

## See Also

- [GRAMMAR_CONVENTIONS.md](../crates/zyn_peg/GRAMMAR_CONVENTIONS.md) - Style guide for .zyn files
- [ZYN_PARSER_IMPLEMENTATION.md](ZYN_PARSER_IMPLEMENTATION.md) - Implementation details
- [BYTECODE_FORMAT_SPEC.md](BYTECODE_FORMAT_SPEC.md) - HIR bytecode format
