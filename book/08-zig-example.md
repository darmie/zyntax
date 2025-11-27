# Chapter 8: Complete Example - Zig Grammar

This chapter walks through the complete Zig grammar implementation (`zig.zyn`), explaining each section and the design decisions behind it.

## Overview

The Zig grammar supports:
- Functions with typed parameters
- Structs and enums
- Control flow (if, while, for)
- Expressions with proper operator precedence
- Type expressions (pointers, optionals, arrays)

## File Structure

```zyn
// 1. Language metadata
@language { ... }

// 2. Program structure
program = { ... }
declarations = { ... }
declaration = { ... }

// 3. Type declarations
struct_decl = { ... }
enum_decl = { ... }

// 4. Function declarations
fn_decl = { ... }

// 5. Statements
statement = { ... }
if_stmt = { ... }
while_stmt = { ... }
// ...

// 6. Expressions (by precedence)
expr = { ... }
logical_or = { ... }
// ... down to atoms

// 7. Literals and identifiers
integer_literal = { ... }
identifier = { ... }

// 8. Operators
add_op = { ... }
// ...

// 9. Whitespace/comments
WHITESPACE = { ... }
COMMENT = { ... }
```

## Language Metadata

```zyn
@language {
    name: "Zig",
    version: "0.11",
    file_extensions: [".zig"],
    entry_point: "main",
}
```

This metadata tells the compiler:
- The language name for error messages
- Which file extensions to recognize
- Which function to execute for `--run`

## Program Structure

### The Entry Point

```zyn
program = { SOI ~ declarations ~ EOI }
  -> TypedProgram {
      "get_child": { "index": 0 }
  }
```

This matches the entire file (`SOI` to `EOI`) and extracts the declarations.

### Collecting Declarations

```zyn
declarations = { declaration* }
  -> TypedProgram {
      "get_all_children": true,
      "define": "program",
      "args": { "declarations": "$result" }
  }
```

Key pattern: `get_all_children` collects all `declaration` matches into a list, then `define: "program"` creates the root node.

### Declaration Dispatch

```zyn
declaration = { struct_decl | enum_decl | fn_decl | const_decl | var_decl }
  -> TypedDeclaration {
      "get_child": { "index": 0 }
  }
```

Order matters! More specific rules should come first if there's ambiguity.

## Struct Declarations

```zyn
struct_decl = { "const" ~ identifier ~ "=" ~ "struct" ~ "{" ~ struct_fields? ~ "}" ~ ";" }
  -> TypedDeclaration {
      "commands": [
          { "define": "struct", "args": {
              "name": "$1",
              "fields": "$2"
          }}
      ]
  }
```

Example input:
```zig
const Point = struct {
    x: i32,
    y: i32,
};
```

Child mapping:
- `$1` = `identifier` → "Point"
- `$2` = `struct_fields?` → list of fields (or null)

### Struct Fields

```zyn
struct_fields = { struct_field ~ ("," ~ struct_field)* ~ ","? }
  -> List {
      "get_all_children": true
  }

struct_field = { identifier ~ ":" ~ type_expr }
  -> TypedField {
      "commands": [
          { "define": "field", "args": { "name": "$1", "type": "$2" } }
      ]
  }
```

Pattern: Optional trailing comma (`","?`) is common in modern languages.

## Enum Declarations

```zyn
enum_decl = { "const" ~ identifier ~ "=" ~ "enum" ~ "{" ~ enum_variants? ~ "}" ~ ";" }
  -> TypedDeclaration {
      "commands": [
          { "define": "enum", "args": {
              "name": "$1",
              "variants": "$2"
          }}
      ]
  }

enum_variants = { enum_variant ~ ("," ~ enum_variant)* ~ ","? }
  -> List {
      "get_all_children": true
  }

enum_variant = { identifier }
  -> TypedVariant {
      "get_text": true,
      "define": "variant",
      "args": { "name": "$result" }
  }
```

Example:
```zig
const Color = enum {
    Red,
    Green,
    Blue,
};
```

The runtime assigns discriminant values (0, 1, 2) automatically.

## Function Declarations

### The Split Pattern

```zyn
fn_decl = { fn_decl_with_params | fn_decl_no_params }
  -> TypedDeclaration {
      "get_child": { "index": 0 }
  }
```

**Why split?** PEG doesn't produce placeholder children for missing optionals. With a single rule like:

```zyn
// PROBLEMATIC
fn_decl = { "fn" ~ identifier ~ "(" ~ fn_params? ~ ")" ~ type_expr ~ block }
```

If params are missing, `$3` would be `type_expr`, not `block`. By splitting, each variant has predictable child indices.

### With Parameters

```zyn
fn_decl_with_params = { "fn" ~ identifier ~ "(" ~ fn_params ~ ")" ~ type_expr ~ block }
  -> TypedDeclaration {
      "commands": [
          { "define": "function", "args": {
              "name": "$1",
              "params": "$2",
              "return_type": "$3",
              "body": "$4"
          }}
      ]
  }
```

### Without Parameters

```zyn
fn_decl_no_params = { "fn" ~ identifier ~ "(" ~ ")" ~ type_expr ~ block }
  -> TypedDeclaration {
      "commands": [
          { "define": "function", "args": {
              "name": "$1",
              "params": [],
              "return_type": "$2",
              "body": "$3"
          }}
      ]
  }
```

Note: `"params": []` provides an empty list literal.

### Parameters

```zyn
fn_params = { fn_param ~ ("," ~ fn_param)* }
  -> List {
      "get_child": { "index": 0 }
  }

fn_param = { identifier ~ ":" ~ type_expr }
  -> TypedParameter {
      "commands": [
          { "define": "param", "args": { "name": "$1", "type": "$2" } }
      ]
  }
```

## Statements

### Statement Dispatch

```zyn
statement = { if_stmt | while_stmt | for_stmt | return_stmt | break_stmt |
              continue_stmt | local_const | local_var | assign_stmt | expr_stmt }
  -> TypedStatement {
      "get_child": { "index": 0 }
  }
```

Order consideration: `if_stmt` before `expr_stmt` because an identifier `if_something` could otherwise match.

### If Statement (Split Pattern Again)

```zyn
if_stmt = { if_else | if_only }
  -> TypedStatement { "get_child": { "index": 0 } }

if_only = { "if" ~ "(" ~ expr ~ ")" ~ block }
  -> TypedStatement {
      "commands": [
          { "define": "if", "args": {
              "condition": "$1",
              "then_branch": "$2"
          }}
      ]
  }

if_else = { "if" ~ "(" ~ expr ~ ")" ~ block ~ "else" ~ block }
  -> TypedStatement {
      "commands": [
          { "define": "if", "args": {
              "condition": "$1",
              "then_branch": "$2",
              "else_branch": "$3"
          }}
      ]
  }
```

**Important**: `if_else` must come before `if_only` in the choice, otherwise `if_only` would always match first!

### While Loop

```zyn
while_stmt = { "while" ~ "(" ~ expr ~ ")" ~ block }
  -> TypedStatement {
      "commands": [
          { "define": "while", "args": {
              "condition": "$1",
              "body": "$2"
          }}
      ]
  }
```

### For Loop (Zig Style)

```zyn
for_stmt = { "for" ~ "(" ~ expr ~ ")" ~ "|" ~ identifier ~ "|" ~ block }
  -> TypedStatement {
      "commands": [
          { "define": "for", "args": {
              "iterable": "$1",
              "binding": "$2",
              "body": "$3"
          }}
      ]
  }
```

Zig's for loop: `for (slice) |item| { ... }`

### Return Statement

```zyn
return_stmt = { "return" ~ expr? ~ ";" }
  -> TypedStatement {
      "commands": [
          { "define": "return_stmt", "args": { "value": "$1" } }
      ]
  }
```

`$1` will be null if `expr?` doesn't match.

### Block

```zyn
block = { "{" ~ statement* ~ "}" }
  -> TypedBlock {
      "get_all_children": true,
      "define": "block",
      "args": { "statements": "$result" }
  }
```

## Switch Expressions

Zig supports switch expressions for pattern matching against values. The grammar handles multiple pattern types including literals, ranges, struct patterns, tagged union patterns, and error patterns.

### Basic Structure

```zyn
switch_expr = { "switch" ~ "(" ~ expr ~ ")" ~ "{" ~ switch_cases? ~ "}" }
  -> TypedExpression {
      "commands": [
          { "define": "switch_expr", "args": {
              "scrutinee": "$1",
              "cases": "$2"
          }}
      ]
  }

switch_cases = { switch_case ~ ("," ~ switch_case)* ~ ","? }
  -> List {
      "get_all_children": true
  }
```

The `switch_expr` command creates a switch expression node with:

- `scrutinee`: The expression being matched against
- `cases`: List of case arms

### Switch Cases

Each case has a pattern and a body:

```zyn
// Value case: pattern => expr
switch_case_value = { switch_pattern ~ "=>" ~ expr }
  -> TypedExpression {
      "commands": [
          { "define": "switch_case", "args": {
              "pattern": { "get_child": { "index": 0 } },
              "body": { "get_child": { "index": 1 } }
          }}
      ]
  }

// Else case: else => expr
switch_case_else = { "else" ~ "=>" ~ expr }
  -> TypedExpression {
      "commands": [
          { "define": "switch_case", "args": {
              "pattern": { "define": "wildcard_pattern" },
              "body": "$1"
          }}
      ]
  }
```

Note the `else` case uses an inline `{ "define": "wildcard_pattern" }` to create the pattern directly in the args.

### Pattern Types

#### Literal Patterns

Match exact values:

```zyn
switch_literal_pattern = { integer_literal | string_literal }
  -> TypedExpression {
      "commands": [
          { "define": "literal_pattern", "args": { "value": "$1" } }
      ]
  }
```

Example:
```zig
const result = switch (x) {
    1 => 10,
    2 => 20,
    else => 0,
};
```

#### Wildcard Pattern

Match anything (used for `_` or `else`):

```zyn
switch_wildcard_pattern = { "_" }
  -> TypedExpression {
      "commands": [
          { "define": "wildcard_pattern" }
      ]
  }
```

#### Range Patterns

Match values within a range:

```zyn
switch_range_pattern = { integer_literal ~ ".." ~ integer_literal }
  -> TypedExpression {
      "commands": [
          { "define": "range_pattern", "args": {
              "start": { "define": "literal_pattern", "args": { "value": "$1" } },
              "end": { "define": "literal_pattern", "args": { "value": "$2" } },
              "inclusive": false
          }}
      ]
  }
```

Example:

```zig
const result = switch (x) {
    0..9 => "single digit",
    10..99 => "double digit",
    else => "other",
};
```

#### Tagged Union Patterns

Match enum or tagged union variants (Zig uses `.variant` syntax):

```zyn
switch_tagged_union_pattern = { "." ~ identifier }
  -> TypedExpression {
      "commands": [
          { "define": "enum_pattern", "args": {
              "name": "",
              "variant": { "text": "$1" },
              "fields": []
          }}
      ]
  }
```

Example:

```zig
const result = switch (optional_value) {
    .some => 100,
    .none => 0,
};
```

**Note**: Tagged union patterns against non-enum types (like integers) gracefully return `false` in the backend, allowing the else case to match.

#### Struct Patterns

Match struct values by field:

```zyn
switch_struct_pattern = { identifier ~ "{" ~ struct_field_patterns? ~ "}" }
  -> TypedExpression {
      "commands": [
          { "define": "struct_pattern", "args": {
              "name": { "text": "$1" },
              "fields": "$2"
          }}
      ]
  }

switch_struct_field_pattern = { "." ~ identifier ~ ("=" ~ switch_pattern)? }
  -> TypedExpression {
      "commands": [
          { "define": "field_pattern", "args": {
              "name": { "text": "$1" },
              "pattern": "$2"
          }}
      ]
  }
```

Example:

```zig
const result = switch (point) {
    Point{ .x = 0, .y = 0 } => "origin",
    Point{ .x = 0 } => "on y-axis",
    else => "elsewhere",
};
```

#### Error Patterns

Match error values from error unions:

```zyn
switch_error_pattern = { "error" ~ "." ~ identifier }
  -> TypedExpression {
      "commands": [
          { "define": "error_pattern", "args": {
              "name": { "text": "$1" }
          }}
      ]
  }
```

Example:

```zig
const result = switch (error_union) {
    error.OutOfMemory => "memory error",
    error.NotFound => "not found",
    else => "success or other",
};
```

#### Pointer Patterns

Match through pointer dereference:

```zyn
switch_pointer_pattern = { "*" ~ switch_pattern }
  -> TypedExpression {
      "commands": [
          { "define": "pointer_pattern", "args": {
              "inner": "$1",
              "mutable": false
          }}
      ]
  }
```

### Testing Switch Expressions

```zig
// Literal pattern match
fn main() i32 {
    const x = 2;
    const result = switch (x) {
        1 => 10,
        2 => 20,
        else => 0,
    };
    return result;
}
```

```bash
# Returns: 20
```

```zig
// Else case (no match)
fn main() i32 {
    const x = 99;
    const result = switch (x) {
        1 => 10,
        2 => 20,
        else => 0,
    };
    return result;
}
```

```bash
# Returns: 0
```

## Expressions

### The Precedence Chain

Operators are handled by a chain from lowest to highest precedence:

```zyn
expr = { logical_or }
  -> TypedExpression { "get_child": { "index": 0 } }

// Lowest: OR
logical_or = { logical_and ~ (or_op ~ logical_and)* }
  -> TypedExpression {
      "fold_binary": { "operand": "logical_and", "operator": "or_op" }
  }

// AND
logical_and = { comparison ~ (and_op ~ comparison)* }
  -> TypedExpression {
      "fold_binary": { "operand": "comparison", "operator": "and_op" }
  }

// Comparison
comparison = { addition ~ ((eq_op | neq_op | lte_op | gte_op | lt_op | gt_op) ~ addition)* }
  -> TypedExpression {
      "fold_binary": { "operand": "addition", "operator": "eq_op|neq_op|lte_op|gte_op|lt_op|gt_op" }
  }

// Addition/Subtraction
addition = { multiplication ~ ((add_op | sub_op) ~ multiplication)* }
  -> TypedExpression {
      "fold_binary": { "operand": "multiplication", "operator": "add_op|sub_op" }
  }

// Multiplication/Division
multiplication = { unary ~ ((mul_op | div_op) ~ unary)* }
  -> TypedExpression {
      "fold_binary": { "operand": "unary", "operator": "mul_op|div_op" }
  }

// Unary (highest before atoms)
unary = { unary_with_op | primary }
  -> TypedExpression { "get_child": { "index": 0 } }

unary_with_op = { unary_op ~ primary }
  -> TypedExpression {
      "commands": [
          { "define": "unary", "args": { "op": "$1", "operand": "$2" } }
      ]
  }
```

### The `fold_binary` Pattern

For `1 + 2 + 3`:

1. Parse: `[term(1), +, term(2), +, term(3)]`
2. Start: `result = 1`
3. Fold: `result = binary(+, 1, 2)` → `(1+2)`
4. Fold: `result = binary(+, (1+2), 3)` → `((1+2)+3)`

This creates left-associative trees automatically.

### Postfix Expressions

```zyn
postfix_expr = { call_expr | field_expr | index_expr | atom }
  -> TypedExpression { "get_child": { "index": 0 } }

// Function call
call_expr = { atom ~ "(" ~ call_args? ~ ")" }
  -> TypedExpression {
      "commands": [
          { "define": "call", "args": { "callee": "$1", "args": "$2" } }
      ]
  }

// Field access
field_expr = { atom ~ "." ~ identifier }
  -> TypedExpression {
      "commands": [
          { "define": "field_access", "args": { "object": "$1", "field": "$2" } }
      ]
  }

// Index access
index_expr = { atom ~ "[" ~ expr ~ "]" }
  -> TypedExpression {
      "commands": [
          { "define": "index", "args": { "object": "$1", "index": "$2" } }
      ]
  }
```

### Atoms (Highest Precedence)

```zyn
atom = { try_expr | struct_init | array_literal | bool_literal |
         string_literal | integer_literal | identifier_expr | paren_expr }
  -> TypedExpression { "get_child": { "index": 0 } }
```

Order matters: `struct_init` (starts with identifier) before `identifier_expr`.

### Struct Initialization

```zyn
struct_init = { identifier ~ "{" ~ struct_init_fields? ~ "}" }
  -> TypedExpression {
      "commands": [
          { "define": "struct_init", "args": { "type_name": "$1", "fields": "$2" } }
      ]
  }

struct_init_fields = { struct_init_field ~ ("," ~ struct_init_field)* ~ ","? }
  -> List { "get_all_children": true }

struct_init_field = { "." ~ identifier ~ "=" ~ expr }
  -> TypedExpression {
      "commands": [
          { "define": "struct_field_init", "args": { "name": "$1", "value": "$2" } }
      ]
  }
```

Example: `Point{ .x = 10, .y = 20 }`

### Parenthesized Expressions

```zyn
paren_expr = _{ "(" ~ expr ~ ")" }
```

Silent rule (`_{ }`) - matches but doesn't create a node. The inner `expr` passes through directly.

## Type Expressions

```zyn
type_expr = { pointer_type | optional_type | error_union_type | array_type |
              primitive_type | identifier }
  -> Type { "get_child": { "index": 0 } }

pointer_type = { "*" ~ "const"? ~ type_expr }
  -> Type {
      "commands": [
          { "define": "pointer_type", "args": { "pointee": "$1" } }
      ]
  }

optional_type = { "?" ~ type_expr }
  -> Type {
      "commands": [
          { "define": "optional_type", "args": { "inner": "$1" } }
      ]
  }

array_type = { "[" ~ integer_literal? ~ "]" ~ type_expr }
  -> Type {
      "commands": [
          { "define": "array_type", "args": { "size": "$1", "element": "$2" } }
      ]
  }

primitive_type = { "i8" | "i16" | "i32" | "i64" | "u8" | "u16" | "u32" | "u64" |
                   "f32" | "f64" | "bool" | "void" }
  -> Type {
      "get_text": true,
      "define": "primitive_type",
      "args": { "name": "$result" }
  }
```

## Identifiers and Keywords

### Keyword Protection

```zyn
keyword = @{
    ("struct" | "enum" | "fn" | "const" | "var" | "if" | "else" | "while" | "for" |
     "return" | "break" | "continue" | "try" | "and" | "or" | "true" | "false" |
     "i8" | "i16" | "i32" | "i64" | "u8" | "u16" | "u32" | "u64" | "f32" | "f64" |
     "bool" | "void")
    ~ !(ASCII_ALPHANUMERIC | "_")
}

identifier = @{ !keyword ~ (ASCII_ALPHA | "_") ~ (ASCII_ALPHANUMERIC | "_")* }
  -> String { "get_text": true }
```

**Key patterns**:
1. `~ !(ASCII_ALPHANUMERIC | "_")` ensures "iffy" doesn't match as "if" + "fy"
2. `!keyword` prevents identifiers from being keywords
3. Both are atomic (`@{ }`) for proper token handling

## Operators

Each operator is a separate rule for use with `fold_binary`:

```zyn
// Must check longer operators first
lte_op = { "<=" } -> String { "get_text": true }
gte_op = { ">=" } -> String { "get_text": true }
eq_op = { "==" } -> String { "get_text": true }
neq_op = { "!=" } -> String { "get_text": true }
lt_op = { "<" } -> String { "get_text": true }
gt_op = { ">" } -> String { "get_text": true }

add_op = { "+" } -> String { "get_text": true }
sub_op = { "-" } -> String { "get_text": true }
mul_op = { "*" } -> String { "get_text": true }
div_op = { "/" } -> String { "get_text": true }

and_op = { "and" } -> String { "get_text": true }
or_op = { "or" } -> String { "get_text": true }

unary_op = { "-" | "!" } -> String { "get_text": true }
```

## Whitespace and Comments

```zyn
WHITESPACE = _{ " " | "\t" | "\n" | "\r" }
COMMENT = _{ "//" ~ (!"\n" ~ ANY)* ~ "\n"? }
```

Both are silent (`_{ }`) - they match but don't appear in the parse tree.

## Testing the Grammar

### Simple Function

```zig
fn main() i32 {
    return 42;
}
```

```bash
zyntax compile --grammar zig.zyn --source test.zig --run
# Output: result: main() returned: 42
```

### Struct with Field Access

```zig
const Point = struct {
    x: i32,
    y: i32,
};

fn main() i32 {
    const p = Point{ .x = 10, .y = 20 };
    return p.x;
}
```

```bash
# Returns: 10
```

### Enum Variants

```zig
const Color = enum {
    Red,
    Green,
    Blue,
};

fn main() i32 {
    return Color.Green;
}
```

```bash
# Returns: 1 (Green's discriminant)
```

### Arithmetic Expression

```zig
fn main() i32 {
    return 2 + 3 * 4;
}
```

```bash
# Returns: 14 (multiplication before addition)
```

## Common Patterns Summary

| Pattern | Use Case |
|---------|----------|
| Split rules | Handle optional children with predictable indices |
| `fold_binary` | Left-associative binary operators |
| `get_all_children` | Collect repetitions into lists |
| Keyword protection | Prevent identifiers matching keywords |
| Silent rules | Grouping without AST nodes |
| Atomic rules | Token-level matching |

## Next Steps

- [Chapter 9](./09-reference.md): Complete command and API reference
- Try modifying the grammar to add new features!
