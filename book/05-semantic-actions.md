# Chapter 5: Semantic Actions

Semantic actions are JSON command blocks that describe how to build TypedAST nodes from parsed syntax. This chapter covers all available commands and patterns.

## Basic Structure

Every grammar rule can have a semantic action:

```zyn
rule_name = { pattern }
  -> ResultType {
      // JSON commands
  }
```

The `ResultType` indicates what kind of AST node the rule produces:
- `TypedExpression` - Expressions (literals, operations, calls)
- `TypedStatement` - Statements (if, while, return)
- `TypedDeclaration` - Top-level declarations (functions, structs)
- `TypedBlock` - Statement blocks
- `TypedParameter` - Function parameters
- `TypedField` - Struct fields
- `TypedVariant` - Enum variants
- `Type` - Type expressions
- `List` - Collect multiple children
- `String` - Extract text

## Child References

Commands can reference parsed children using `$N` syntax:

```zyn
// Children are numbered by their position in the parse tree
// $1 = first non-silent child, $2 = second, etc.

binary_expr = { left ~ "+" ~ right }
  -> TypedExpression {
      "commands": [
          { "define": "binary", "args": { "left": "$1", "op": "+", "right": "$2" } }
      ]
  }
```

Special references:
- `$result` - The result of previous commands (like `get_text`)
- `$1`, `$2`, ... - Child nodes by position

## Core Commands

### get_text

Extracts the matched text as a string:

```zyn
identifier = @{ ASCII_ALPHA ~ (ASCII_ALPHANUMERIC | "_")* }
  -> String {
      "get_text": true
  }
```

### parse_int

Parses extracted text as an integer:

```zyn
integer_literal = @{ "-"? ~ ASCII_DIGIT+ }
  -> TypedExpression {
      "get_text": true,
      "parse_int": true,
      "define": "int_literal",
      "args": { "value": "$result" }
  }
```

### get_child

Gets a specific child by index:

```zyn
// Get the first child (index 0)
expr = { inner_expr }
  -> TypedExpression {
      "get_child": { "index": 0 }
  }
```

### get_all_children

Collects all children into a list:

```zyn
statements = { statement* }
  -> List {
      "get_all_children": true
  }
```

### define

Calls an AST builder method with arguments:

```zyn
return_stmt = { "return" ~ expr? ~ ";" }
  -> TypedStatement {
      "commands": [
          { "define": "return_stmt", "args": { "value": "$1" } }
      ]
  }
```

## The `define` Command

The `define` command is the primary way to create AST nodes. It calls a method on the TypedAstBuilder.

### Syntax

```json
{ "define": "method_name", "args": { "arg1": "value1", "arg2": "$1" } }
```

### Available Methods

#### Literals

| Method | Arguments | Creates |
|--------|-----------|---------|
| `int_literal` | `value` | Integer literal |
| `bool_literal` | `value` | Boolean literal |
| `string_literal` | `value` | String literal |
| `char_literal` | `value` | Character literal |

```zyn
bool_literal = { "true" | "false" }
  -> TypedExpression {
      "get_text": true,
      "define": "bool_literal",
      "args": { "value": "$result" }
  }
```

#### Variables and Access

| Method | Arguments | Creates |
|--------|-----------|---------|
| `variable` | `name` | Variable reference |
| `field_access` | `object`, `field` | Field access (obj.field) |
| `index` | `object`, `index` | Index access (arr[i]) |

```zyn
field_expr = { atom ~ "." ~ identifier }
  -> TypedExpression {
      "commands": [
          { "define": "field_access", "args": { "object": "$1", "field": "$2" } }
      ]
  }
```

#### Operations

| Method | Arguments | Creates |
|--------|-----------|---------|
| `binary` | `op`, `left`, `right` | Binary operation |
| `unary` | `op`, `operand` | Unary operation |
| `call` | `callee`, `args` | Function call |

```zyn
unary_expr = { unary_op ~ primary }
  -> TypedExpression {
      "commands": [
          { "define": "unary", "args": { "op": "$1", "operand": "$2" } }
      ]
  }
```

#### Statements

| Method | Arguments | Creates |
|--------|-----------|---------|
| `let_stmt` | `name`, `init`, `is_const`, `type`? | Variable declaration |
| `return_stmt` | `value`? | Return statement |
| `if` | `condition`, `then_branch`, `else_branch`? | If statement |
| `while` | `condition`, `body` | While loop |
| `for` | `iterable`, `binding`, `body` | For loop |
| `expression_stmt` | `expr` | Expression statement |
| `assignment` | `target`, `value` | Assignment |
| `break` | | Break statement |
| `continue` | | Continue statement |

```zyn
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

#### Blocks and Programs

| Method | Arguments | Creates |
|--------|-----------|---------|
| `block` | `statements` | Statement block |
| `program` | `declarations` | Program root |

```zyn
block = { "{" ~ statement* ~ "}" }
  -> TypedBlock {
      "get_all_children": true,
      "define": "block",
      "args": { "statements": "$result" }
  }
```

#### Declarations

| Method | Arguments | Creates |
|--------|-----------|---------|
| `function` | `name`, `params`, `return_type`, `body` | Function declaration |
| `struct` | `name`, `fields` | Struct declaration |
| `enum` | `name`, `variants` | Enum declaration |
| `param` | `name`, `type` | Function parameter |
| `field` | `name`, `type` | Struct field |
| `variant` | `name` | Enum variant |

```zyn
fn_decl = { "fn" ~ identifier ~ "(" ~ fn_params ~ ")" ~ type_expr ~ block }
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

#### Structs and Enums

| Method | Arguments | Creates |
|--------|-----------|---------|
| `struct_init` | `type_name`, `fields` | Struct literal |
| `struct_field_init` | `name`, `value` | Field initializer |
| `array_literal` | `elements` | Array literal |

```zyn
struct_init = { identifier ~ "{" ~ struct_init_fields? ~ "}" }
  -> TypedExpression {
      "commands": [
          { "define": "struct_init", "args": { "type_name": "$1", "fields": "$2" } }
      ]
  }
```

#### Types

| Method | Arguments | Creates |
|--------|-----------|---------|
| `primitive_type` | `name` | Primitive type (i32, bool, etc.) |
| `pointer_type` | `pointee` | Pointer type (*T) |
| `optional_type` | `inner` | Optional type (?T) |
| `array_type` | `size`, `element` | Array type ([N]T) |

```zyn
primitive_type = { "i32" | "i64" | "bool" | "void" }
  -> Type {
      "get_text": true,
      "define": "primitive_type",
      "args": { "name": "$result" }
  }
```

#### Pattern Matching

These commands create pattern nodes for switch expressions and pattern matching:

| Method | Arguments | Creates |
|--------|-----------|---------|
| `literal_pattern` | `value` | Match a literal value (int, string) |
| `wildcard_pattern` | | Match anything (`_` or `else`) |
| `range_pattern` | `start`, `end`, `inclusive` | Match a range (`1..10`) |
| `identifier_pattern` | `name` | Bind matched value to variable |
| `struct_pattern` | `name`, `fields` | Match struct with field patterns |
| `field_pattern` | `name`, `pattern`? | Match a struct field |
| `enum_pattern` | `name`, `variant`, `fields` | Match enum/tagged union variant |
| `array_pattern` | `elements` | Match array elements |
| `pointer_pattern` | `inner`, `mutable` | Match pointer dereference |
| `error_pattern` | `name` | Match error value (`error.OutOfMemory`) |
| `switch_expr` | `scrutinee`, `cases` | Switch expression |
| `switch_case` | `pattern`, `body` | Single switch case arm |

```zyn
// Literal pattern: match exact value
switch_literal_pattern = { integer_literal }
  -> TypedExpression {
      "commands": [
          { "define": "literal_pattern", "args": { "value": "$1" } }
      ]
  }

// Wildcard pattern: match anything
switch_wildcard_pattern = { "_" }
  -> TypedExpression {
      "commands": [
          { "define": "wildcard_pattern" }
      ]
  }

// Range pattern: match value in range
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

// Struct pattern: match struct fields
switch_struct_pattern = { identifier ~ "{" ~ struct_field_patterns? ~ "}" }
  -> TypedExpression {
      "commands": [
          { "define": "struct_pattern", "args": {
              "name": { "text": "$1" },
              "fields": "$2"
          }}
      ]
  }

// Tagged union pattern: .some, .none
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

// Error pattern: error.OutOfMemory
switch_error_pattern = { "error" ~ "." ~ identifier }
  -> TypedExpression {
      "commands": [
          { "define": "error_pattern", "args": {
              "name": { "text": "$1" }
          }}
      ]
  }

// Pointer pattern: *x
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

## The `fold_binary` Command

This special command builds left-associative binary expression trees from repetition patterns.

### Problem It Solves

Given input `1 + 2 + 3`, we want:
```
    +
   / \
  +   3
 / \
1   2
```

Not:
```
  +
 / \
1   +
   / \
  2   3
```

### Usage

```zyn
addition = { term ~ ((add_op | sub_op) ~ term)* }
  -> TypedExpression {
      "fold_binary": { "operand": "term", "operator": "add_op|sub_op" }
  }
```

Parameters:
- `operand`: Name of the operand rule
- `operator`: Operator rules (pipe-separated for multiple)

### How It Works

For input `1 + 2 - 3`:

1. Parse produces: `[term(1), add_op(+), term(2), sub_op(-), term(3)]`
2. Fold starts with first term: `result = 1`
3. Process pairs: `result = binary(+, result, 2)` → `(1 + 2)`
4. Continue: `result = binary(-, result, 3)` → `((1 + 2) - 3)`

### Multiple Operators

Handle different operators at the same precedence level:

```zyn
comparison = { addition ~ ((eq_op | neq_op | lt_op | gt_op) ~ addition)* }
  -> TypedExpression {
      "fold_binary": { "operand": "addition", "operator": "eq_op|neq_op|lt_op|gt_op" }
  }
```

## Command Sequences

Use `commands` array to execute multiple commands in sequence:

```zyn
typed_var_decl = { "const" ~ identifier ~ ":" ~ type_expr ~ "=" ~ expr ~ ";" }
  -> TypedDeclaration {
      "commands": [
          { "define": "let_stmt", "args": {
              "name": "$1",
              "type": "$2",
              "init": "$3",
              "is_const": true
          }}
      ]
  }
```

## The `fold_left_ops` Command

This command handles left-associative binary expressions where operators are interleaved with operands in the child list.

### Difference from `fold_binary`

While `fold_binary` expects named rules for operands and operators, `fold_left_ops` works with children in a flat pattern: `[operand, operator, operand, operator, operand, ...]`

### Usage

```zyn
// Multiplication with operators in child list
multiplicative_expr = { unary_expr ~ (multiplicative_op ~ unary_expr)* }
  -> TypedExpression {
      "get_all_children": true,
      "fold_left_ops": true
  }

multiplicative_op = { "*" | "/" | "%" }
  -> String {
      "get_text": true
  }
```

### How It Works

For input `2 * 3 / 4`:

1. Parse produces: `[unary(2), "*", unary(3), "/", unary(4)]`
2. Start with first operand: `result = 2`
3. Process pairs: `result = binary(*, result, 3)` → `(2 * 3)`
4. Continue: `result = binary(/, result, 4)` → `((2 * 3) / 4)`

### Important Notes

- Requires `get_all_children: true` before `fold_left_ops`
- Operators should be extracted as strings (use `get_text: true`)
- Automatically unwraps nested single-element lists

## The `apply_unary` Command

This command handles optional unary prefix operators.

### Problem It Solves

When parsing unary expressions like `-x` or `!flag`, the unary operator is optional:
- `-42` has a unary prefix
- `42` has no unary prefix

Without `apply_unary`, you'd need to split into separate rules.

### Usage

```zyn
// Unary operators: -, !
unary_expr = { unary_op? ~ postfix_expr }
  -> TypedExpression {
      "get_all_children": true,
      "apply_unary": true
  }

unary_op = { "-" | "!" }
  -> String {
      "get_text": true
  }
```

### How It Works

1. Collects all children: `[operator?, operand]`
2. If operator present: creates `unary(op, operand)`
3. If no operator: passes through the operand unchanged
4. Unwraps nested single-element lists from expression cascading

## The `fold_left` Command

This command provides custom left-folding behavior for special operators like the pipe operator.

### Usage

```zyn
// Pipe operator: x |> f(args) |> g()
// Transforms: a |> f(b) into f(a, b)
pipe_expr = { or_expr ~ ("|>" ~ pipe_call)* }
  -> TypedExpression {
      "get_all_children": true,
      "fold_left": {
          "op": "pipe",
          "transform": "prepend_arg"
      }
  }

pipe_call = { identifier ~ "(" ~ call_args? ~ ")" }
  -> TypedExpression {
      "commands": [
          { "define": "pipe_target", "args": {
              "callee": { "define": "variable", "args": { "name": "$1" } },
              "args": "$2"
          }}
      ]
  }
```

### Parameters

- `op`: The operator name ("pipe", "||", "&&", etc.)
- `transform` (optional): Special transformation to apply
  - `"prepend_arg"`: For pipe operator, prepends left-hand side to function args

### How It Works for Pipe Operator

For input `data |> filter(pred) |> map(fn)`:

1. Parse produces: `[data, pipe_call(filter, [pred]), pipe_call(map, [fn])]`
2. Start with `result = data`
3. Transform: `filter(result, pred)` (prepends result to args)
4. Transform: `map(result2, fn)` (prepends to args)

Result is equivalent to: `map(filter(data, pred), fn)`

### Logical Operators

For simple left-folding (like `||` and `&&`):

```zyn
or_expr = { and_expr ~ ("||" ~ and_expr)* }
  -> TypedExpression {
      "get_all_children": true,
      "fold_left": { "op": "||" }
  }
```

## The `fold_postfix` Command

This command handles postfix operations like function calls, array indexing, and member access.

### Problem It Solves

Postfix expressions chain operations: `obj.field[0](arg)` needs to fold left-to-right into nested AST nodes.

### Usage

```zyn
// Postfix: function calls, indexing, member access
postfix_expr = { primary_expr ~ postfix_op* }
  -> TypedExpression {
      "get_all_children": true,
      "fold_postfix": true
  }

postfix_op = { call_op | index_op | member_op }
  -> TypedExpression {
      "get_child": { "index": 0 }
  }

// Function call: f(args)
call_op = { "(" ~ call_args? ~ ")" }
  -> TypedExpression {
      "get_child": { "index": 0 },
      "define": "call_args",
      "args": { "args": "$result" }
  }

// Indexing: x[i]
index_op = { "[" ~ expr ~ "]" }
  -> TypedExpression {
      "define": "index",
      "args": { "index": "$1" }
  }

// Member access: x.field
member_op = { "." ~ identifier }
  -> TypedExpression {
      "define": "member",
      "args": { "field": "$1" }
  }
```

### How It Works

For input `arr[0].length`:

1. Parse produces: `[primary(arr), index_op(0), member_op(length)]`
2. Start with `result = arr`
3. Apply index: `result = index(result, 0)`
4. Apply member: `result = field_access(result, "length")`

### Postfix Operation Types

Each postfix operation is wrapped with a marker so `fold_postfix` knows how to apply it:

| Marker | Creates |
|--------|---------|
| `call_args` | Function call with args |
| `index` | Array/map index access |
| `member` | Field/property access |

## Type Inference Markers

When defining variables in dynamically-typed or type-inferred languages, use these special type markers:

### The `infer_type` Define

```zyn
let_stmt = { "let" ~ identifier ~ "=" ~ expr }
  -> TypedStatement {
      "commands": [
          { "define": "let_stmt", "args": {
              "name": "$1",
              "type": { "define": "infer_type" },
              "init": "$2",
              "is_const": false
          }}
      ]
  }
```

The `infer_type` define returns a special null marker indicating the type should be inferred from the initializer expression by the type checker.

### Aliases

These aliases also work for type inference:
- `"define": "auto"` - C++ style auto type
- `"define": "var"` - TypeScript/C# style var

## Handling Optional Children

When a child might be absent, the builder handles null/missing gracefully:

```zyn
// expr? produces None if missing
return_stmt = { "return" ~ expr? ~ ";" }
  -> TypedStatement {
      "commands": [
          { "define": "return_stmt", "args": { "value": "$1" } }
      ]
  }
```

For complex cases, use separate rules:

```zyn
// Split into variants to avoid indexing issues
if_stmt = { if_else | if_only }

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

## Passthrough Rules

Sometimes a rule just selects between alternatives without transforming:

```zyn
// Just pass through the matched child
expr = { logical_or }
  -> TypedExpression {
      "get_child": { "index": 0 }
  }

statement = { if_stmt | while_stmt | return_stmt | expr_stmt }
  -> TypedStatement {
      "get_child": { "index": 0 }
  }
```

## Complete Example

Here's a complete expression grammar with proper operator precedence:

```zyn
expr = { logical_or }
  -> TypedExpression { "get_child": { "index": 0 } }

logical_or = { logical_and ~ (or_op ~ logical_and)* }
  -> TypedExpression {
      "fold_binary": { "operand": "logical_and", "operator": "or_op" }
  }

logical_and = { comparison ~ (and_op ~ comparison)* }
  -> TypedExpression {
      "fold_binary": { "operand": "comparison", "operator": "and_op" }
  }

comparison = { addition ~ ((eq_op | neq_op | lt_op | gt_op) ~ addition)* }
  -> TypedExpression {
      "fold_binary": { "operand": "addition", "operator": "eq_op|neq_op|lt_op|gt_op" }
  }

addition = { multiplication ~ ((add_op | sub_op) ~ multiplication)* }
  -> TypedExpression {
      "fold_binary": { "operand": "multiplication", "operator": "add_op|sub_op" }
  }

multiplication = { unary ~ ((mul_op | div_op) ~ unary)* }
  -> TypedExpression {
      "fold_binary": { "operand": "unary", "operator": "mul_op|div_op" }
  }

unary = { unary_with_op | primary }
  -> TypedExpression { "get_child": { "index": 0 } }

unary_with_op = { unary_op ~ primary }
  -> TypedExpression {
      "commands": [
          { "define": "unary", "args": { "op": "$1", "operand": "$2" } }
      ]
  }

primary = { integer | identifier_expr | paren_expr }
  -> TypedExpression { "get_child": { "index": 0 } }

paren_expr = _{ "(" ~ expr ~ ")" }

integer = @{ ASCII_DIGIT+ }
  -> TypedExpression {
      "get_text": true,
      "parse_int": true,
      "define": "int_literal",
      "args": { "value": "$result" }
  }

identifier_expr = { identifier }
  -> TypedExpression {
      "get_text": true,
      "define": "variable",
      "args": { "name": "$result" }
  }

// Operators
and_op = { "and" } -> String { "get_text": true }
or_op = { "or" } -> String { "get_text": true }
eq_op = { "==" } -> String { "get_text": true }
neq_op = { "!=" } -> String { "get_text": true }
lt_op = { "<" } -> String { "get_text": true }
gt_op = { ">" } -> String { "get_text": true }
add_op = { "+" } -> String { "get_text": true }
sub_op = { "-" } -> String { "get_text": true }
mul_op = { "*" } -> String { "get_text": true }
div_op = { "/" } -> String { "get_text": true }
unary_op = { "-" | "!" } -> String { "get_text": true }
```

## Next Steps

- [Chapter 6](./06-typed-ast.md): Understand the TypedAST structure these commands create
- [Chapter 7](./07-typed-ast-builder.md): Use the builder API directly in Rust
- [Chapter 8](./08-zig-example.md): See all commands used in a complete grammar
