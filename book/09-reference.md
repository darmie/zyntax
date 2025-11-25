# Chapter 9: Reference

Complete reference for Zyn grammar commands and the TypedAST builder API.

## Command Reference

### Data Extraction Commands

#### get_text

Extracts the matched text as a string.

```zyn
identifier = @{ ASCII_ALPHA+ }
  -> String {
      "get_text": true
  }
```

**Result**: The matched source text as a string.

#### parse_int

Parses extracted text as an integer. Must be preceded by `get_text`.

```zyn
integer = @{ "-"? ~ ASCII_DIGIT+ }
  -> TypedExpression {
      "get_text": true,
      "parse_int": true,
      "define": "int_literal",
      "args": { "value": "$result" }
  }
```

**Result**: An integer value.

#### get_child

Gets a specific child node by index.

```zyn
expr = { inner }
  -> TypedExpression {
      "get_child": { "index": 0 }
  }
```

**Parameters**:
- `index`: Zero-based child index

#### get_all_children

Collects all children into a list.

```zyn
items = { item* }
  -> List {
      "get_all_children": true
  }
```

**Result**: Array of child nodes.

### AST Construction Commands

#### define

Calls a builder method to create an AST node.

```zyn
{ "define": "method_name", "args": { "arg1": "value", "arg2": "$1" } }
```

**Arguments**:
- Static values: `"string"`, `42`, `true`, `false`, `[]`
- Child references: `"$1"`, `"$2"`, etc.
- Previous result: `"$result"`

### Binary Expression Commands

#### fold_binary

Creates left-associative binary expression trees.

```zyn
addition = { term ~ (("+" | "-") ~ term)* }
  -> TypedExpression {
      "fold_binary": { "operand": "term", "operator": "+|-" }
  }
```

**Parameters**:
- `operand`: Name of the operand rule
- `operator`: Operator rules (pipe-separated)

## Define Methods Reference

### Literals

| Method | Arguments | Description |
|--------|-----------|-------------|
| `int_literal` | `value: i64` | Integer literal |
| `bool_literal` | `value: "true"\|"false"` | Boolean literal |
| `string_literal` | `value: string` | String literal |
| `char_literal` | `value: char` | Character literal |

### Expressions

| Method | Arguments | Description |
|--------|-----------|-------------|
| `variable` | `name: string` | Variable reference |
| `binary` | `op: string, left: expr, right: expr` | Binary operation |
| `unary` | `op: string, operand: expr` | Unary operation |
| `call` | `callee: expr, args: list` | Function call |
| `field_access` | `object: expr, field: string` | Field access |
| `index` | `object: expr, index: expr` | Index access |
| `struct_init` | `type_name: string, fields: list` | Struct literal |
| `struct_field_init` | `name: string, value: expr` | Field initializer |
| `array_literal` | `elements: list` | Array literal |
| `try` | `expr: expr` | Try expression |

### Statements

| Method | Arguments | Description |
|--------|-----------|-------------|
| `let_stmt` | `name, init?, is_const, type?` | Variable declaration |
| `return_stmt` | `value?: expr` | Return statement |
| `expression_stmt` | `expr: expr` | Expression statement |
| `if` | `condition, then_branch, else_branch?` | If statement |
| `while` | `condition, body` | While loop |
| `for` | `iterable, binding, body` | For loop |
| `assignment` | `target, value` | Assignment |
| `break` | | Break statement |
| `continue` | | Continue statement |
| `block` | `statements: list` | Statement block |

### Declarations

| Method | Arguments | Description |
|--------|-----------|-------------|
| `function` | `name, params, return_type, body` | Function declaration |
| `struct` | `name, fields` | Struct declaration |
| `enum` | `name, variants` | Enum declaration |
| `param` | `name, type` | Function parameter |
| `field` | `name, type` | Struct field |
| `variant` | `name` | Enum variant |
| `program` | `declarations: list` | Program root |

### Types

| Method | Arguments | Description |
|--------|-----------|-------------|
| `primitive_type` | `name: string` | Primitive type |
| `pointer_type` | `pointee: type` | Pointer type |
| `optional_type` | `inner: type` | Optional type |
| `array_type` | `size?, element: type` | Array type |
| `error_union_type` | `payload: type` | Error union type |

## Grammar Syntax Reference

### Rule Types

| Syntax | Description | Creates Node |
|--------|-------------|--------------|
| `rule = { ... }` | Normal rule | Yes |
| `rule = @{ ... }` | Atomic rule | Yes |
| `rule = _{ ... }` | Silent rule | No |

### Operators

| Operator | Name | Description |
|----------|------|-------------|
| `~` | Sequence | Match in order |
| `\|` | Choice | First match wins |
| `*` | Zero or more | Repeat 0+ times |
| `+` | One or more | Repeat 1+ times |
| `?` | Optional | Match 0 or 1 time |
| `!` | Not | Succeed if doesn't match |
| `&` | And | Succeed if matches (no consume) |

### Built-in Rules

| Rule | Matches |
|------|---------|
| `SOI` | Start of input |
| `EOI` | End of input |
| `ANY` | Any character |
| `ASCII` | ASCII character (0x00-0x7F) |
| `ASCII_DIGIT` | 0-9 |
| `ASCII_ALPHA` | a-z, A-Z |
| `ASCII_ALPHANUMERIC` | a-z, A-Z, 0-9 |
| `ASCII_HEX_DIGIT` | 0-9, a-f, A-F |
| `NEWLINE` | \n or \r\n |

### Special Rules

| Rule | Purpose |
|------|---------|
| `WHITESPACE` | Define whitespace handling |
| `COMMENT` | Define comment syntax |

## TypedAST Builder API

### Construction

```rust
use zyntax_typed_ast::TypedASTBuilder;

let mut builder = TypedASTBuilder::new();
```

### Type Helpers

```rust
builder.i32_type()     // Type::Primitive(PrimitiveType::I32)
builder.i64_type()     // Type::Primitive(PrimitiveType::I64)
builder.bool_type()    // Type::Primitive(PrimitiveType::Bool)
builder.string_type()  // Type::Primitive(PrimitiveType::String)
builder.unit_type()    // Type::Primitive(PrimitiveType::Unit)
builder.char_type()    // Type::Primitive(PrimitiveType::Char)
builder.f32_type()     // Type::Primitive(PrimitiveType::F32)
builder.f64_type()     // Type::Primitive(PrimitiveType::F64)
```

### Span Helpers

```rust
builder.span(start, end)  // Create span from byte offsets
builder.dummy_span()      // Create (0, 0) span for testing
```

### Expression Builders

```rust
// Literals
builder.int_literal(42, span)
builder.string_literal("hello", span)
builder.bool_literal(true, span)
builder.char_literal('x', span)
builder.unit_literal(span)

// References
builder.variable("name", ty, span)

// Operations
builder.binary(BinaryOp::Add, left, right, result_ty, span)
builder.unary(UnaryOp::Minus, operand, result_ty, span)

// Access
builder.field_access(object, "field", field_ty, span)
builder.index(object, index_expr, element_ty, span)

// Calls
builder.call_positional(callee, args_vec, return_ty, span)
builder.call_named(callee, named_args_vec, return_ty, span)

// Composite
builder.struct_literal("Name", fields_vec, struct_ty, span)
builder.array_literal(elements_vec, array_ty, span)
builder.tuple(elements_vec, tuple_ty, span)
builder.lambda(params_vec, body, lambda_ty, span)

// Special
builder.cast(expr, target_ty, span)
builder.try_expr(expr, result_ty, span)
builder.await_expr(expr, result_ty, span)
builder.reference(expr, mutability, ptr_ty, span)
builder.dereference(expr, deref_ty, span)
```

### Statement Builders

```rust
// Declarations
builder.let_statement("name", ty, mutability, init_opt, span)

// Control flow
builder.if_statement(condition, then_block, else_opt, span)
builder.while_loop(condition, body, span)
builder.for_loop("binding", iterable, body, span)
builder.loop_stmt(body, span)

// Jumps
builder.return_stmt(value, span)
builder.return_void(span)
builder.break_stmt(span)
builder.break_with_value(value, span)
builder.continue_stmt(span)

// Other
builder.expression_statement(expr, span)
builder.throw_stmt(exception, span)
builder.block(statements_vec, span)
```

### Pattern Builders

```rust
builder.struct_pattern("Name", fields_vec, span)
builder.enum_pattern("Enum", "Variant", fields_vec, span)
builder.array_pattern(patterns_vec, span)
builder.slice_pattern(prefix, middle_opt, suffix, span)
```

## CLI Reference

### Compile Command

```bash
zyntax compile [OPTIONS] [INPUT]...

Options:
  -s, --source <SOURCE>    Source file (with --grammar)
  -g, --grammar <GRAMMAR>  ZynPEG grammar file (.zyn)
  -o, --output <OUTPUT>    Output file path
  -b, --backend <BACKEND>  Backend (jit, llvm) [default: jit]
  -v, --verbose            Verbose output
  -O, --opt-level <LEVEL>  Optimization (0-3) [default: 2]
  -f, --format <FORMAT>    Input format (auto, typed-ast, hir-bytecode, zyn)
      --run                Run immediately (JIT only)
```

### Examples

```bash
# Compile and run Zig file
zyntax compile --grammar zig.zyn --source hello.zig --run

# Compile to object file
zyntax compile --grammar zig.zyn --source main.zig -o main.o

# Verbose compilation
zyntax compile --grammar zig.zyn --source test.zig -v --run

# Use LLVM backend
zyntax compile --grammar zig.zyn --source test.zig -b llvm -o test.o
```

## Error Messages

### Grammar Errors

| Error | Cause | Solution |
|-------|-------|----------|
| "Rule not found" | Reference to undefined rule | Define the rule or check spelling |
| "Left recursion detected" | `a = { a ~ ... }` | Rewrite using repetition |
| "Invalid command" | Unknown JSON command | Check command spelling |
| "Missing argument" | Required arg not provided | Add the missing argument |

### Runtime Errors

| Error | Cause | Solution |
|-------|-------|----------|
| "Cannot access fields on non-struct type" | Field access on wrong type | Check object type |
| "Unknown variant" | Enum variant not found | Check variant name |
| "Type mismatch" | Incompatible types | Check expression types |

## Best Practices

### Grammar Organization

1. **Group related rules** - Keep declarations, statements, expressions separate
2. **Order choices correctly** - Longer/more specific first
3. **Use meaningful names** - `if_stmt` not `rule1`
4. **Comment complex rules** - Explain non-obvious patterns

### Performance

1. **Avoid excessive backtracking** - Use negative lookahead
2. **Make atomic rules atomic** - Use `@{ }` for tokens
3. **Keep grammar focused** - Don't over-generalize

### Debugging

1. **Test incrementally** - Add rules one at a time
2. **Use verbose mode** - `--verbose` shows parse tree
3. **Check child indices** - Print children to verify order
4. **Start simple** - Get basic cases working first
