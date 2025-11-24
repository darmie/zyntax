# Zig Implementation Migration Plan

## Overview

Migrate the Zig language frontend from direct Rust parser (`zyn_parser/zig_builder.rs`) to the declarative Zyn grammar format (`zig.zyn`) that works with the CLI's `--format zyn` option.

## Current State

### Two Parallel Implementations

1. **Direct Parser** (`crates/zyn_parser/src/zig_builder.rs`)
   - ~3,362 lines of hand-written Rust
   - Uses `pest` for parsing + manual AST construction
   - Tightly coupled to `zyntax_typed_ast` types
   - Well-tested but maintenance-heavy

2. **Declarative Grammar** (`crates/zyn_peg/grammars/zig.zyn`)
   - ~1,290 lines of grammar + Rust action blocks
   - PEG syntax with `-> Type { ... }` action blocks
   - Currently uses embedded Rust code (NOT JSON commands)
   - Not integrated with CLI runtime

### Target Architecture

```text
┌─────────────────────────────────────────────────────────────────┐
│  zig.zyn (Grammar + JSON Commands)                              │
│    ↓                                                            │
│  ZynPEG Compiler                                                │
│    ↓                                                            │
│  ZpegModule (pest_grammar + rule commands)                      │
│    ↓                                                            │
│  pest_vm (Runtime Parser) + CommandInterpreter                  │
│    ↓                                                            │
│  TypedAST JSON → TypedProgram → HIR → Native                    │
└─────────────────────────────────────────────────────────────────┘
```

## Gap Analysis

### Current `zig.zyn` Problems

The existing `zig.zyn` uses **Rust action blocks**, not JSON commands:

```zyn
// Current (Rust code - WON'T WORK with runtime)
const_decl = { "const" ~ identifier ~ (":" ~ type_expr)? ~ "=" ~ expr ~ ";" }
  -> TypedNode<TypedDeclaration> {
      typed_node(
          TypedDeclaration::Variable(TypedVariable {
              name: intern($2),
              ...
          }),
          Type::Never,
          span($1, $6),
      )
  }
```

Needs to become **JSON commands** like `calc.zyn`:

```zyn
// Target (JSON commands - WORKS with runtime)
const_decl = { "const" ~ identifier ~ (":" ~ type_expr)? ~ "=" ~ expr ~ ";" }
  -> TypedDeclaration {
      "commands": [
          { "define": "variable", "args": {
              "name": "$2",
              "mutability": "immutable",
              "initializer": "$5"
          }}
      ]
  }
```

### Naming Convention

Grammar files follow the `lang.zyn` pattern (e.g., `zig.zyn`, `calc.zyn`). The current `zig.zyn` will be updated in place - we don't create separate files.

### Missing Runtime Commands

The `CommandInterpreter` supports:

- `define` - Create AST nodes
- `get_child` - Get child by index/name
- `get_text` - Get matched text
- `parse_int` / `parse_float` - Parse literals
- `fold_binary` - Binary expression folding
- `map_children` - Iterate children
- `match_rule` - Conditional dispatch
- `store` / `load` - Variables
- `return` - Return value

But `TypedAstBuilder` host functions need extension for:

- `struct_decl` / `enum_decl` / `union_decl`
- `function_decl` with full params
- `type_expr` variants (pointer, optional, error_union)
- `statement` variants (if, while, for, defer, etc.)
- Field/variant collection

## Migration Phases

### Phase 1: Extend Runtime Host Functions

**Goal**: Add missing `define` handlers to `TypedAstBuilder`

**Files to modify**:

- `crates/zyn_peg/src/runtime.rs`

**New handlers needed**:

```rust
// In TypedAstBuilder::define_node()
match node_type {
    // Declarations
    "variable" => { /* const/var decl */ }
    "function" => { /* function decl */ }
    "struct" => { /* struct decl */ }
    "enum" => { /* enum decl */ }
    "union" => { /* union decl */ }

    // Statements
    "if" => { /* if statement */ }
    "while" => { /* while loop */ }
    "for" => { /* for loop */ }
    "return" => { /* return stmt */ }
    "break" | "continue" => { /* control flow */ }
    "defer" | "errdefer" => { /* defer stmts */ }
    "let" => { /* local variable */ }
    "expression_stmt" => { /* expr as stmt */ }

    // Expressions
    "binary" => { /* already exists via fold_binary */ }
    "unary" => { /* unary op */ }
    "call" => { /* function call */ }
    "field_access" => { /* obj.field */ }
    "index" => { /* arr[i] */ }
    "try" => { /* try expr */ }
    "lambda" => { /* closure */ }
    "struct_literal" => { /* Point { x: 1 } */ }
    "array_literal" => { /* [1, 2, 3] */ }

    // Literals (most exist)
    "int_literal" => { /* exists */ }
    "float_literal" => { /* exists */ }
    "string_literal" => { /* exists */ }
    "bool_literal" => { /* exists */ }
    "null_literal" => { /* add */ }

    // Types
    "primitive_type" => { /* i32, bool, etc */ }
    "pointer_type" => { /* *T */ }
    "optional_type" => { /* ?T */ }
    "error_union_type" => { /* !T */ }
    "array_type" => { /* [N]T */ }
    "named_type" => { /* MyStruct */ }

    // Patterns
    "wildcard_pattern" => { /* _ */ }
    "identifier_pattern" => { /* x, mut x */ }
    "literal_pattern" => { /* 42 */ }
}
```

### Phase 2: Convert Grammar Rules to JSON Commands

**Goal**: Rewrite `zig.zyn` action blocks from Rust to JSON

**Priority order** (start simple, build up):

1. **Literals** - Integer, float, string, bool, null
2. **Identifiers** - Variable references
3. **Unary expressions** - -x, !x, ~x
4. **Binary expressions** - Using `fold_binary`
5. **Postfix expressions** - call, field, index
6. **Statements** - let, return, expression
7. **Control flow** - if, while, for, break, continue
8. **Declarations** - const, var, fn
9. **Types** - primitives, pointers, optionals, arrays
10. **Complex** - struct, enum, union, patterns

**Example conversions**:

```zyn
// Before: Rust action block
integer_literal = @{ "-"? ~ ASCII_DIGIT+ }
  -> TypedNode<TypedExpression> {
      typed_node(
          TypedExpression::Literal(TypedLiteral::Integer(Self::parse_int(pair_str))),
          Type::Primitive(PrimitiveType::I32),
          span,
      )
  }

// After: JSON commands
integer_literal = @{ "-"? ~ ASCII_DIGIT+ }
  -> TypedExpression {
      "get_text": true,
      "parse_int": true,
      "define": "int_literal",
      "args": { "value": "$result" }
  }
```

```zyn
// Before: Rust
fn_decl = { "fn" ~ identifier ~ "(" ~ fn_params? ~ ")" ~ type_expr ~ block }
  -> TypedNode<TypedDeclaration> {
      typed_node(
          TypedDeclaration::Function(TypedFunction {
              name: intern($2),
              params: $4.unwrap_or_default(),
              return_type: $6,
              body: Some($7),
              ...
          }),
          Type::Never,
          span($1, $7),
      )
  }

// After: JSON
fn_decl = { "fn" ~ identifier ~ "(" ~ fn_params? ~ ")" ~ type_expr ~ block }
  -> TypedDeclaration {
      "commands": [
          { "define": "function", "args": {
              "name": "$2",
              "params": "$4",
              "return_type": "$6",
              "body": "$7"
          }}
      ]
  }
```

### Phase 3: Test Integration

**Goal**: Verify Zig code compiles via CLI

**Test progression**:

1. **Literal expressions**

   ```bash
   echo '42' > test.zig
   zyntax compile --source test.zig --grammar zig.zyn --format zyn --run
   ```

2. **Simple functions**

   ```zig
   fn main() i32 {
       return 42;
   }
   ```

3. **Variables and expressions**

   ```zig
   fn main() i32 {
       const x = 10;
       const y = 20;
       return x + y;
   }
   ```

4. **Control flow**

   ```zig
   fn main() i32 {
       var sum: i32 = 0;
       for (i in 0..10) {
           sum = sum + i;
       }
       return sum;
   }
   ```

5. **Structs and methods**

   ```zig
   const Point = struct {
       x: i32,
       y: i32,
   };
   ```

6. **Full E2E tests**
   - Port existing `zig_e2e_jit` tests
   - Compare output with direct parser

### Phase 4: Deprecate Direct Parser

**Goal**: Remove `zig_builder.rs` once grammar works

1. Add deprecation warnings to `zig_builder.rs`
2. Update documentation to prefer Zyn grammar
3. Run parallel tests comparing both paths
4. Remove `zig_builder.rs` when confident

## Implementation Tasks

### Task 1: Extend TypedAstBuilder Host Functions

- [ ] Add `define_variable()` - for const/var declarations
- [ ] Add `define_function()` - for function declarations
- [ ] Add `define_struct()` - for struct types
- [ ] Add `define_enum()` - for enum types
- [ ] Add `define_if()` - for if statements
- [ ] Add `define_while()` - for while loops
- [ ] Add `define_for()` - for for loops
- [ ] Add `define_unary()` - for unary expressions
- [ ] Add `define_call()` - for function calls
- [ ] Add `define_field_access()` - for field access
- [ ] Add `define_index()` - for array indexing
- [ ] Add `define_try()` - for try expressions
- [ ] Add type construction helpers

### Task 2: Update `zig.zyn` Grammar

- [ ] Convert literals (int, float, string, bool) to JSON commands
- [ ] Convert identifier references
- [ ] Convert binary expressions with fold_binary
- [ ] Convert unary expressions
- [ ] Convert function declarations
- [ ] Convert variable declarations
- [ ] Convert statements (if, while, for, return)
- [ ] Convert type expressions
- [ ] Convert struct/enum/union declarations

### Task 3: Test Suite

- [ ] Create `tests/zig_grammar_tests.rs`
- [ ] Test literals through CLI
- [ ] Test expressions through CLI
- [ ] Test functions through CLI
- [ ] Test control flow through CLI
- [ ] Port `zig_e2e_jit` tests

### Task 4: Documentation & Cleanup

- [ ] Update README Zig section
- [ ] Document Zyn grammar format
- [ ] Add migration guide for grammar authors
- [ ] Deprecate and remove `zig_builder.rs`

## Timeline Estimate

| Phase | Effort | Dependencies |
|-------|--------|--------------|
| Phase 1: Host Functions | Medium | None |
| Phase 2: Grammar Conversion | Large | Phase 1 |
| Phase 3: Testing | Medium | Phase 2 |
| Phase 4: Cleanup | Small | Phase 3 |

## Success Criteria

1. `zyntax compile --source test.zig --grammar zig.zyn --format zyn --run` works
2. All existing Zig E2E tests pass with grammar-based parser
3. Performance is comparable (within 2x of direct parser)
4. `zig_builder.rs` can be safely removed

## Notes

- Back up current `zig.zyn` before modifying (or use git)
- Can incrementally migrate rules (test one at a time)
- The `calc.zyn` example is the template for JSON command syntax
- Keep direct parser functional until migration is complete
