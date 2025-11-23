# ZynPEG Grammar Conventions

This document describes the conventions and best practices for writing ZynPEG grammar files (`.zyn`) that generate TypedAST-compatible parsers.

## Overview

ZynPEG extends pest PEG syntax with action blocks that generate TypedAST nodes directly during parsing. The generated code integrates with `zyntax_typed_ast` types for full compilation pipeline support.

## File Structure

A `.zyn` grammar file has these sections:

```
@language { ... }     // Language metadata
@imports { ... }      // Rust imports for generated code
@context { ... }      // Parser context fields
@type_helpers { ... } // Helper functions for actions

// Rule definitions
rule_name = { pattern } -> ReturnType { action_code }
```

## Action Block Conventions

### Use `all_children` for Child Access

**DO NOT** use `$1`, `$2` placeholder syntax for complex rules. The generator's placeholder transformation is incomplete and can produce incorrect code.

**DO** use explicit `all_children` iteration:

```zyn
// CORRECT: Explicit child collection
fn_params = { fn_param ~ ("," ~ fn_param)* }
  -> Vec<TypedParameter> {
      all_children
          .iter()
          .filter(|p| p.as_rule() == Rule::fn_param)
          .filter_map(|p| self.build_fn_param(p.clone()).ok())
          .collect()
  }

// INCORRECT: Placeholder syntax (may only get first child)
fn_params = { fn_param ~ ("," ~ fn_param)* }
  -> Vec<TypedParameter> {
      let mut params = vec![$1];
      params.extend($2);  // $2 won't work correctly
      params
  }
```

### Binary Expression Rules

For rules with repeated binary operators like `A ~ (op ~ A)*`, use explicit iteration with `fold_binary`:

```zyn
addition = { multiplication ~ (add_op ~ multiplication)* }
  -> TypedNode<TypedExpression> {
      let mut rest: Vec<(String, TypedNode<TypedExpression>)> = Vec::new();
      let mut iter = all_children.iter();
      if let Some(first) = iter.next() {
          while let Some(op_pair) = iter.next() {
              if let Some(operand_pair) = iter.next() {
                  let op_str = op_pair.as_str().to_string();
                  if let Ok(operand) = self.build_multiplication(operand_pair.clone()) {
                      rest.push((op_str, operand));
                  }
              }
          }
          Self::fold_binary(self.build_multiplication(first.clone())?, rest, BinaryOp::Add)
      } else {
          typed_node(TypedExpression::Variable(intern("error")), Type::Never, span)
      }
  }
```

### Dispatch Rules

For choice rules without custom logic, use explicit dispatch:

```zyn
primary = {
    literal | struct_literal | identifier
}
  -> TypedNode<TypedExpression> {
      if let Some(inner) = all_children.first() {
          match inner.as_rule() {
              Rule::literal => self.build_literal(inner.clone())?,
              Rule::struct_literal => self.build_struct_literal(inner.clone())?,
              Rule::identifier => {
                  let name = inner.as_str();
                  typed_node(
                      TypedExpression::Variable(intern(name)),
                      Type::Never,
                      span,
                  )
              }
              _ => typed_node(TypedExpression::Variable(intern(pair_str)), Type::Never, span)
          }
      } else {
          typed_node(TypedExpression::Variable(intern(pair_str)), Type::Never, span)
      }
  }
```

## Available Variables in Actions

The generator provides these variables in action blocks:

| Variable | Type | Description |
|----------|------|-------------|
| `span` | `Span` | Source span of the matched text |
| `pair_str` | `&str` | The matched text as a string |
| `all_children` | `Vec<Pair<Rule>>` | All child pairs from the match |

## Helper Functions

### `intern(s: &str) -> InternedString`
Interns a string using the global interner. Use for identifiers and names.

### `span_from_pest(pest_span) -> Span`
Converts a pest span to a TypedAST span.

### `typed_node(expr, ty, span) -> TypedNode<T>`
Creates a TypedNode wrapper.

### `Self::fold_binary(first, rest, default_op) -> TypedNode<TypedExpression>`
Folds a list of (operator, operand) pairs into a binary expression tree.

### `Self::parse_unary_op(op: &str) -> UnaryOp`
Parses a unary operator string.

## Type Annotations

Return types must be valid Rust types that exist in the imported modules:

```zyn
// Primitive types
  -> String { ... }
  -> i32 { ... }

// TypedAST types
  -> TypedParameter { ... }
  -> TypedNode<TypedExpression> { ... }
  -> Vec<TypedParameter> { ... }

// Struct initialization
  -> TypedFieldInit {
      TypedFieldInit {
          name: intern(&field_name),
          value: Box::new(value_expr),
      }
  }
```

## Common Patterns

### Struct Construction

```zyn
rule = { ... }
  -> TypedBlock {
      TypedBlock {
          statements: child_statement
              .iter()
              .filter_map(|p| self.build_statement(p.clone()).ok())
              .collect::<Vec<_>>(),
          span: span,
      }
  }
```

### Optional Children

```zyn
rule = { required ~ optional? }
  -> TypedNode<TypedExpression> {
      let opt_value = all_children
          .iter()
          .find(|p| p.as_rule() == Rule::optional)
          .and_then(|p| self.build_optional(p.clone()).ok());
      // Use opt_value...
  }
```

### Filtering by Rule Type

```zyn
  -> Vec<TypedStatement> {
      all_children
          .iter()
          .filter(|p| p.as_rule() == Rule::statement)
          .filter_map(|p| self.build_statement(p.clone()).ok())
          .collect()
  }
```

## Testing

Integration tests should verify the full pipeline:

1. **Grammar parsing**: `ZigParser::parse(Rule::program, input)`
2. **TypedAST building**: `parse_to_typed_ast::<ZigParser>(input)`
3. **HIR lowering**: `lowering_ctx.lower_program(&program)`
4. **JIT compilation**: `backend.compile_module(&hir_module)`
5. **Execution**: Call the compiled function and verify results

Example:
```rust
#[test]
fn test_jit_compile_and_execute() {
    let input = r#"
fn add(x: i32, y: i32) i32 {
    return x + y;
}
"#;
    let program = parse_to_typed_ast::<ZigParser>(input).expect("Failed to parse");
    // ... lower, compile, execute
    assert_eq!(result, 30);
}
```

## Troubleshooting

### "Both parameters have the same name"
Use explicit `all_children` iteration instead of `$1`/`$2` placeholders.

### "Variable not found" in SSA
Ensure variable names are correctly interned. Check that parameter names match variable references.

### Binary expression only returns first operand
Implement explicit `fold_binary` pattern instead of `$1.clone()`.

### Generated code has `todo!("Failed to parse raw code")`
The action code couldn't be parsed as valid Rust. Use full struct constructors instead of field-only syntax.
