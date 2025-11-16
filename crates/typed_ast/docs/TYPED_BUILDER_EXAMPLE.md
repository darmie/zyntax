# TypedAST Builder Guide

## Overview

The `TypedASTBuilder` provides a fluent, type-safe API for constructing well-formed TypedAST nodes programmatically. It's the primary way to build typed abstract syntax trees for compiler frontends targeting Zyntax.

## Quick Start

```rust
use zyntax_typed_ast::TypedASTBuilder;

let mut builder = TypedASTBuilder::new();

// Register types
let i32_type = builder.registry.register_primitive(PrimitiveType::I32);
let string_type = builder.registry.register_primitive(PrimitiveType::String);

// Build a simple function
let add_fn = builder.build_function(
    "add",
    vec![
        builder.build_param("a", i32_type.clone()),
        builder.build_param("b", i32_type.clone()),
    ],
    i32_type.clone(),
    // Function body goes here
);
```

## Core Concepts

### 1. Arena and String Interning

The builder uses an arena for efficient string storage:

```rust
// Intern a string
let name = builder.intern("myFunction");

// Access the arena directly
let arena = builder.arena();
```

### 2. Type Registry Integration

The builder has an integrated `TypeRegistry` for managing types:

```rust
// Primitive types
let i32_ty = builder.registry.register_primitive(PrimitiveType::I32);
let bool_ty = builder.registry.register_primitive(PrimitiveType::Bool);
let string_ty = builder.registry.register_primitive(PrimitiveType::String);

// Struct types
let point_ty = builder.registry.register_struct(
    builder.intern("Point"),
    vec![
        ("x", i32_ty.clone()),
        ("y", i32_ty.clone()),
    ],
    false, // not structural
);

// Function types
let fn_ty = builder.registry.register_function_type(
    vec![i32_ty.clone(), i32_ty.clone()],
    i32_ty.clone(),
);
```

## Building Expressions

### Literals

```rust
// Integer literals
let ten = builder.build_int_literal(10, i32_ty.clone());

// Boolean literals
let true_val = builder.build_bool_literal(true);

// String literals
let hello = builder.build_string_literal("Hello, World!");

// Float literals
let pi = builder.build_float_literal(3.14159, f64_ty.clone());
```

### Variables and Identifiers

```rust
// Variable reference
let x_ref = builder.build_variable("x", i32_ty.clone());

// Identifier expression
let ident = builder.build_identifier("counter", i32_ty.clone());
```

### Binary Operations

```rust
// Arithmetic: a + b
let sum = builder.build_binary_op(
    BinaryOperator::Add,
    a_expr,
    b_expr,
    i32_ty.clone(),
);

// Comparison: x < 10
let cmp = builder.build_binary_op(
    BinaryOperator::LessThan,
    x_expr,
    ten_expr,
    bool_ty.clone(),
);

// Logical: p && q
let and = builder.build_binary_op(
    BinaryOperator::And,
    p_expr,
    q_expr,
    bool_ty.clone(),
);
```

### Function Calls

```rust
// Simple call: add(5, 3)
let call = builder.build_call(
    add_fn_id,
    vec![five_expr, three_expr],
    i32_ty.clone(),
);

// Method call: obj.method(arg)
let method_call = builder.build_method_call(
    obj_expr,
    "method",
    vec![arg_expr],
    return_ty.clone(),
);
```

### Member Access

```rust
// Field access: point.x
let field = builder.build_field_access(
    point_expr,
    "x",
    i32_ty.clone(),
);

// Array indexing: arr[0]
let index = builder.build_index(
    arr_expr,
    zero_expr,
    element_ty.clone(),
);
```

### Construction

```rust
// Struct construction: Point { x: 10, y: 20 }
let point = builder.build_struct_literal(
    point_ty.clone(),
    vec![
        ("x", ten_expr),
        ("y", twenty_expr),
    ],
);

// Array literal: [1, 2, 3]
let array = builder.build_array_literal(
    vec![one_expr, two_expr, three_expr],
    array_ty.clone(),
);

// Tuple: (42, "hello", true)
let tuple = builder.build_tuple(
    vec![num_expr, str_expr, bool_expr],
    tuple_ty.clone(),
);
```

## Building Statements

### Variable Declarations

```rust
// let mut x: i32 = 42;
let var_decl = builder.build_variable_declaration(
    "x",
    Some(i32_ty.clone()),
    Some(forty_two_expr),
    Mutability::Mutable,
);

// Immutable: let y = 10;
let const_decl = builder.build_variable_declaration(
    "y",
    None, // type inferred
    Some(ten_expr),
    Mutability::Immutable,
);
```

### Assignment

```rust
// x = value;
let assign = builder.build_assignment(
    x_ref,
    value_expr,
);

// Compound: x += 5;
let compound = builder.build_compound_assignment(
    BinaryOperator::Add,
    x_ref,
    five_expr,
);
```

### Control Flow

```rust
// if condition { then_branch } else { else_branch }
let if_stmt = builder.build_if_statement(
    condition_expr,
    then_block,
    Some(else_block),
);

// while condition { body }
let while_loop = builder.build_while_loop(
    condition_expr,
    body_block,
);

// for item in iterable { body }
let for_loop = builder.build_for_loop(
    "item",
    iterable_expr,
    body_block,
);
```

### Return Statements

```rust
// return value;
let ret = builder.build_return(Some(value_expr));

// return; (void)
let void_ret = builder.build_return(None);
```

## Building Declarations

### Functions

```rust
// fn add(a: i32, b: i32) -> i32 { a + b }
let add_fn = builder.build_function(
    "add",
    vec![
        builder.build_param("a", i32_ty.clone()),
        builder.build_param("b", i32_ty.clone()),
    ],
    i32_ty.clone(),
    vec![
        builder.build_return(Some(
            builder.build_binary_op(
                BinaryOperator::Add,
                a_ref,
                b_ref,
                i32_ty.clone(),
            )
        ))
    ],
);
```

### Generic Functions

```rust
// fn identity<T>(x: T) -> T { x }
let identity_fn = builder.build_generic_function(
    "identity",
    vec![builder.intern("T")], // type parameters
    vec![builder.build_param("x", t_var)],
    t_var.clone(),
    body,
);
```

### Classes/Structs

```rust
// struct Point { x: i32, y: i32 }
let point_struct = builder.build_struct_declaration(
    "Point",
    vec![
        builder.build_field("x", i32_ty.clone(), Visibility::Public),
        builder.build_field("y", i32_ty.clone(), Visibility::Public),
    ],
    vec![], // methods
    vec![], // type parameters
);
```

### Traits/Interfaces

```rust
// trait Drawable { fn draw(&self); }
let drawable = builder.build_trait_declaration(
    "Drawable",
    vec![
        builder.build_trait_method(
            "draw",
            vec![builder.build_self_param(Mutability::Immutable)],
            unit_ty.clone(),
        ),
    ],
    vec![], // type parameters
);
```

### Enums

```rust
// enum Option<T> { Some(T), None }
let option_enum = builder.build_enum_declaration(
    "Option",
    vec![builder.intern("T")], // type parameter
    vec![
        builder.build_enum_variant("Some", vec![t_var.clone()]),
        builder.build_enum_variant("None", vec![]),
    ],
);
```

## Pattern Matching

```rust
// match value {
//     Some(x) => x,
//     None => 0,
// }
let match_expr = builder.build_match(
    value_expr,
    vec![
        builder.build_match_arm(
            builder.build_constructor_pattern("Some", vec![x_pattern]),
            x_expr,
        ),
        builder.build_match_arm(
            builder.build_constructor_pattern("None", vec![]),
            zero_expr,
        ),
    ],
    result_ty.clone(),
);
```

### Pattern Types

```rust
// Literal pattern: 42
let lit_pattern = builder.build_literal_pattern(42);

// Variable binding: x
let var_pattern = builder.build_variable_pattern("x", i32_ty.clone());

// Wildcard: _
let wildcard = builder.build_wildcard_pattern();

// Constructor: Some(x)
let constructor = builder.build_constructor_pattern(
    "Some",
    vec![x_pattern],
);

// Tuple: (x, y)
let tuple_pattern = builder.build_tuple_pattern(
    vec![x_pattern, y_pattern],
);
```

## Async/Await

```rust
// async fn fetch_data() -> String { ... }
let async_fn = builder.build_async_function(
    "fetch_data",
    vec![],
    string_ty.clone(),
    body,
);

// await expression: await future
let await_expr = builder.build_await(
    future_expr,
    result_ty.clone(),
);
```

## Advanced Features

### Closures/Lambdas

```rust
// |x| x + 1
let closure = builder.build_closure(
    vec![builder.build_param("x", i32_ty.clone())],
    builder.build_binary_op(
        BinaryOperator::Add,
        x_ref,
        one_expr,
        i32_ty.clone(),
    ),
    fn_ty.clone(),
);
```

### Type Annotations

```rust
// value as TargetType
let cast = builder.build_type_cast(
    value_expr,
    target_ty.clone(),
);

// type assertion
let annotated = builder.build_type_annotation(
    expr,
    explicit_ty.clone(),
);
```

### Memory Operations

```rust
// Reference: &x
let ref_expr = builder.build_reference(
    x_expr,
    Mutability::Immutable,
    ref_ty.clone(),
);

// Dereference: *ptr
let deref = builder.build_dereference(
    ptr_expr,
    value_ty.clone(),
);
```

## Complete Example: Building a Module

```rust
use zyntax_typed_ast::TypedASTBuilder;

fn build_math_module() -> TypedProgram {
    let mut builder = TypedASTBuilder::new();

    // Register types
    let i32_ty = builder.registry.register_primitive(PrimitiveType::I32);

    // Build add function
    let a_param = builder.build_param("a", i32_ty.clone());
    let b_param = builder.build_param("b", i32_ty.clone());

    let a_ref = builder.build_variable("a", i32_ty.clone());
    let b_ref = builder.build_variable("b", i32_ty.clone());

    let add_body = vec![
        builder.build_return(Some(
            builder.build_binary_op(
                BinaryOperator::Add,
                a_ref,
                b_ref,
                i32_ty.clone(),
            )
        ))
    ];

    let add_fn = builder.build_function(
        "add",
        vec![a_param, b_param],
        i32_ty.clone(),
        add_body,
    );

    // Build multiply function (similar structure)
    // ...

    // Create program
    TypedProgram {
        declarations: vec![add_fn],
        module_path: vec![builder.intern("math")],
    }
}
```

## Best Practices

1. **Type Registration**: Register all types with the `TypeRegistry` before building expressions
2. **String Interning**: Use `builder.intern()` for all identifiers and string literals
3. **Type Safety**: Always provide correct types to avoid type mismatches
4. **Spans**: Use real spans for production, `dummy_span()` only for testing
5. **Immutability**: Prefer immutable declarations unless mutation is required

## Integration with Type Checking

The builder creates well-formed AST nodes, but they still need type checking:

```rust
let mut builder = TypedASTBuilder::new();

// Build AST
let program = build_my_program(&mut builder);

// Type check
let mut type_checker = TypeChecker::new(&builder.registry);
let typed_program = type_checker.check_program(&program)?;
```

## See Also

- [Type System Design](type_system_design.md) - Understanding the type system
- [../../README.md](../../README.md) - TypedAST crate overview
- [../../docs/ARCHITECTURE.md](../../../docs/ARCHITECTURE.md) - Overall architecture

## API Reference

For a complete list of builder methods, see the source code documentation in `typed_builder.rs`.
