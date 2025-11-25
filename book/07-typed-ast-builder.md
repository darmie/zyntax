# Chapter 7: TypedAST Builder

The `TypedASTBuilder` provides a fluent API for constructing TypedAST nodes programmatically. While Zyn grammars use this internally, you can also use it directly for testing, code generation, or custom frontends.

## Getting Started

```rust
use zyntax_typed_ast::{
    TypedASTBuilder, TypedProgram, Type, PrimitiveType,
    Mutability, Span, BinaryOp, UnaryOp,
};

fn main() {
    let mut builder = TypedASTBuilder::new();
    let span = Span::new(0, 10);

    // Build a simple expression: 1 + 2
    let one = builder.int_literal(1, span);
    let two = builder.int_literal(2, span);
    let sum = builder.binary(
        BinaryOp::Add,
        one,
        two,
        Type::Primitive(PrimitiveType::I32),
        span,
    );
}
```

## Core Concepts

### The Builder Pattern

The builder maintains internal state for:
- **Arena allocation** - Efficient memory management
- **String interning** - Deduplicated identifiers
- **Type registry** - Type definitions

```rust
let mut builder = TypedASTBuilder::new();

// Intern strings for identifiers
let name = builder.intern("my_variable");

// Access the arena for custom allocation
let arena = builder.arena();
```

### Spans

Every node needs a source location:

```rust
// Create a span (byte offsets)
let span = builder.span(0, 42);

// Or use a dummy span for testing
let span = builder.dummy_span();
```

### Type Helpers

Common types have convenience methods:

```rust
let i32_ty = builder.i32_type();    // Type::Primitive(PrimitiveType::I32)
let i64_ty = builder.i64_type();    // Type::Primitive(PrimitiveType::I64)
let bool_ty = builder.bool_type();  // Type::Primitive(PrimitiveType::Bool)
let str_ty = builder.string_type(); // Type::Primitive(PrimitiveType::String)
let unit_ty = builder.unit_type();  // Type::Primitive(PrimitiveType::Unit)
```

## Building Expressions

### Literals

```rust
let span = builder.dummy_span();

// Integer literal
let int_expr = builder.int_literal(42, span);

// String literal
let str_expr = builder.string_literal("hello", span);

// Boolean literal
let bool_expr = builder.bool_literal(true, span);

// Character literal
let char_expr = builder.char_literal('x', span);

// Unit literal (void)
let unit_expr = builder.unit_literal(span);
```

### Variables

```rust
// Variable reference
let var_expr = builder.variable(
    "counter",
    Type::Primitive(PrimitiveType::I32),
    span,
);
```

### Binary Operations

```rust
let left = builder.int_literal(10, span);
let right = builder.int_literal(5, span);

// 10 + 5
let sum = builder.binary(
    BinaryOp::Add,
    left.clone(),
    right.clone(),
    builder.i32_type(),
    span,
);

// 10 == 5
let eq = builder.binary(
    BinaryOp::Eq,
    left.clone(),
    right.clone(),
    builder.bool_type(),
    span,
);

// Available operators:
// Arithmetic: Add, Sub, Mul, Div, Rem
// Comparison: Eq, Ne, Lt, Le, Gt, Ge
// Logical: And, Or
// Bitwise: BitAnd, BitOr, BitXor, Shl, Shr
// Assignment: Assign
```

### Unary Operations

```rust
let value = builder.int_literal(42, span);

// -42
let neg = builder.unary(
    UnaryOp::Minus,
    value,
    builder.i32_type(),
    span,
);

// !flag
let flag = builder.bool_literal(true, span);
let not_flag = builder.unary(
    UnaryOp::Not,
    flag,
    builder.bool_type(),
    span,
);
```

### Function Calls

```rust
// Simple call: add(1, 2)
let callee = builder.variable("add", builder.i32_type(), span);
let arg1 = builder.int_literal(1, span);
let arg2 = builder.int_literal(2, span);

let call = builder.call_positional(
    callee,
    vec![arg1, arg2],
    builder.i32_type(),  // Return type
    span,
);

// Named arguments: greet(name: "Alice", times: 3)
let callee = builder.variable("greet", builder.unit_type(), span);
let call = builder.call_named(
    callee,
    vec![
        ("name", builder.string_literal("Alice", span)),
        ("times", builder.int_literal(3, span)),
    ],
    builder.unit_type(),
    span,
);
```

### Field Access

```rust
// point.x
let point = builder.variable("point", builder.i32_type(), span);
let field = builder.field_access(
    point,
    "x",
    builder.i32_type(),  // Field type
    span,
);
```

### Array/Index Access

```rust
// arr[0]
let arr = builder.variable("arr", builder.i32_type(), span);
let index = builder.int_literal(0, span);
let element = builder.index(
    arr,
    index,
    builder.i32_type(),  // Element type
    span,
);
```

### Struct Literals

```rust
// Point{ x: 10, y: 20 }
let struct_lit = builder.struct_literal(
    "Point",
    vec![
        ("x", builder.int_literal(10, span)),
        ("y", builder.int_literal(20, span)),
    ],
    Type::Named {
        name: builder.intern("Point"),
        type_args: vec![],
    },
    span,
);
```

### Array Literals

```rust
// [1, 2, 3]
let arr = builder.array_literal(
    vec![
        builder.int_literal(1, span),
        builder.int_literal(2, span),
        builder.int_literal(3, span),
    ],
    Type::Array {
        element: Box::new(builder.i32_type()),
        size: Some(zyntax_typed_ast::ConstValue::Int(3)),
        nullability: zyntax_typed_ast::NullabilityKind::NonNull,
    },
    span,
);
```

### Lambda/Closure

```rust
// |x| x + 1
let body = builder.binary(
    BinaryOp::Add,
    builder.variable("x", builder.i32_type(), span),
    builder.int_literal(1, span),
    builder.i32_type(),
    span,
);

let lambda = builder.lambda(
    vec![("x", Some(builder.i32_type()))],
    body,
    Type::Function {
        params: vec![],
        return_type: Box::new(builder.i32_type()),
        is_variadic: false,
        async_kind: zyntax_typed_ast::AsyncKind::Sync,
    },
    span,
);
```

## Building Statements

### Let/Variable Declaration

```rust
// const x: i32 = 42;
let init = builder.int_literal(42, span);
let let_stmt = builder.let_statement(
    "x",
    builder.i32_type(),
    Mutability::Immutable,
    Some(init),
    span,
);

// var y = 0;
let init = builder.int_literal(0, span);
let var_stmt = builder.let_statement(
    "y",
    builder.i32_type(),
    Mutability::Mutable,
    Some(init),
    span,
);
```

### Expression Statement

```rust
// foo();
let call = builder.call_positional(
    builder.variable("foo", builder.unit_type(), span),
    vec![],
    builder.unit_type(),
    span,
);
let expr_stmt = builder.expression_statement(call, span);
```

### Return Statement

```rust
// return 42;
let value = builder.int_literal(42, span);
let ret = builder.return_stmt(value, span);

// return;
let ret_void = builder.return_void(span);
```

### If Statement

```rust
// if (x > 0) { return x; }
let condition = builder.binary(
    BinaryOp::Gt,
    builder.variable("x", builder.i32_type(), span),
    builder.int_literal(0, span),
    builder.bool_type(),
    span,
);

let then_block = builder.block(
    vec![builder.return_stmt(
        builder.variable("x", builder.i32_type(), span),
        span,
    )],
    span,
);

let if_stmt = builder.if_statement(
    condition,
    then_block,
    None,  // No else branch
    span,
);

// With else branch
let else_block = builder.block(
    vec![builder.return_stmt(
        builder.int_literal(0, span),
        span,
    )],
    span,
);

let if_else = builder.if_statement(
    condition,
    then_block,
    Some(else_block),
    span,
);
```

### While Loop

```rust
// while (i < 10) { i = i + 1; }
let condition = builder.binary(
    BinaryOp::Lt,
    builder.variable("i", builder.i32_type(), span),
    builder.int_literal(10, span),
    builder.bool_type(),
    span,
);

let increment = builder.binary(
    BinaryOp::Add,
    builder.variable("i", builder.i32_type(), span),
    builder.int_literal(1, span),
    builder.i32_type(),
    span,
);

let body = builder.block(
    vec![builder.expression_statement(
        builder.binary(
            BinaryOp::Assign,
            builder.variable("i", builder.i32_type(), span),
            increment,
            builder.i32_type(),
            span,
        ),
        span,
    )],
    span,
);

let while_stmt = builder.while_loop(condition, body, span);
```

### For Loop

```rust
// for (items) |item| { process(item); }
let items = builder.variable("items", builder.i32_type(), span);
let body = builder.block(
    vec![builder.expression_statement(
        builder.call_positional(
            builder.variable("process", builder.unit_type(), span),
            vec![builder.variable("item", builder.i32_type(), span)],
            builder.unit_type(),
            span,
        ),
        span,
    )],
    span,
);

let for_stmt = builder.for_loop("item", items, body, span);
```

### Break and Continue

```rust
let break_stmt = builder.break_stmt(span);
let continue_stmt = builder.continue_stmt(span);

// break with value
let value = builder.int_literal(42, span);
let break_val = builder.break_with_value(value, span);
```

### Blocks

```rust
let statements = vec![
    builder.let_statement("x", builder.i32_type(), Mutability::Immutable,
                         Some(builder.int_literal(1, span)), span),
    builder.let_statement("y", builder.i32_type(), Mutability::Immutable,
                         Some(builder.int_literal(2, span)), span),
    builder.return_stmt(
        builder.binary(
            BinaryOp::Add,
            builder.variable("x", builder.i32_type(), span),
            builder.variable("y", builder.i32_type(), span),
            builder.i32_type(),
            span,
        ),
        span,
    ),
];

let block = builder.block(statements, span);
```

## Building Declarations

### Function Declaration

```rust
use zyntax_typed_ast::typed_ast::{TypedFunction, TypedParameter, ParameterKind};
use zyntax_typed_ast::type_registry::CallingConvention;

// fn add(a: i32, b: i32) i32 { return a + b; }
let body = builder.block(
    vec![builder.return_stmt(
        builder.binary(
            BinaryOp::Add,
            builder.variable("a", builder.i32_type(), span),
            builder.variable("b", builder.i32_type(), span),
            builder.i32_type(),
            span,
        ),
        span,
    )],
    span,
);

let func = TypedFunction {
    name: builder.intern("add"),
    type_params: vec![],
    params: vec![
        TypedParameter::regular(
            builder.intern("a"),
            builder.i32_type(),
            Mutability::Immutable,
            span,
        ),
        TypedParameter::regular(
            builder.intern("b"),
            builder.i32_type(),
            Mutability::Immutable,
            span,
        ),
    ],
    return_type: builder.i32_type(),
    body: Some(body),
    visibility: Visibility::Public,
    is_async: false,
    is_external: false,
    calling_convention: CallingConvention::Default,
    link_name: None,
};
```

## Building Patterns

For match expressions and destructuring:

```rust
// Variable pattern
let var_pat = TypedPattern::Variable {
    name: builder.intern("x"),
    mutability: Mutability::Immutable,
};

// Wildcard pattern
let wildcard = TypedPattern::Wildcard;

// Struct pattern: Point { x, y }
let struct_pat = builder.struct_pattern(
    "Point",
    vec![
        ("x", typed_node(var_pat.clone(), Type::Never, span)),
        ("y", typed_node(var_pat.clone(), Type::Never, span)),
    ],
    span,
);

// Enum pattern: Some(value)
let enum_pat = builder.enum_pattern(
    "Option",
    "Some",
    vec![typed_node(var_pat, Type::Never, span)],
    span,
);
```

## Complete Example

Building a factorial function:

```rust
fn build_factorial(builder: &mut TypedASTBuilder) -> TypedProgram {
    let span = builder.dummy_span();

    // fn factorial(n: i32) i32 {
    //     if (n <= 1) {
    //         return 1;
    //     }
    //     return n * factorial(n - 1);
    // }

    // Condition: n <= 1
    let condition = builder.binary(
        BinaryOp::Le,
        builder.variable("n", builder.i32_type(), span),
        builder.int_literal(1, span),
        builder.bool_type(),
        span,
    );

    // Then block: return 1
    let then_block = builder.block(
        vec![builder.return_stmt(builder.int_literal(1, span), span)],
        span,
    );

    // Recursive call: factorial(n - 1)
    let recursive_call = builder.call_positional(
        builder.variable("factorial", builder.i32_type(), span),
        vec![builder.binary(
            BinaryOp::Sub,
            builder.variable("n", builder.i32_type(), span),
            builder.int_literal(1, span),
            builder.i32_type(),
            span,
        )],
        builder.i32_type(),
        span,
    );

    // n * factorial(n - 1)
    let multiply = builder.binary(
        BinaryOp::Mul,
        builder.variable("n", builder.i32_type(), span),
        recursive_call,
        builder.i32_type(),
        span,
    );

    // Function body
    let body = builder.block(
        vec![
            builder.if_statement(condition, then_block, None, span),
            builder.return_stmt(multiply, span),
        ],
        span,
    );

    // Build function
    let func = TypedFunction {
        name: builder.intern("factorial"),
        type_params: vec![],
        params: vec![TypedParameter::regular(
            builder.intern("n"),
            builder.i32_type(),
            Mutability::Immutable,
            span,
        )],
        return_type: builder.i32_type(),
        body: Some(body),
        visibility: Visibility::Public,
        is_async: false,
        is_external: false,
        calling_convention: CallingConvention::Default,
        link_name: None,
    };

    TypedProgram {
        declarations: vec![typed_node(
            TypedDeclaration::Function(func),
            Type::Never,
            span,
        )],
        span,
    }
}
```

## Next Steps

- [Chapter 8](./08-zig-example.md): See how grammar semantic actions map to builder calls
- [Chapter 9](./09-reference.md): Complete API reference
