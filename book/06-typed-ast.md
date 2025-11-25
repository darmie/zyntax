# Chapter 6: The TypedAST

The TypedAST is the universal intermediate representation that Zyn grammars produce. This chapter explains its structure and design principles.

## Design Philosophy

The TypedAST is designed to:

1. **Carry full type information** - Every node knows its type
2. **Preserve source locations** - Enable precise error messages
3. **Support multiple paradigms** - OOP, functional, procedural
4. **Be serializable** - JSON and binary formats for tooling

## Core Structure

### TypedNode

Every AST node is wrapped in `TypedNode`:

```rust
pub struct TypedNode<T> {
    pub node: T,      // The actual content
    pub ty: Type,     // Type annotation
    pub span: Span,   // Source location (start, end)
}
```

This means every expression, statement, and declaration carries:
- What it is (the node)
- What type it has
- Where it came from in the source

### TypedProgram

The root of every AST:

```rust
pub struct TypedProgram {
    pub declarations: Vec<TypedNode<TypedDeclaration>>,
    pub span: Span,
}
```

A program is simply a list of top-level declarations.

## Declarations

### TypedDeclaration

Top-level items in a program:

```rust
pub enum TypedDeclaration {
    Function(TypedFunction),
    Variable(TypedVariable),
    Class(TypedClass),
    Interface(TypedInterface),
    Enum(TypedEnum),
    TypeAlias(TypedTypeAlias),
    Module(TypedModule),
    Import(TypedImport),
}
```

### TypedFunction

```rust
pub struct TypedFunction {
    pub name: InternedString,
    pub type_params: Vec<TypedTypeParam>,  // Generics
    pub params: Vec<TypedParameter>,
    pub return_type: Type,
    pub body: Option<TypedBlock>,          // None for extern
    pub visibility: Visibility,
    pub is_async: bool,
    pub is_external: bool,
    pub calling_convention: CallingConvention,
    pub link_name: Option<InternedString>,
}
```

### TypedEnum

```rust
pub struct TypedEnum {
    pub name: InternedString,
    pub type_params: Vec<TypedTypeParam>,
    pub variants: Vec<TypedVariant>,
    pub visibility: Visibility,
    pub span: Span,
}

pub struct TypedVariant {
    pub name: InternedString,
    pub fields: TypedVariantFields,  // Unit, Tuple, or Struct
    pub discriminant: Option<TypedNode<TypedExpression>>,
    pub span: Span,
}
```

### TypedClass (Structs)

```rust
pub struct TypedClass {
    pub name: InternedString,
    pub type_params: Vec<TypedTypeParam>,
    pub extends: Option<Type>,             // Inheritance
    pub implements: Vec<Type>,             // Interfaces
    pub fields: Vec<TypedField>,
    pub methods: Vec<TypedFunction>,
    pub constructors: Vec<TypedFunction>,
    pub visibility: Visibility,
    pub is_abstract: bool,
    pub span: Span,
}
```

## Statements

### TypedStatement

```rust
pub enum TypedStatement {
    Expression(Box<TypedNode<TypedExpression>>),
    Let(TypedLet),
    Return(Option<Box<TypedNode<TypedExpression>>>),
    If(TypedIf),
    While(TypedWhile),
    For(TypedFor),
    Loop(TypedLoop),
    Match(TypedMatch),
    Block(TypedBlock),
    Break(Option<Box<TypedNode<TypedExpression>>>),
    Continue,
    Try(TypedTry),
    Throw(Box<TypedNode<TypedExpression>>),
    Defer(TypedDefer),
}
```

### TypedLet

Variable declarations:

```rust
pub struct TypedLet {
    pub name: InternedString,
    pub ty: Type,
    pub mutability: Mutability,  // Mutable or Immutable
    pub initializer: Option<Box<TypedNode<TypedExpression>>>,
    pub span: Span,
}
```

### TypedIf

```rust
pub struct TypedIf {
    pub condition: Box<TypedNode<TypedExpression>>,
    pub then_block: TypedBlock,
    pub else_block: Option<TypedBlock>,
    pub span: Span,
}
```

### TypedBlock

```rust
pub struct TypedBlock {
    pub statements: Vec<TypedNode<TypedStatement>>,
    pub span: Span,
}
```

## Expressions

### TypedExpression

```rust
pub enum TypedExpression {
    Literal(TypedLiteral),
    Variable(InternedString),
    Binary(TypedBinary),
    Unary(TypedUnary),
    Call(TypedCall),
    Field(TypedFieldAccess),
    Index(TypedIndex),
    Array(Vec<TypedNode<TypedExpression>>),
    Tuple(Vec<TypedNode<TypedExpression>>),
    Struct(TypedStructLiteral),
    Lambda(TypedLambda),
    Match(TypedMatchExpr),
    If(TypedIfExpr),
    Cast(TypedCast),
    Await(Box<TypedNode<TypedExpression>>),
    Try(Box<TypedNode<TypedExpression>>),
    Reference(TypedReference),
    Dereference(Box<TypedNode<TypedExpression>>),
    Range(TypedRange),
    MethodCall(TypedMethodCall),
    Block(TypedBlock),
}
```

### TypedLiteral

```rust
pub enum TypedLiteral {
    Integer(i128),
    Float(f64),
    Bool(bool),
    String(InternedString),
    Char(char),
    Unit,
    Null,
    Undefined,
}
```

### TypedBinary

```rust
pub struct TypedBinary {
    pub op: BinaryOp,
    pub left: Box<TypedNode<TypedExpression>>,
    pub right: Box<TypedNode<TypedExpression>>,
}

pub enum BinaryOp {
    // Arithmetic
    Add, Sub, Mul, Div, Rem,
    // Comparison
    Eq, Ne, Lt, Le, Gt, Ge,
    // Logical
    And, Or,
    // Bitwise
    BitAnd, BitOr, BitXor, Shl, Shr,
    // Assignment
    Assign,
    // Error handling (Zig-style)
    Orelse, Catch,
}
```

### TypedCall

```rust
pub struct TypedCall {
    pub callee: Box<TypedNode<TypedExpression>>,
    pub positional_args: Vec<TypedNode<TypedExpression>>,
    pub named_args: Vec<TypedNamedArg>,
    pub type_args: Vec<Type>,  // Generic arguments
}
```

### TypedStructLiteral

```rust
pub struct TypedStructLiteral {
    pub name: InternedString,
    pub fields: Vec<TypedFieldInit>,
}

pub struct TypedFieldInit {
    pub name: InternedString,
    pub value: Box<TypedNode<TypedExpression>>,
}
```

## The Type System

### Type

```rust
pub enum Type {
    // Primitive types
    Primitive(PrimitiveType),

    // Composite types
    Array { element: Box<Type>, size: Option<ConstValue>, nullability: NullabilityKind },
    Tuple(Vec<Type>),
    Struct { fields: Vec<FieldDef>, is_anonymous: bool, nullability: NullabilityKind },

    // Reference types
    Reference { inner: Box<Type>, mutability: Mutability },
    Pointer { inner: Box<Type>, mutability: Mutability },

    // Named types
    Named { name: InternedString, type_args: Vec<Type> },

    // Function types
    Function { params: Vec<ParamInfo>, return_type: Box<Type>, is_variadic: bool, async_kind: AsyncKind },

    // Optional/Error types
    Optional(Box<Type>),
    Result { ok: Box<Type>, err: Box<Type> },

    // Special types
    Never,      // Bottom type (unreachable)
    Unknown,    // Top type (any)
    Infer,      // To be inferred
}
```

### PrimitiveType

```rust
pub enum PrimitiveType {
    Bool,
    I8, I16, I32, I64, I128,
    U8, U16, U32, U64, U128,
    F32, F64,
    Char,
    String,
    Unit,
}
```

### Mutability

```rust
pub enum Mutability {
    Mutable,
    Immutable,
}
```

### Visibility

```rust
pub enum Visibility {
    Public,
    Private,
    Protected,
    Internal,
}
```

## Patterns (for Match)

### TypedPattern

```rust
pub enum TypedPattern {
    // Simple patterns
    Wildcard,
    Variable { name: InternedString, mutability: Mutability },
    Literal(TypedLiteralPattern),

    // Compound patterns
    Tuple(Vec<TypedNode<TypedPattern>>),
    Array(Vec<TypedNode<TypedPattern>>),
    Struct { name: InternedString, fields: Vec<TypedFieldPattern> },
    Enum { name: InternedString, variant: InternedString, fields: Vec<TypedNode<TypedPattern>> },

    // Advanced patterns
    Or(Vec<TypedNode<TypedPattern>>),
    Guard { pattern: Box<TypedNode<TypedPattern>>, condition: Box<TypedNode<TypedExpression>> },
    Range { start: Box<TypedNode<TypedPattern>>, end: Box<TypedNode<TypedPattern>>, inclusive: bool },
}
```

## String Interning

The TypedAST uses `InternedString` for all identifiers to:
- Save memory (each unique string stored once)
- Enable fast equality checks (pointer comparison)
- Support serialization (strings resolved on load)

```rust
pub struct InternedString(Symbol);

impl InternedString {
    // Create from string using global interner
    pub fn new_global(s: &str) -> Self;

    // Resolve to string
    pub fn resolve_global(&self) -> Option<String>;
}
```

## Source Locations

### Span

```rust
pub struct Span {
    pub start: usize,  // Byte offset
    pub end: usize,    // Byte offset
}
```

Spans enable:
- Error messages with line/column info
- IDE features (go to definition, hover)
- Source maps for debugging

## Serialization

The TypedAST supports JSON serialization:

```rust
// Serialize to JSON
let json = serde_json::to_string_pretty(&program)?;

// Deserialize from JSON
let program: TypedProgram = serde_json::from_str(&json)?;
```

This enables:
- Caching compiled ASTs
- Tool interoperability
- Debugging and inspection

## Example: Complete AST

For this Zig code:

```zig
fn add(a: i32, b: i32) i32 {
    return a + b;
}
```

The TypedAST (simplified) looks like:

```rust
TypedProgram {
    declarations: [
        TypedNode {
            node: TypedDeclaration::Function(TypedFunction {
                name: "add",
                params: [
                    TypedParameter { name: "a", ty: Type::Primitive(I32) },
                    TypedParameter { name: "b", ty: Type::Primitive(I32) },
                ],
                return_type: Type::Primitive(I32),
                body: Some(TypedBlock {
                    statements: [
                        TypedNode {
                            node: TypedStatement::Return(Some(
                                TypedNode {
                                    node: TypedExpression::Binary(TypedBinary {
                                        op: BinaryOp::Add,
                                        left: TypedNode {
                                            node: TypedExpression::Variable("a"),
                                            ty: Type::Primitive(I32),
                                        },
                                        right: TypedNode {
                                            node: TypedExpression::Variable("b"),
                                            ty: Type::Primitive(I32),
                                        },
                                    }),
                                    ty: Type::Primitive(I32),
                                }
                            )),
                            ty: Type::Primitive(Unit),
                        }
                    ]
                }),
            }),
            ty: Type::Never,
        }
    ]
}
```

## Next Steps

- [Chapter 7](./07-typed-ast-builder.md): Use the builder API to construct ASTs programmatically
- [Chapter 8](./08-zig-example.md): See how grammar rules map to this structure
