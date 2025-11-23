#![doc = r" ZynPEG TypedAST Types"]
#![doc = r""]
#![doc = r" This module provides the complete TypedAST type system for the"]
#![doc = r" generated parser. These types form the semantic representation"]
#![doc = r" of parsed source code."]
#![doc = r""]
#![doc = "Generated for: Zig"]
#![allow(dead_code, unused_variables, unused_imports)]
use std::collections::HashMap;
#[doc = r" Span in source code"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}
impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
    pub fn merge(a: &Self, b: &Self) -> Self {
        Self {
            start: a.start.min(b.start),
            end: a.end.max(b.end),
        }
    }
}
#[doc = r" Type representation (simplified for testing)"]
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    I8,
    I16,
    I32,
    I64,
    I128,
    U8,
    U16,
    U32,
    U64,
    U128,
    F32,
    F64,
    Bool,
    Void,
    String,
    Char,
    Usize,
    Isize,
    Array(Box<Type>, Option<usize>),
    Slice(Box<Type>),
    Pointer(Box<Type>),
    Optional(Box<Type>),
    Result(Box<Type>, Box<Type>),
    Function(Vec<Type>, Box<Type>),
    Tuple(Vec<Type>),
    Named(String),
    Unknown,
    Never,
    Any,
    Comptime(Box<Type>),
}
impl Default for Type {
    fn default() -> Self {
        Type::Unknown
    }
}
impl Type {
    pub fn unwrap_optional(&self) -> Type {
        match self {
            Type::Optional(inner) => (**inner).clone(),
            _ => self.clone(),
        }
    }
    pub fn unwrap_result(&self) -> Type {
        match self {
            Type::Result(inner, _) => (**inner).clone(),
            _ => self.clone(),
        }
    }
    pub fn deref(&self) -> Type {
        match self {
            Type::Pointer(inner) => (**inner).clone(),
            _ => self.clone(),
        }
    }
    pub fn element_type(&self) -> Type {
        match self {
            Type::Array(inner, _) | Type::Slice(inner) => (**inner).clone(),
            _ => Type::Unknown,
        }
    }
    pub fn return_type(&self) -> Type {
        match self {
            Type::Function(_, ret) => (**ret).clone(),
            _ => Type::Unknown,
        }
    }
}
#[doc = r" Binary operations"]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    And,
    Or,
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,
    Orelse,
    Catch,
}
#[doc = r" Unary operations"]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Neg,
    Not,
    Deref,
    AddrOf,
    Try,
    Await,
}
#[doc = r" Postfix operations for expression chaining"]
#[derive(Debug, Clone, PartialEq)]
pub enum PostfixOp {
    Deref,
    Field(String),
    Index(TypedExpression),
    Call(Vec<TypedExpression>),
    OptionalUnwrap,
    TryUnwrap,
}
#[doc = r" A typed expression"]
#[derive(Debug, Clone, PartialEq)]
pub struct TypedExpression {
    pub expr: Expression,
    pub ty: Type,
    pub span: Span,
}
impl Default for TypedExpression {
    fn default() -> Self {
        Self {
            expr: Expression::Unit,
            ty: Type::Unknown,
            span: Span::default(),
        }
    }
}
#[doc = r" Expression variants"]
#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    IntLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),
    CharLiteral(char),
    BoolLiteral(bool),
    NullLiteral,
    UndefinedLiteral,
    Identifier(String),
    Variable(String, Type),
    BinaryOp(BinaryOp, Box<TypedExpression>, Box<TypedExpression>),
    UnaryOp(UnaryOp, Box<TypedExpression>),
    FieldAccess(Box<TypedExpression>, String),
    Index(Box<TypedExpression>, Box<TypedExpression>),
    Deref(Box<TypedExpression>),
    Call(Box<TypedExpression>, Vec<TypedExpression>),
    MethodCall(Box<TypedExpression>, String, Vec<TypedExpression>),
    Array(Vec<TypedExpression>),
    Tuple(Vec<TypedExpression>),
    Struct(String, Vec<(String, TypedExpression)>),
    If(
        Box<TypedExpression>,
        Box<TypedExpression>,
        Option<Box<TypedExpression>>,
    ),
    Block(Vec<TypedStatement>),
    Unit,
    Error(String),
}
#[doc = r" A typed statement"]
#[derive(Debug, Clone, PartialEq)]
pub struct TypedStatement {
    pub stmt: Statement,
    pub span: Span,
}
#[doc = r" Statement variants"]
#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Let(String, Option<Type>, Option<TypedExpression>),
    Const(String, Option<Type>, TypedExpression),
    Assign(TypedExpression, TypedExpression),
    Expression(TypedExpression),
    Return(Option<TypedExpression>),
    If(
        TypedExpression,
        Vec<TypedStatement>,
        Option<Vec<TypedStatement>>,
    ),
    While(TypedExpression, Vec<TypedStatement>),
    For(String, TypedExpression, Vec<TypedStatement>),
    Block(Vec<TypedStatement>),
    Break(Option<TypedExpression>),
    Continue,
    Defer(Box<TypedStatement>),
    Try(Box<TypedStatement>, Vec<CatchClause>),
}
#[derive(Debug, Clone, PartialEq)]
pub struct CatchClause {
    pub error_var: String,
    pub body: Vec<TypedStatement>,
}
#[doc = r" A typed declaration"]
#[derive(Debug, Clone, PartialEq)]
pub struct TypedDeclaration {
    pub decl: Declaration,
    pub span: Span,
}
#[doc = r" Declaration variants"]
#[derive(Debug, Clone, PartialEq)]
pub enum Declaration {
    Function(FnDecl),
    Const(ConstDecl),
    Var(VarDecl),
    Struct(StructDecl),
    Enum(EnumDecl),
    Union(UnionDecl),
    ErrorSet(ErrorSetDecl),
    Import(ImportDecl),
}
#[derive(Debug, Clone, PartialEq)]
pub struct FnDecl {
    pub name: String,
    pub params: Vec<Param>,
    pub return_type: Type,
    pub body: Option<Vec<TypedStatement>>,
    pub is_pub: bool,
    pub is_export: bool,
    pub is_extern: bool,
}
#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    pub name: String,
    pub ty: Type,
    pub is_comptime: bool,
}
#[derive(Debug, Clone, PartialEq)]
pub struct ConstDecl {
    pub name: String,
    pub ty: Option<Type>,
    pub value: TypedExpression,
    pub is_pub: bool,
}
#[derive(Debug, Clone, PartialEq)]
pub struct VarDecl {
    pub name: String,
    pub ty: Option<Type>,
    pub value: Option<TypedExpression>,
    pub is_pub: bool,
}
#[derive(Debug, Clone, PartialEq)]
pub struct StructDecl {
    pub name: String,
    pub fields: Vec<FieldDecl>,
    pub is_packed: bool,
}
#[derive(Debug, Clone, PartialEq)]
pub struct FieldDecl {
    pub name: String,
    pub ty: Type,
    pub default: Option<TypedExpression>,
}
#[derive(Debug, Clone, PartialEq)]
pub struct EnumDecl {
    pub name: String,
    pub tag_type: Option<Type>,
    pub variants: Vec<EnumVariant>,
}
#[derive(Debug, Clone, PartialEq)]
pub struct EnumVariant {
    pub name: String,
    pub value: Option<TypedExpression>,
}
#[derive(Debug, Clone, PartialEq)]
pub struct UnionDecl {
    pub name: String,
    pub is_tagged: bool,
    pub fields: Vec<UnionField>,
}
#[derive(Debug, Clone, PartialEq)]
pub struct UnionField {
    pub name: String,
    pub ty: Type,
}
#[derive(Debug, Clone, PartialEq)]
pub struct ErrorSetDecl {
    pub name: String,
    pub errors: Vec<String>,
}
#[derive(Debug, Clone, PartialEq)]
pub struct ImportDecl {
    pub path: String,
}
#[doc = r" A complete program"]
#[derive(Debug, Clone, PartialEq, Default)]
pub struct TypedProgram {
    pub declarations: Vec<TypedDeclaration>,
    pub span: Span,
}
#[doc = r" Create span from two HasSpan items"]
pub fn make_span(start_pair: impl HasSpan, end_pair: impl HasSpan) -> Span {
    Span::merge(&start_pair.span(), &end_pair.span())
}
pub trait HasSpan {
    fn span(&self) -> Span;
}
impl HasSpan for Span {
    fn span(&self) -> Span {
        *self
    }
}
impl HasSpan for TypedExpression {
    fn span(&self) -> Span {
        self.span
    }
}
impl HasSpan for TypedStatement {
    fn span(&self) -> Span {
        self.span
    }
}
impl HasSpan for TypedDeclaration {
    fn span(&self) -> Span {
        self.span
    }
}
#[doc = r" Intern a string (stub - just returns the string)"]
pub fn intern(s: &str) -> String {
    s.to_string()
}
#[doc = r" Parse integer from string"]
pub fn parse_int(s: &str) -> i64 {
    s.parse().unwrap_or(0)
}
#[doc = r" Parse float from string"]
pub fn parse_float(s: &str) -> f64 {
    s.parse().unwrap_or(0.0)
}
#[doc = r" Parse error"]
#[derive(Debug)]
pub struct ParseError(pub String);
impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
impl std::error::Error for ParseError {}
impl<R: pest::RuleType> From<pest::error::Error<R>> for ParseError {
    fn from(e: pest::error::Error<R>) -> Self {
        ParseError(e.to_string())
    }
}
#[doc = r" Alias for Param - used in action blocks as TypedParam"]
pub type TypedParam = Param;
#[doc = r" Alias for FieldDecl - used in action blocks as TypedField"]
pub type TypedField = FieldDecl;
#[doc = r" Alias for FnDecl - used in action blocks as FunctionDecl"]
pub type FunctionDecl = FnDecl;
#[doc = r" TypedBlock - wrapper for a block of statements"]
#[derive(Debug, Clone, PartialEq, Default)]
pub struct TypedBlock {
    pub statements: Vec<TypedStatement>,
    pub span: Span,
}
#[doc = r" TypedLiteral for literal values"]
#[derive(Debug, Clone, PartialEq)]
pub enum TypedLiteral {
    Int(i64),
    Float(f64),
    String(String),
    Char(char),
    Bool(bool),
    Null,
    Undefined,
}
#[doc = r" Visibility modifier for declarations"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Visibility {
    #[default]
    Private,
    Public,
}
#[doc = r" Pattern for pattern matching"]
#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Wildcard,
    Binding(String),
    Literal(TypedLiteral),
    Variant(String, Box<Pattern>),
    Tuple(Vec<Pattern>),
    Struct(String, Vec<(String, Pattern)>),
}
impl Default for Pattern {
    fn default() -> Self {
        Pattern::Wildcard
    }
}
#[doc = r" Match arm for switch/match expressions"]
#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub body: TypedExpression,
}
impl Span {
    #[doc = r" Create a Span from a pest::Span"]
    pub fn from_pest<'i>(pest_span: pest::Span<'i>) -> Self {
        Self {
            start: pest_span.start(),
            end: pest_span.end(),
        }
    }
}
#[doc = r" Infer type from an expression (stub)"]
pub fn infer_type<T>(_expr: T) -> Type {
    Type::Unknown
}
#[doc = r" Parse assignment operator from string"]
pub fn parse_assign_op<T>(_op: T) -> AssignOp {
    AssignOp::Assign
}
#[doc = r" Parse equality operator"]
pub fn parse_eq_op<T>(_op: T) -> BinaryOp {
    BinaryOp::Eq
}
#[doc = r" Parse comparison operator"]
pub fn parse_cmp_op<T>(_op: T) -> BinaryOp {
    BinaryOp::Lt
}
#[doc = r" Parse shift operator"]
pub fn parse_shift_op<T>(_op: T) -> BinaryOp {
    BinaryOp::Shl
}
#[doc = r" Parse add operator"]
pub fn parse_add_op<T>(_op: T) -> BinaryOp {
    BinaryOp::Add
}
#[doc = r" Parse mul operator"]
pub fn parse_mul_op<T>(_op: T) -> BinaryOp {
    BinaryOp::Mul
}
#[doc = r" Parse unary operator"]
pub fn parse_unary_op<T>(_op: T) -> UnaryOp {
    UnaryOp::Neg
}
#[doc = r" Fold binary operations"]
pub fn fold_binary<T, U>(_first: T, _rest: U, _default_op: BinaryOp) -> TypedExpression {
    TypedExpression::default()
}
#[doc = r" Fold postfix operations"]
pub fn fold_postfix<T, U>(_base: T, _ops: U) -> TypedExpression {
    TypedExpression::default()
}
#[doc = r" Collect switch cases"]
pub fn collect_cases<T, U>(_scrutinee: T, _cases: U) -> Vec<(Pattern, TypedExpression)> {
    vec![]
}
#[doc = r" Infer switch result type"]
pub fn infer_switch_type<T, U>(_scrutinee: T, _cases: U) -> Type {
    Type::Unknown
}
#[doc = r" Collect struct fields"]
pub fn collect_fields<T, U>(_first: T, _rest: U) -> Vec<(String, TypedExpression)> {
    vec![]
}
#[doc = r" Assignment operators"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum AssignOp {
    #[default]
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
    BitAndAssign,
    BitOrAssign,
    BitXorAssign,
    ShlAssign,
    ShrAssign,
}
#[doc = r" Error type"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Error;
impl Type {
    #[doc = r" The Error type constant"]
    #[allow(non_upper_case_globals)]
    pub const Error: Type = Type::Unknown;
    #[doc = r" The Type type constant (for comptime)"]
    #[allow(non_upper_case_globals)]
    pub const Type: Type = Type::Unknown;
}
#[doc = r" Assignment statement (can be used directly in stmt field)"]
#[derive(Debug, Clone, PartialEq, Default)]
pub struct Assignment {
    pub target: TypedExpression,
    pub op: AssignOp,
    pub value: TypedExpression,
}
impl From<Assignment> for Statement {
    fn from(a: Assignment) -> Self {
        Statement::Assign(a.target, a.value)
    }
}
#[doc = r" Return statement"]
#[derive(Debug, Clone, PartialEq, Default)]
pub struct Return {
    pub value: Option<TypedExpression>,
}
impl From<Return> for Statement {
    fn from(r: Return) -> Self {
        Statement::Return(r.value)
    }
}
#[doc = r" Expression statement"]
#[derive(Debug, Clone, PartialEq, Default)]
pub struct ExprStmt {
    pub expr: TypedExpression,
}
impl From<ExprStmt> for Statement {
    fn from(e: ExprStmt) -> Self {
        Statement::Expression(e.expr)
    }
}
#[doc = r" Match statement (pattern matching)"]
#[derive(Debug, Clone, PartialEq)]
pub struct Match {
    pub scrutinee: TypedExpression,
    pub arms: Vec<MatchArm>,
}
impl Default for Match {
    fn default() -> Self {
        Self {
            scrutinee: TypedExpression::default(),
            arms: vec![],
        }
    }
}
#[doc = r" If statement"]
#[derive(Debug, Clone, PartialEq, Default)]
pub struct If {
    pub condition: TypedExpression,
    pub then_branch: TypedBlock,
    pub else_branch: Option<TypedBlock>,
}
#[doc = r" While statement"]
#[derive(Debug, Clone, PartialEq, Default)]
pub struct While {
    pub condition: TypedExpression,
    pub body: TypedBlock,
}
#[doc = r" Break statement (unit struct for direct use)"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Break;
#[doc = r" Continue statement (unit struct for direct use)"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Continue;
#[doc = r" Literal expression"]
#[derive(Debug, Clone, PartialEq)]
pub struct Literal(pub TypedLiteral);
#[doc = r" UnaryOp expression (for action blocks)"]
#[derive(Debug, Clone, PartialEq)]
pub struct UnaryOpExpr(pub UnaryOp, pub Box<TypedExpression>);
#[doc = r" Array expression"]
#[derive(Debug, Clone, PartialEq)]
pub struct Array(pub Vec<TypedExpression>);
#[doc = r" Struct literal expression"]
#[derive(Debug, Clone, PartialEq)]
pub struct StructLiteral {
    pub name: String,
    pub fields: Vec<(String, TypedExpression)>,
}
#[doc = r" Lambda expression"]
#[derive(Debug, Clone, PartialEq)]
pub struct Lambda {
    pub params: Vec<TypedParam>,
    pub body: TypedBlock,
    pub captures: Vec<String>,
}
impl Default for Lambda {
    fn default() -> Self {
        Self {
            params: vec![],
            body: TypedBlock::default(),
            captures: vec![],
        }
    }
}
#[doc = r" Try expression"]
#[derive(Debug, Clone, PartialEq)]
pub struct Try {
    pub expr: Box<TypedExpression>,
}
impl Default for Try {
    fn default() -> Self {
        Self {
            expr: Box::new(TypedExpression::default()),
        }
    }
}
#[doc = r" Switch expression"]
#[derive(Debug, Clone, PartialEq)]
pub struct Switch {
    pub scrutinee: Box<TypedExpression>,
    pub cases: Vec<(Pattern, TypedExpression)>,
}
impl Default for Switch {
    fn default() -> Self {
        Self {
            scrutinee: Box::new(TypedExpression::default()),
            cases: vec![],
        }
    }
}
#[doc = r" Construct Expression::UnaryOp variant"]
#[doc = r" Note: Uses self:: prefix because this function shadows the UnaryOp enum"]
#[allow(non_snake_case)]
pub fn UnaryOp(op: self::UnaryOp, expr: Box<TypedExpression>) -> Expression {
    Expression::UnaryOp(op, expr)
}
#[doc = r" Construct Expression::BinaryOp variant"]
#[doc = r" Note: Uses self:: prefix because this function shadows the BinaryOp enum"]
#[allow(non_snake_case)]
pub fn BinaryOp(
    op: self::BinaryOp,
    left: Box<TypedExpression>,
    right: Box<TypedExpression>,
) -> Expression {
    Expression::BinaryOp(op, left, right)
}
#[doc = r" Construct Expression::IntLiteral variant"]
#[allow(non_snake_case)]
pub fn IntLiteral(n: i64) -> Expression {
    Expression::IntLiteral(n)
}
#[doc = r" Construct Expression::FloatLiteral variant"]
#[allow(non_snake_case)]
pub fn FloatLiteral(n: f64) -> Expression {
    Expression::FloatLiteral(n)
}
#[doc = r" Construct Expression::StringLiteral variant"]
#[allow(non_snake_case)]
pub fn StringLiteral(s: String) -> Expression {
    Expression::StringLiteral(s)
}
#[doc = r" Construct Expression::Identifier variant"]
#[allow(non_snake_case)]
pub fn Identifier(name: String) -> Expression {
    Expression::Identifier(name)
}
