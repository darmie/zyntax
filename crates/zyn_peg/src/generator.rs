//! Code generator for ZynPEG
//!
//! Generates Rust code from .zyn grammars:
//! 1. A pest parser (from the PEG patterns)
//! 2. A TypedAST builder (from the action blocks)

use proc_macro2::TokenStream;
use quote::{quote, format_ident};
use std::process::{Command, Stdio};
use std::io::Write;
use crate::{ZynGrammar, RuleDef, RuleModifier};
use crate::error::{Result, ZynPegError};

/// Generate complete Rust code from a ZynGrammar
pub fn generate_parser(grammar: &ZynGrammar) -> Result<GeneratedCode> {
    let pest_grammar = generate_pest_grammar(grammar)?;
    let ast_builder = generate_ast_builder(grammar)?;
    let parser_impl = generate_parser_impl(grammar)?;

    Ok(GeneratedCode {
        pest_grammar,
        ast_builder,
        parser_impl,
        typed_ast_types: None,
    })
}

/// Generated code components
pub struct GeneratedCode {
    /// The .pest grammar file content
    pub pest_grammar: String,
    /// The TypedAST builder Rust code (TokenStream)
    pub ast_builder: TokenStream,
    /// The parser implementation with parse_to_typed_ast method (TokenStream)
    pub parser_impl: TokenStream,
    /// Optional standalone TypedAST types module (TokenStream)
    pub typed_ast_types: Option<TokenStream>,
}

impl GeneratedCode {
    /// Get formatted AST builder code using rustfmt
    pub fn ast_builder_formatted(&self) -> String {
        format_rust_code(&self.ast_builder.to_string())
    }

    /// Get formatted parser impl code using rustfmt
    pub fn parser_impl_formatted(&self) -> String {
        format_rust_code(&self.parser_impl.to_string())
    }

    /// Get formatted TypedAST types code using rustfmt
    pub fn typed_ast_types_formatted(&self) -> Option<String> {
        self.typed_ast_types.as_ref().map(|ts| format_rust_code(&ts.to_string()))
    }
}

/// Generate complete standalone parser with TypedAST types
///
/// This generates a complete, self-contained parser that doesn't depend on
/// external type crates. It includes:
/// - typed_ast.rs: Complete TypedAST type definitions
/// - ast_builder.rs: Parser actions that build TypedAST
/// - parser_impl.rs: Convenience parse functions
pub fn generate_standalone_parser(grammar: &ZynGrammar) -> Result<GeneratedCode> {
    let pest_grammar = generate_pest_grammar(grammar)?;
    let ast_builder = generate_standalone_ast_builder(grammar)?;
    let parser_impl = generate_standalone_parser_impl(grammar)?;
    let typed_ast_types = Some(generate_typed_ast_types(grammar)?);

    Ok(GeneratedCode {
        pest_grammar,
        ast_builder,
        parser_impl,
        typed_ast_types,
    })
}

/// Generate the TypedAST types module for the grammar
///
/// This generates a complete, standalone TypedAST API including:
/// - Core types (Span, Type, Expression, Statement, Declaration)
/// - TypedProgram and related structures
/// - Helper functions for AST construction
fn generate_typed_ast_types(grammar: &ZynGrammar) -> Result<TokenStream> {
    let lang_name = &grammar.language.name;
    let lang_comment = format!("Generated for: {}", lang_name);

    Ok(quote! {
        //! ZynPEG TypedAST Types
        //!
        //! This module provides the complete TypedAST type system for the
        //! generated parser. These types form the semantic representation
        //! of parsed source code.
        //!
        #![doc = #lang_comment]

        #![allow(dead_code, unused_variables, unused_imports)]

        use std::collections::HashMap;

        // ============================================================
        // Core Types
        // ============================================================

        /// Span in source code
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

        /// Type representation (simplified for testing)
        #[derive(Debug, Clone, PartialEq)]
        pub enum Type {
            // Primitives
            I8, I16, I32, I64, I128,
            U8, U16, U32, U64, U128,
            F32, F64,
            Bool,
            Void,
            String,
            Char,
            Usize, Isize,

            // Compound types
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
            fn default() -> Self { Type::Unknown }
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

        // ============================================================
        // Expression Types
        // ============================================================

        /// Binary operations
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        pub enum BinaryOp {
            // Arithmetic
            Add, Sub, Mul, Div, Mod,
            // Comparison
            Eq, Ne, Lt, Le, Gt, Ge,
            // Logical
            And, Or,
            // Bitwise
            BitAnd, BitOr, BitXor, Shl, Shr,
            // Special (Zig-specific)
            Orelse, Catch,
        }

        /// Unary operations
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        pub enum UnaryOp {
            Neg, Not, Deref, AddrOf, Try, Await,
        }

        /// Postfix operations for expression chaining
        #[derive(Debug, Clone, PartialEq)]
        pub enum PostfixOp {
            Deref,
            Field(String),
            Index(TypedExpression),
            Call(Vec<TypedExpression>),
            OptionalUnwrap,
            TryUnwrap,
        }

        /// A typed expression
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

        /// Expression variants
        #[derive(Debug, Clone, PartialEq)]
        pub enum Expression {
            // Literals
            IntLiteral(i64),
            FloatLiteral(f64),
            StringLiteral(String),
            CharLiteral(char),
            BoolLiteral(bool),
            NullLiteral,
            UndefinedLiteral,

            // Variables and references
            Identifier(String),
            Variable(String, Type),

            // Operations
            BinaryOp(BinaryOp, Box<TypedExpression>, Box<TypedExpression>),
            UnaryOp(UnaryOp, Box<TypedExpression>),

            // Access
            FieldAccess(Box<TypedExpression>, String),
            Index(Box<TypedExpression>, Box<TypedExpression>),
            Deref(Box<TypedExpression>),

            // Calls
            Call(Box<TypedExpression>, Vec<TypedExpression>),
            MethodCall(Box<TypedExpression>, String, Vec<TypedExpression>),

            // Collections
            Array(Vec<TypedExpression>),
            Tuple(Vec<TypedExpression>),
            Struct(String, Vec<(String, TypedExpression)>),

            // Control flow as expression
            If(Box<TypedExpression>, Box<TypedExpression>, Option<Box<TypedExpression>>),
            Block(Vec<TypedStatement>),

            // Special
            Unit,
            Error(String),
        }

        // ============================================================
        // Statement Types
        // ============================================================

        /// A typed statement
        #[derive(Debug, Clone, PartialEq)]
        pub struct TypedStatement {
            pub stmt: Statement,
            pub span: Span,
        }

        /// Statement variants
        #[derive(Debug, Clone, PartialEq)]
        pub enum Statement {
            Let(String, Option<Type>, Option<TypedExpression>),
            Const(String, Option<Type>, TypedExpression),
            Assign(TypedExpression, TypedExpression),
            Expression(TypedExpression),
            Return(Option<TypedExpression>),
            If(TypedExpression, Vec<TypedStatement>, Option<Vec<TypedStatement>>),
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

        // ============================================================
        // Declaration Types
        // ============================================================

        /// A typed declaration
        #[derive(Debug, Clone, PartialEq)]
        pub struct TypedDeclaration {
            pub decl: Declaration,
            pub span: Span,
        }

        /// Declaration variants
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

        // ============================================================
        // Program
        // ============================================================

        /// A complete program
        #[derive(Debug, Clone, PartialEq, Default)]
        pub struct TypedProgram {
            pub declarations: Vec<TypedDeclaration>,
            pub span: Span,
        }

        // ============================================================
        // Helper Functions
        // ============================================================

        /// Create span from two HasSpan items
        pub fn make_span(start_pair: impl HasSpan, end_pair: impl HasSpan) -> Span {
            Span::merge(&start_pair.span(), &end_pair.span())
        }

        pub trait HasSpan {
            fn span(&self) -> Span;
        }

        impl HasSpan for Span {
            fn span(&self) -> Span { *self }
        }

        impl HasSpan for TypedExpression {
            fn span(&self) -> Span { self.span }
        }

        impl HasSpan for TypedStatement {
            fn span(&self) -> Span { self.span }
        }

        impl HasSpan for TypedDeclaration {
            fn span(&self) -> Span { self.span }
        }

        /// Intern a string (stub - just returns the string)
        pub fn intern(s: &str) -> String {
            s.to_string()
        }

        /// Parse integer from string
        pub fn parse_int(s: &str) -> i64 {
            s.parse().unwrap_or(0)
        }

        /// Parse float from string
        pub fn parse_float(s: &str) -> f64 {
            s.parse().unwrap_or(0.0)
        }

        // ============================================================
        // Error Types
        // ============================================================

        /// Parse error
        #[derive(Debug)]
        pub struct ParseError(pub String);

        impl std::fmt::Display for ParseError {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", self.0)
            }
        }

        impl std::error::Error for ParseError {}

        // Specific impl for pest errors to avoid blanket impl conflict
        impl<R: pest::RuleType> From<pest::error::Error<R>> for ParseError {
            fn from(e: pest::error::Error<R>) -> Self {
                ParseError(e.to_string())
            }
        }

        // ============================================================
        // Type Aliases for Compatibility with Action Blocks
        // ============================================================

        /// Alias for Param - used in action blocks as TypedParam
        pub type TypedParam = Param;

        /// Alias for FieldDecl - used in action blocks as TypedField
        pub type TypedField = FieldDecl;

        /// Alias for FnDecl - used in action blocks as FunctionDecl
        pub type FunctionDecl = FnDecl;

        /// TypedBlock - wrapper for a block of statements
        #[derive(Debug, Clone, PartialEq, Default)]
        pub struct TypedBlock {
            pub statements: Vec<TypedStatement>,
            pub span: Span,
        }

        /// TypedLiteral for literal values
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

        // ============================================================
        // Visibility
        // ============================================================

        /// Visibility modifier for declarations
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
        pub enum Visibility {
            #[default]
            Private,
            Public,
        }

        // ============================================================
        // Patterns for Pattern Matching
        // ============================================================

        /// Pattern for pattern matching
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
            fn default() -> Self { Pattern::Wildcard }
        }

        /// Match arm for switch/match expressions
        #[derive(Debug, Clone, PartialEq)]
        pub struct MatchArm {
            pub pattern: Pattern,
            pub body: TypedExpression,
        }

        // ============================================================
        // Span from pest
        // ============================================================

        impl Span {
            /// Create a Span from a pest::Span
            pub fn from_pest<'i>(pest_span: pest::Span<'i>) -> Self {
                Self {
                    start: pest_span.start(),
                    end: pest_span.end(),
                }
            }
        }

        // ============================================================
        // Helper Functions for Action Blocks
        // ============================================================

        /// Infer type from an expression (stub)
        pub fn infer_type<T>(_expr: T) -> Type {
            Type::Unknown
        }

        /// Parse assignment operator from string
        pub fn parse_assign_op<T>(_op: T) -> AssignOp {
            AssignOp::Assign
        }

        /// Parse equality operator
        pub fn parse_eq_op<T>(_op: T) -> BinaryOp {
            BinaryOp::Eq
        }

        /// Parse comparison operator
        pub fn parse_cmp_op<T>(_op: T) -> BinaryOp {
            BinaryOp::Lt
        }

        /// Parse shift operator
        pub fn parse_shift_op<T>(_op: T) -> BinaryOp {
            BinaryOp::Shl
        }

        /// Parse add operator
        pub fn parse_add_op<T>(_op: T) -> BinaryOp {
            BinaryOp::Add
        }

        /// Parse mul operator
        pub fn parse_mul_op<T>(_op: T) -> BinaryOp {
            BinaryOp::Mul
        }

        /// Parse unary operator
        pub fn parse_unary_op<T>(_op: T) -> UnaryOp {
            UnaryOp::Neg
        }

        /// Fold binary operations
        pub fn fold_binary<T, U>(_first: T, _rest: U, _default_op: BinaryOp) -> TypedExpression {
            TypedExpression::default()
        }

        /// Fold postfix operations
        pub fn fold_postfix<T, U>(_base: T, _ops: U) -> TypedExpression {
            TypedExpression::default()
        }

        /// Collect switch cases
        pub fn collect_cases<T, U>(_scrutinee: T, _cases: U) -> Vec<(Pattern, TypedExpression)> {
            vec![]
        }

        /// Infer switch result type
        pub fn infer_switch_type<T, U>(_scrutinee: T, _cases: U) -> Type {
            Type::Unknown
        }

        /// Collect struct fields
        pub fn collect_fields<T, U>(_first: T, _rest: U) -> Vec<(String, TypedExpression)> {
            vec![]
        }

        /// Assignment operators
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

        /// Error type
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
        pub struct Error;

        impl Type {
            /// The Error type constant
            #[allow(non_upper_case_globals)]
            pub const Error: Type = Type::Unknown;
            /// The Type type constant (for comptime)
            #[allow(non_upper_case_globals)]
            pub const Type: Type = Type::Unknown;
        }

        // ============================================================
        // Statement Struct Types (for direct use in action blocks)
        // ============================================================

        /// Assignment statement (can be used directly in stmt field)
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

        /// Return statement
        #[derive(Debug, Clone, PartialEq, Default)]
        pub struct Return {
            pub value: Option<TypedExpression>,
        }

        impl From<Return> for Statement {
            fn from(r: Return) -> Self {
                Statement::Return(r.value)
            }
        }

        /// Expression statement
        #[derive(Debug, Clone, PartialEq, Default)]
        pub struct ExprStmt {
            pub expr: TypedExpression,
        }

        impl From<ExprStmt> for Statement {
            fn from(e: ExprStmt) -> Self {
                Statement::Expression(e.expr)
            }
        }

        /// Match statement (pattern matching)
        #[derive(Debug, Clone, PartialEq)]
        pub struct Match {
            pub scrutinee: TypedExpression,
            pub arms: Vec<MatchArm>,
        }

        impl Default for Match {
            fn default() -> Self {
                Self { scrutinee: TypedExpression::default(), arms: vec![] }
            }
        }

        /// If statement
        #[derive(Debug, Clone, PartialEq, Default)]
        pub struct If {
            pub condition: TypedExpression,
            pub then_branch: TypedBlock,
            pub else_branch: Option<TypedBlock>,
        }

        /// While statement
        #[derive(Debug, Clone, PartialEq, Default)]
        pub struct While {
            pub condition: TypedExpression,
            pub body: TypedBlock,
        }

        /// Break statement (unit struct for direct use)
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
        pub struct Break;

        /// Continue statement (unit struct for direct use)
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
        pub struct Continue;

        // ============================================================
        // Expression Struct Types (for direct use in action blocks)
        // ============================================================

        /// Literal expression
        #[derive(Debug, Clone, PartialEq)]
        pub struct Literal(pub TypedLiteral);

        /// UnaryOp expression (for action blocks)
        #[derive(Debug, Clone, PartialEq)]
        pub struct UnaryOpExpr(pub UnaryOp, pub Box<TypedExpression>);

        /// Array expression
        #[derive(Debug, Clone, PartialEq)]
        pub struct Array(pub Vec<TypedExpression>);

        /// Struct literal expression
        #[derive(Debug, Clone, PartialEq)]
        pub struct StructLiteral {
            pub name: String,
            pub fields: Vec<(String, TypedExpression)>,
        }

        /// Lambda expression
        #[derive(Debug, Clone, PartialEq)]
        pub struct Lambda {
            pub params: Vec<TypedParam>,
            pub body: TypedBlock,
            pub captures: Vec<String>,
        }

        impl Default for Lambda {
            fn default() -> Self {
                Self { params: vec![], body: TypedBlock::default(), captures: vec![] }
            }
        }

        /// Try expression
        #[derive(Debug, Clone, PartialEq)]
        pub struct Try {
            pub expr: Box<TypedExpression>,
        }

        impl Default for Try {
            fn default() -> Self {
                Self { expr: Box::new(TypedExpression::default()) }
            }
        }

        /// Switch expression
        #[derive(Debug, Clone, PartialEq)]
        pub struct Switch {
            pub scrutinee: Box<TypedExpression>,
            pub cases: Vec<(Pattern, TypedExpression)>,
        }

        impl Default for Switch {
            fn default() -> Self {
                Self { scrutinee: Box::new(TypedExpression::default()), cases: vec![] }
            }
        }

        // ============================================================
        // Helper Constructors for Expression Variants
        // These allow action blocks to use short names like `UnaryOp(...)`
        // instead of `Expression::UnaryOp(...)`
        // ============================================================

        /// Construct Expression::UnaryOp variant
        /// Note: Uses self:: prefix because this function shadows the UnaryOp enum
        #[allow(non_snake_case)]
        pub fn UnaryOp(op: self::UnaryOp, expr: Box<TypedExpression>) -> Expression {
            Expression::UnaryOp(op, expr)
        }

        /// Construct Expression::BinaryOp variant
        /// Note: Uses self:: prefix because this function shadows the BinaryOp enum
        #[allow(non_snake_case)]
        pub fn BinaryOp(op: self::BinaryOp, left: Box<TypedExpression>, right: Box<TypedExpression>) -> Expression {
            Expression::BinaryOp(op, left, right)
        }

        /// Construct Expression::IntLiteral variant
        #[allow(non_snake_case)]
        pub fn IntLiteral(n: i64) -> Expression {
            Expression::IntLiteral(n)
        }

        /// Construct Expression::FloatLiteral variant
        #[allow(non_snake_case)]
        pub fn FloatLiteral(n: f64) -> Expression {
            Expression::FloatLiteral(n)
        }

        /// Construct Expression::StringLiteral variant
        #[allow(non_snake_case)]
        pub fn StringLiteral(s: String) -> Expression {
            Expression::StringLiteral(s)
        }

        /// Construct Expression::Identifier variant
        #[allow(non_snake_case)]
        pub fn Identifier(name: String) -> Expression {
            Expression::Identifier(name)
        }
    })
}

/// Format Rust code using rustfmt
/// Falls back to unformatted code if rustfmt is not available
pub fn format_rust_code(code: &str) -> String {
    // Try to run rustfmt
    let result = Command::new("rustfmt")
        .arg("--edition")
        .arg("2021")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn();

    match result {
        Ok(mut child) => {
            // Write code to stdin
            if let Some(mut stdin) = child.stdin.take() {
                let _ = stdin.write_all(code.as_bytes());
            }

            // Read formatted output
            match child.wait_with_output() {
                Ok(output) if output.status.success() => {
                    String::from_utf8(output.stdout).unwrap_or_else(|_| code.to_string())
                }
                _ => code.to_string(),
            }
        }
        Err(_) => {
            // rustfmt not available, return unformatted code
            code.to_string()
        }
    }
}

/// Generate a pest-compatible grammar from ZynGrammar rules
fn generate_pest_grammar(grammar: &ZynGrammar) -> Result<String> {
    let mut lines = Vec::new();

    // Add header comment
    lines.push(format!(
        "// Generated by ZynPEG from {}.zyn",
        grammar.language.name.to_lowercase()
    ));
    lines.push(String::new());

    // Generate each rule
    for rule in &grammar.rules {
        let modifier = match rule.modifier {
            Some(RuleModifier::Atomic) => "@",
            Some(RuleModifier::Silent) => "_",
            Some(RuleModifier::Compound) => "$",
            Some(RuleModifier::NonAtomic) => "!",
            None => "",
        };

        lines.push(format!(
            "{} = {}{{ {} }}",
            rule.name, modifier, rule.pattern
        ));
    }

    // Add standard whitespace/comment rules if not defined
    let has_whitespace = grammar.rules.iter().any(|r| r.name == "WHITESPACE");
    let has_comment = grammar.rules.iter().any(|r| r.name == "COMMENT");

    if !has_whitespace {
        lines.push(String::new());
        lines.push("WHITESPACE = _{ \" \" | \"\\t\" | \"\\n\" | \"\\r\" }".to_string());
    }

    if !has_comment {
        lines.push("COMMENT = _{ \"//\" ~ (!\"\\n\" ~ ANY)* ~ \"\\n\"? }".to_string());
    }

    Ok(lines.join("\n"))
}

/// Generate the TypedAST builder code
fn generate_ast_builder(grammar: &ZynGrammar) -> Result<TokenStream> {
    let imports = parse_imports(&grammar.imports.code);
    let context_fields = generate_context_fields(&grammar.context);
    let type_helpers = parse_type_helpers(&grammar.type_helpers.code);

    // Generate build methods for each rule with an action
    let build_methods: Vec<TokenStream> = grammar.rules.iter()
        .filter(|r| r.action.is_some())
        .map(|r| generate_build_method(r, grammar))
        .collect::<Result<Vec<_>>>()?;

    Ok(quote! {
        // Generated imports
        #imports

        /// TypedAST builder context
        pub struct AstBuilderContext<'a> {
            #context_fields
        }

        impl<'a> AstBuilderContext<'a> {
            #type_helpers

            #(#build_methods)*
        }
    })
}

/// Generate the parser implementation
fn generate_parser_impl(grammar: &ZynGrammar) -> Result<TokenStream> {
    let parser_name = format_ident!("{}Parser", to_pascal_case(&grammar.language.name));
    let grammar_file = format!("{}.pest", grammar.language.name.to_lowercase());

    Ok(quote! {
        use pest_derive::Parser;

        #[derive(Parser)]
        #[grammar = #grammar_file]
        pub struct #parser_name;

        impl #parser_name {
            /// Parse source code to TypedAST
            pub fn parse_to_typed_ast(
                input: &str,
                arena: &mut AstArena,
                type_registry: &mut TypeRegistry,
            ) -> Result<TypedProgram, ParseError> {
                use pest::Parser;

                // Parse with pest
                let pairs = Self::parse(Rule::program, input)?;

                // Build TypedAST
                let mut ctx = AstBuilderContext { arena, type_registry };
                ctx.build_program(pairs)
            }
        }
    })
}

/// Generate standalone AST builder (no external dependencies)
///
/// This version doesn't include the grammar's @imports - instead it expects
/// types to be provided by the typed_ast module (scaffolding).
fn generate_standalone_ast_builder(grammar: &ZynGrammar) -> Result<TokenStream> {
    let type_helpers = parse_type_helpers(&grammar.type_helpers.code);

    // Generate build methods for rules WITH actions
    let action_methods: Vec<TokenStream> = grammar.rules.iter()
        .filter(|r| r.action.is_some())
        .map(|r| generate_build_method(r, grammar))
        .collect::<Result<Vec<_>>>()?;

    // Generate dispatch methods for rules WITHOUT actions (like declaration, type_expr, etc.)
    let dispatch_methods: Vec<TokenStream> = grammar.rules.iter()
        .filter(|r| r.action.is_none())
        .filter(|r| !r.name.starts_with("WHITESPACE") && !r.name.starts_with("COMMENT"))
        .filter(|r| r.modifier != Some(RuleModifier::Silent) && r.modifier != Some(RuleModifier::Atomic))
        .map(|r| generate_dispatch_method(r, grammar))
        .collect();

    // Standalone: no external imports, uses generated typed_ast types
    Ok(quote! {
        //! Generated AST builder
        //!
        //! This file is auto-generated by ZynPEG. Do not edit manually.

        #![allow(dead_code, unused_variables, unused_imports, unused_mut)]

        use super::typed_ast::*;
        use super::Rule;  // Import the Rule enum from the parser

        // Type helper for folding postfix operations
        fn fold_postfix_ops(base: TypedExpression, ops: Vec<PostfixOp>) -> TypedExpression {
            ops.into_iter().fold(base, |expr, op| {
                let span = expr.span;
                match op {
                    PostfixOp::Deref => TypedExpression {
                        ty: expr.ty.deref(),
                        expr: Expression::Deref(Box::new(expr)),
                        span,
                    },
                    PostfixOp::Field(name) => TypedExpression {
                        ty: Type::Unknown,
                        expr: Expression::FieldAccess(Box::new(expr), name),
                        span,
                    },
                    PostfixOp::Index(index) => TypedExpression {
                        ty: expr.ty.element_type(),
                        expr: Expression::Index(Box::new(expr), Box::new(index)),
                        span,
                    },
                    PostfixOp::Call(args) => TypedExpression {
                        ty: expr.ty.return_type(),
                        expr: Expression::Call(Box::new(expr), args),
                        span,
                    },
                    PostfixOp::OptionalUnwrap => TypedExpression {
                        ty: expr.ty.unwrap_optional(),
                        expr: Expression::UnaryOp(UnaryOp::Try, Box::new(expr)),
                        span,
                    },
                    PostfixOp::TryUnwrap => TypedExpression {
                        ty: expr.ty.unwrap_result(),
                        expr: Expression::UnaryOp(UnaryOp::Try, Box::new(expr)),
                        span,
                    },
                }
            })
        }

        /// TypedAST builder context
        pub struct AstBuilderContext;

        impl AstBuilderContext {
            pub fn new() -> Self {
                Self
            }

            #type_helpers

            #(#action_methods)*

            #(#dispatch_methods)*
        }
    })
}

/// Generate a dispatch method for a rule without an action
/// These methods dispatch to child rules based on match
fn generate_dispatch_method(rule: &RuleDef, grammar: &ZynGrammar) -> TokenStream {
    let method_name = format_ident!("build_{}", rule.name);

    // Parse the pattern to find child rules
    let child_rules = extract_child_rules(&rule.pattern);

    // Filter to rules that exist in grammar AND have build methods
    // (i.e., rules with actions OR non-atomic rules without actions that get dispatch methods)
    let dispatchable: Vec<_> = child_rules.iter()
        .filter(|name| {
            grammar.rules.iter().any(|r| {
                &r.name == *name && (
                    // Has an action - will have a build method
                    r.action.is_some() ||
                    // Non-atomic rule without action - will get a dispatch method
                    (r.modifier != Some(RuleModifier::Atomic) && r.modifier != Some(RuleModifier::Silent))
                )
            })
        })
        .collect();

    // Determine return type based on first child with an action
    let return_type_str = dispatchable.iter()
        .find_map(|name| {
            grammar.rules.iter()
                .find(|r| &r.name == *name)
                .and_then(|r| r.action.as_ref())
                .map(|a| a.return_type.clone())
        })
        .unwrap_or_else(|| "TypedExpression".to_string());

    let return_type: TokenStream = return_type_str.parse()
        .unwrap_or_else(|_| quote! { TypedExpression });

    if dispatchable.is_empty() {
        // No child rules to dispatch to - return a default/passthrough
        quote! {
            pub fn #method_name(&mut self, pair: pest::iterators::Pair<Rule>) -> Result<TypedExpression, ParseError> {
                let span = Span::from_pest(pair.as_span());
                let text = pair.as_str().trim();

                // Try to interpret as literal or identifier
                if let Ok(n) = text.parse::<i64>() {
                    return Ok(TypedExpression {
                        expr: Expression::IntLiteral(n),
                        ty: Type::I64,
                        span,
                    });
                }
                if let Ok(n) = text.parse::<f64>() {
                    return Ok(TypedExpression {
                        expr: Expression::FloatLiteral(n),
                        ty: Type::F64,
                        span,
                    });
                }
                if text == "true" {
                    return Ok(TypedExpression {
                        expr: Expression::BoolLiteral(true),
                        ty: Type::Bool,
                        span,
                    });
                }
                if text == "false" {
                    return Ok(TypedExpression {
                        expr: Expression::BoolLiteral(false),
                        ty: Type::Bool,
                        span,
                    });
                }

                Ok(TypedExpression {
                    expr: Expression::Identifier(text.to_string()),
                    ty: Type::Unknown,
                    span,
                })
            }
        }
    } else {
        // Generate match arms for child rules
        let match_arms: Vec<TokenStream> = dispatchable.iter().map(|child_name| {
            let rule_variant = format_ident!("{}", child_name);
            let build_method = format_ident!("build_{}", child_name);
            quote! {
                Rule::#rule_variant => self.#build_method(inner),
            }
        }).collect();

        quote! {
            pub fn #method_name(&mut self, pair: pest::iterators::Pair<Rule>) -> Result<#return_type, ParseError> {
                let span = Span::from_pest(pair.as_span());
                if let Some(inner) = pair.into_inner().next() {
                    match inner.as_rule() {
                        #(#match_arms)*
                        _ => Err(ParseError(format!("Unexpected rule in {}: {:?}", stringify!(#method_name), inner.as_rule()))),
                    }
                } else {
                    Err(ParseError(format!("Empty {} rule", stringify!(#method_name))))
                }
            }
        }
    }
}

/// Extract child rule names from a pattern
fn extract_child_rules(pattern: &str) -> Vec<String> {
    let mut rules = Vec::new();

    // Simple extraction - find lowercase identifiers that aren't keywords
    let keywords = ["SOI", "EOI", "ANY", "ASCII", "ASCII_DIGIT", "ASCII_ALPHA", "ASCII_ALPHANUMERIC", "WHITESPACE", "COMMENT"];

    for part in pattern.split(|c: char| !c.is_alphanumeric() && c != '_') {
        let part = part.trim();
        if !part.is_empty()
            && part.chars().next().map(|c| c.is_lowercase()).unwrap_or(false)
            && !keywords.contains(&part)
        {
            if !rules.contains(&part.to_string()) {
                rules.push(part.to_string());
            }
        }
    }

    rules
}

/// Generate standalone parser implementation (simpler, no external types)
fn generate_standalone_parser_impl(_grammar: &ZynGrammar) -> Result<TokenStream> {
    Ok(quote! {
        //! Generated parser implementation
        //!
        //! This file is auto-generated by ZynPEG. Do not edit manually.

        #![allow(unused_imports)]

        // Note: The actual parser struct is defined in lib.rs using pest_derive
        // This file just provides the parse_to_typed_ast helper.

        use super::typed_ast::*;
        use super::ast_builder::AstBuilderContext;
        use super::Rule;
        use pest::Parser;

        /// Parse source code to TypedProgram
        pub fn parse_to_typed_ast<P: pest::Parser<Rule>>(
            input: &str,
        ) -> Result<TypedProgram, ParseError> {
            // Parse with pest
            let pairs = P::parse(Rule::program, input)?;

            // Build TypedAST
            let mut ctx = AstBuilderContext::new();
            for pair in pairs {
                if pair.as_rule() == Rule::program {
                    return ctx.build_program(pair);
                }
            }

            Err(ParseError("No program rule found".to_string()))
        }
    })
}

/// Generate a build method for a rule with an action
fn generate_build_method(rule: &RuleDef, _grammar: &ZynGrammar) -> Result<TokenStream> {
    let action = rule.action.as_ref()
        .ok_or_else(|| ZynPegError::InvalidAction("No action for rule".into()))?;

    let method_name = format_ident!("build_{}", rule.name);
    let return_type: TokenStream = action.return_type.parse()
        .map_err(|e| ZynPegError::CodeGenError(format!("Invalid return type: {}", e)))?;

    // Analyze the rule pattern to understand what children exist
    let pattern_info = analyze_pattern(&rule.pattern);

    // Generate child extraction code based on pattern analysis
    let child_extraction = generate_child_extraction(&pattern_info);

    // Check if this is a raw code action or structured fields
    let body = if let Some(ref raw_code) = action.raw_code {
        // Raw code action - transform captures to proper variable access
        let code = transform_captures_to_vars(raw_code, &pattern_info);
        let code_tokens: TokenStream = code.parse()
            .unwrap_or_else(|_| quote! { todo!("Failed to parse raw code") });
        quote! { { #code_tokens } }
    } else {
        // Structured field assignments
        let field_assignments: Vec<TokenStream> = action.fields.iter()
            .map(|f| {
                let name = format_ident!("{}", f.name);
                let value = transform_captures_to_vars(&f.value, &pattern_info);
                let value_tokens: TokenStream = value.parse()
                    .unwrap_or_else(|_| quote! { todo!("Failed to parse field value") });
                quote! { #name: #value_tokens }
            })
            .collect();

        quote! {
            #return_type {
                #(#field_assignments,)*
            }
        }
    };

    let child_extraction_tokens: TokenStream = child_extraction.parse()
        .unwrap_or_else(|_| quote! {});

    Ok(quote! {
        /// Build a #return_type from a parsed rule
        pub fn #method_name(&mut self, pair: pest::iterators::Pair<Rule>) -> Result<#return_type, ParseError> {
            let span = Span::from_pest(pair.as_span());
            let pair_str = pair.as_str();  // Capture text before consuming pair
            let mut children = pair.into_inner().peekable();

            #child_extraction_tokens

            Ok(#body)
        }
    })
}

/// Information about a pattern element
#[derive(Debug, Clone)]
struct PatternElement {
    index: usize,           // 1-based index in the pattern
    kind: PatternKind,
    name: Option<String>,   // Named element (identifier, type_expr, etc.)
    optional: bool,         // Wrapped in (...)? or includes ?
    repeated: bool,         // Wrapped in (...)* or (...)+
}

#[derive(Debug, Clone)]
enum PatternKind {
    Literal(String),        // "const", ";", etc.
    Rule(String),           // identifier, expr, type_expr, etc.
    Builtin(String),        // ASCII_DIGIT, ANY, etc.
}

/// Split a pattern by top-level alternation `|`, respecting parentheses
///
/// "a ~ b | c" -> ["a ~ b", "c"]
/// "(a | b) ~ c" -> ["(a | b) ~ c"]  (alternation inside parens is not top-level)
fn split_top_level_alternation(pattern: &str) -> Vec<String> {
    let mut branches = Vec::new();
    let mut depth = 0;
    let mut current = String::new();

    for ch in pattern.chars() {
        match ch {
            '(' | '[' | '{' => {
                depth += 1;
                current.push(ch);
            }
            ')' | ']' | '}' => {
                depth -= 1;
                current.push(ch);
            }
            '|' if depth == 0 => {
                if !current.trim().is_empty() {
                    branches.push(current.trim().to_string());
                }
                current.clear();
            }
            _ => {
                current.push(ch);
            }
        }
    }
    if !current.trim().is_empty() {
        branches.push(current.trim().to_string());
    }

    if branches.is_empty() {
        vec![pattern.to_string()]
    } else {
        branches
    }
}

/// Analyze a pattern to understand its structure
///
/// Groups like `(":" ~ type_expr)?` are treated as a single element that captures
/// the rule inside (type_expr). Literals inside groups are ignored for capture purposes.
///
/// For alternation patterns like `a ~ b | c`, we analyze the first branch (`a ~ b`)
/// since action blocks typically only reference elements from one branch at a time.
fn analyze_pattern(pattern: &str) -> Vec<PatternElement> {
    let mut elements = Vec::new();
    let mut index = 1;

    // First, handle top-level alternation - take first branch only
    // Pattern like "unary_op ~ unary | postfix" -> analyze "unary_op ~ unary"
    let branches = split_top_level_alternation(pattern);
    let pattern = branches.first().map(|s| s.as_str()).unwrap_or(pattern);

    // Split by ~ at the top level (respecting parentheses depth AND quoted strings)
    let mut depth = 0;
    let mut in_string = false;
    let mut current = String::new();
    let mut parts = Vec::new();
    let mut prev_char = ' ';

    for ch in pattern.chars() {
        match ch {
            '"' if prev_char != '\\' => {
                in_string = !in_string;
                current.push(ch);
            }
            '(' if !in_string => {
                depth += 1;
                current.push(ch);
            }
            ')' if !in_string => {
                depth -= 1;
                current.push(ch);
            }
            '~' if depth == 0 && !in_string => {
                if !current.trim().is_empty() {
                    parts.push(current.trim().to_string());
                }
                current.clear();
            }
            _ => {
                current.push(ch);
            }
        }
        prev_char = ch;
    }
    if !current.trim().is_empty() {
        parts.push(current.trim().to_string());
    }

    for part in parts {
        let part = part.trim();
        if part.is_empty() {
            continue;
        }

        // Check modifiers
        let optional = part.ends_with('?') || part.ends_with(")?");
        let repeated = part.ends_with('*') || part.ends_with('+') || part.ends_with(")*") || part.ends_with(")+");

        // Check if this is a group
        if part.starts_with('(') && (part.ends_with(')') || part.ends_with(")?") || part.ends_with(")*") || part.ends_with(")+")) {
            // For a group, find the rule inside (ignoring literals)
            let inner = if part.ends_with(")?") || part.ends_with(")*") || part.ends_with(")+") {
                &part[1..part.len()-2]
            } else {
                &part[1..part.len()-1]
            };

            // Handle alternation inside groups - take first branch
            let inner_branches = split_top_level_alternation(inner);
            let inner = inner_branches.first().map(|s| s.as_str()).unwrap_or(inner);

            // Find the rule reference inside the group (skip literals)
            let inner_parts: Vec<&str> = inner.split('~').map(|s| s.trim()).collect();
            let mut found_rule = None;
            for inner_part in inner_parts {
                let clean = inner_part.trim_end_matches(['?', '*', '+']);
                if !clean.starts_with('"') && !clean.starts_with('\'') && !clean.is_empty() {
                    if !clean.chars().next().map(|c| c.is_uppercase()).unwrap_or(false) {
                        found_rule = Some(clean.to_string());
                        break;
                    }
                }
            }

            if let Some(rule_name) = found_rule {
                elements.push(PatternElement {
                    index,
                    kind: PatternKind::Rule(rule_name),
                    name: None,
                    optional,
                    repeated,
                });
            } else {
                // Group with only literals - still counts as a position but no capture
                elements.push(PatternElement {
                    index,
                    kind: PatternKind::Literal("group".to_string()),
                    name: None,
                    optional,
                    repeated,
                });
            }
            index += 1;
        } else {
            // Single element
            let clean = part.trim_end_matches(['?', '*', '+']);

            let kind = if clean.starts_with('"') || clean.starts_with('\'') {
                // String literal
                let literal = clean.trim_matches(|c| c == '"' || c == '\'');
                PatternKind::Literal(literal.to_string())
            } else if clean.chars().next().map(|c| c.is_uppercase()).unwrap_or(false) {
                // Builtin (ASCII_DIGIT, ANY, etc.)
                PatternKind::Builtin(clean.to_string())
            } else if !clean.is_empty() {
                // Rule reference
                PatternKind::Rule(clean.to_string())
            } else {
                index += 1;
                continue;
            };

            elements.push(PatternElement {
                index,
                kind,
                name: None,
                optional,
                repeated,
            });
            index += 1;
        }
    }

    elements
}

/// Sanitize a rule name to be a valid Rust identifier
fn sanitize_rule_name(name: &str) -> String {
    // Remove any alternation or sequence operators, take first valid identifier
    let name = name.split('|').next().unwrap_or(name).trim();
    let name = name.split('~').next().unwrap_or(name).trim();
    // Remove parentheses and modifiers
    let name = name.trim_matches(|c| c == '(' || c == ')' || c == '?' || c == '*' || c == '+' || c == '"' || c == '\'' || c == ' ');
    // Replace any remaining invalid chars with underscore
    name.chars()
        .map(|c| if c.is_alphanumeric() || c == '_' { c } else { '_' })
        .collect::<String>()
        .trim_matches('_')
        .to_string()
}

/// Generate code to extract children from the parse tree
///
/// IMPORTANT: pest does NOT capture string literals as children.
/// Only named rules become children in the parse tree.
///
/// This function now generates simpler code that collects all children
/// and provides access by rule type, avoiding complex position mapping.
fn generate_child_extraction(pattern: &[PatternElement]) -> String {
    // Collect unique rule names from the pattern
    let mut rule_names: Vec<(String, bool, bool)> = Vec::new(); // (name, optional, repeated)

    for elem in pattern {
        if let PatternKind::Rule(name) = &elem.kind {
            let sanitized = sanitize_rule_name(name);
            if sanitized.is_empty() {
                continue;
            }
            // Avoid duplicates
            if !rule_names.iter().any(|(n, _, _)| n == &sanitized) {
                rule_names.push((sanitized, elem.optional, elem.repeated));
            }
        }
    }

    let mut code = String::new();
    code.push_str("let all_children: Vec<_> = children.collect();\n            ");
    code.push_str("let mut child_iter = all_children.iter();\n            ");

    for (name, optional, repeated) in &rule_names {
        let var_name = format!("child_{}", name);
        if *repeated {
            code.push_str(&format!(
                "let {}: Vec<_> = all_children.iter().filter(|p| p.as_rule() == Rule::{}).cloned().collect();\n            ",
                var_name, name
            ));
        } else if *optional {
            code.push_str(&format!(
                "let {} = all_children.iter().find(|p| p.as_rule() == Rule::{}).cloned();\n            ",
                var_name, name
            ));
        } else {
            code.push_str(&format!(
                "let {} = all_children.iter().find(|p| p.as_rule() == Rule::{}).cloned();\n            ",
                var_name, name
            ));
        }
    }

    code
}

/// Transform capture references ($1, $2, etc.) to proper variable access
///
/// Now uses rule-name-based variables (child_identifier, child_expr, etc.)
/// instead of position-based ones, which is more reliable.
fn transform_captures_to_vars(value: &str, pattern: &[PatternElement]) -> String {
    let mut result = value.to_string();

    // First, replace span($X, $Y) patterns with just `span`
    // since we pre-compute span at the start of each build method
    // Use simple string search to find and replace span(...) calls
    while let Some(start) = result.find("span(") {
        if let Some(end) = result[start..].find(')') {
            let end_pos = start + end + 1;
            result = format!("{}{}{}", &result[..start], "span", &result[end_pos..]);
        } else {
            break;
        }
    }

    // First, flatten groups to get the true element order
    // For pattern like: "const" ~ identifier ~ (":" ~ type_expr)? ~ "=" ~ expr ~ ";"
    // The effective positions are:
    // $1 = "const" (literal)
    // $2 = identifier
    // $3 = type_expr (inside the group)
    // $4 = "=" (literal)
    // $5 = expr
    // $6 = ";" (literal)

    // Build a mapping from position to (rule_name, is_optional, is_repeated)
    let mut position_to_rule: Vec<Option<(String, bool, bool)>> = Vec::new();

    for elem in pattern {
        match &elem.kind {
            PatternKind::Literal(_) => {
                // Literals don't produce children but do occupy a position
                position_to_rule.push(None);
            }
            PatternKind::Rule(name) => {
                let sanitized = sanitize_rule_name(name);
                if !sanitized.is_empty() {
                    position_to_rule.push(Some((sanitized, elem.optional, elem.repeated)));
                } else {
                    position_to_rule.push(None);
                }
            }
            PatternKind::Builtin(_) => {
                // Builtins typically don't produce children
                position_to_rule.push(None);
            }
        }
    }

    // Process from $9 down to $1 to avoid $1 matching $10, etc.
    for i in (1..=9).rev() {
        let pattern_str = format!("${}", i);
        let pattern_with_space = format!("$ {}", i);  // TokenStream may insert spaces

        // First, normalize any `$ N` to `$N` for easier processing
        result = result.replace(&pattern_with_space, &pattern_str);

        if !result.contains(&pattern_str) {
            continue;
        }

        // Find what kind of element this is (1-indexed, so i-1)
        if let Some(Some((rule_name, optional, repeated))) = position_to_rule.get(i - 1) {
            let var_name = format!("child_{}", rule_name);

            // Replace intern($N) with getting text from the child
            let intern_pattern = format!("intern({})", pattern_str);
            if result.contains(&intern_pattern) {
                let replacement = format!("{}.as_ref().map(|p| p.as_str().to_string()).unwrap_or_default()", var_name);
                result = result.replace(&intern_pattern, &replacement);
            }

            // Replace $N.collect() with proper iteration and building
            let collect_pattern = format!("{}.collect()", pattern_str);
            if result.contains(&collect_pattern) {
                let replacement = format!(
                    "{}.iter().filter_map(|p| self.build_{}(p.clone()).ok()).collect()",
                    var_name, rule_name
                );
                result = result.replace(&collect_pattern, &replacement);
            }

            // Replace $N.map(|t| t) with proper optional handling
            let map_pattern = format!("{}.map(|t| t)", pattern_str);
            if result.contains(&map_pattern) {
                let replacement = format!(
                    "{}.as_ref().and_then(|p| self.build_{}(p.clone()).ok())",
                    var_name, rule_name
                );
                result = result.replace(&map_pattern, &replacement);
            }

            // Replace $N.unwrap_or_default() pattern
            let unwrap_pattern = format!("{}.unwrap_or_default()", pattern_str);
            if result.contains(&unwrap_pattern) {
                let replacement = format!(
                    "{}.as_ref().and_then(|p| self.build_{}(p.clone()).ok()).unwrap_or_default()",
                    var_name, rule_name
                );
                result = result.replace(&unwrap_pattern, &replacement);
            }

            // Handle $N.map(|...|...) - general map patterns with custom closures
            // e.g., $2.map(|s| parse_int(s)) -> build the value and apply the closure
            let general_map_pattern = format!("{}.map(|", pattern_str);
            while result.contains(&general_map_pattern) {
                let start = result.find(&general_map_pattern).unwrap();
                // Find the closing paren of the map call
                let after_map = start + general_map_pattern.len();
                let mut depth = 1;
                let mut end = after_map;
                for (i, ch) in result[after_map..].chars().enumerate() {
                    match ch {
                        '(' => depth += 1,
                        ')' => {
                            depth -= 1;
                            if depth == 0 {
                                end = after_map + i + 1;
                                break;
                            }
                        }
                        _ => {}
                    }
                }
                // Extract the closure body: "|arg| body)"
                let closure_full = &result[after_map..end];
                // Replace $N.map(|x| ...) with build_value.map(|x| ...)
                let replacement = format!(
                    "{}.as_ref().and_then(|p| self.build_{}(p.clone()).ok()).map(|{}",
                    var_name, rule_name, closure_full
                );
                result = format!("{}{}{}", &result[..start], replacement, &result[end..]);
            }

            // Replace $N.field patterns (e.g., $7.statements) with building and field access
            let dot_pattern = format!("{}.", pattern_str);
            while let Some(start) = result.find(&dot_pattern) {
                let after_dot = start + dot_pattern.len();
                // Find the field name (alphanumeric chars after the dot)
                let field_end = result[after_dot..]
                    .chars()
                    .take_while(|c| c.is_alphanumeric() || *c == '_')
                    .count();
                if field_end > 0 {
                    let field_name = &result[after_dot..after_dot + field_end];
                    // Skip if it's "map" - that's handled above
                    if field_name == "map" {
                        break;
                    }
                    let full_match_len = dot_pattern.len() + field_end;
                    let replacement = format!(
                        "{}.as_ref().and_then(|p| self.build_{}(p.clone()).ok()).map(|v| v.{}).unwrap_or_default()",
                        var_name, rule_name, field_name
                    );
                    result = format!("{}{}{}", &result[..start], replacement, &result[start + full_match_len..]);
                } else {
                    break;
                }
            }

            // Replace plain $N with the properly typed value
            if *repeated {
                // For repeated elements, build each and collect
                let replacement = format!(
                    "{}.iter().filter_map(|p| self.build_{}(p.clone()).ok()).collect::<Vec<_>>()",
                    var_name, rule_name
                );
                result = result.replace(&pattern_str, &replacement);
            } else if *optional {
                // For optional elements, build if present
                let replacement = format!(
                    "{}.as_ref().and_then(|p| self.build_{}(p.clone()).ok())",
                    var_name, rule_name
                );
                result = result.replace(&pattern_str, &replacement);
            } else {
                // For required elements, build and unwrap with default fallback
                let replacement = format!(
                    "{}.as_ref().and_then(|p| self.build_{}(p.clone()).ok()).unwrap_or_default()",
                    var_name, rule_name
                );
                result = result.replace(&pattern_str, &replacement);
            }
        } else if let Some(None) = position_to_rule.get(i - 1) {
            // This position is a literal - it doesn't produce a child, so $N refers to the text
            // Note: pair_str is captured before pair.into_inner() consumes it
            if i == 1 {
                // For $1 referring to a literal (e.g., the matched keyword), use pair_str
                result = result.replace(&pattern_str, "pair_str");
            } else {
                // Other literal positions - use pair_str
                result = result.replace(&pattern_str, "pair_str");
            }
        } else {
            // Position not found in pattern analysis - use a placeholder
            // This happens when the pattern has more elements than we parsed
            // (e.g., complex nested patterns or alternations)
            if result.contains(&format!("{}.unwrap_or_default()", pattern_str)) {
                result = result.replace(&format!("{}.unwrap_or_default()", pattern_str), "Default::default()");
            } else if result.contains(&format!("{}.span", pattern_str)) {
                // $1.span -> span (the local span variable)
                result = result.replace(&format!("{}.span", pattern_str), "span");
            } else if i == 1 {
                // $1 with no matching rule often means "the whole match" (the pair's text)
                // This happens when pattern is all literals, or when $1 refers to the matched string
                result = result.replace(&pattern_str, "pair_str");
            } else {
                // Replace with pair_str as fallback
                result = result.replace(&pattern_str, "pair_str");
            }
        }
    }

    result
}

/// Parse imports string into TokenStream
fn parse_imports(code: &str) -> TokenStream {
    if code.is_empty() {
        return quote! {};
    }

    code.parse().unwrap_or_else(|_| quote! {
        // Failed to parse imports
    })
}

/// Generate context field declarations
fn generate_context_fields(context: &[crate::ContextVar]) -> TokenStream {
    let fields: Vec<TokenStream> = context.iter().map(|v| {
        let name = format_ident!("{}", v.name);
        let ty: TokenStream = v.ty.parse().unwrap_or_else(|_| quote! { () });
        quote! { pub #name: #ty }
    }).collect();

    quote! { #(#fields,)* }
}

/// Parse type helpers code
fn parse_type_helpers(code: &str) -> TokenStream {
    if code.is_empty() {
        return quote! {};
    }

    code.parse().unwrap_or_else(|_| quote! {
        // Failed to parse type helpers
    })
}

/// Convert string to PascalCase
fn to_pascal_case(s: &str) -> String {
    let mut result = String::new();
    let mut capitalize_next = true;

    for c in s.chars() {
        if c == '_' || c == '-' || c == ' ' {
            capitalize_next = true;
        } else if capitalize_next {
            result.push(c.to_ascii_uppercase());
            capitalize_next = false;
        } else {
            result.push(c.to_ascii_lowercase());
        }
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_to_pascal_case() {
        assert_eq!(to_pascal_case("hello"), "Hello");
        assert_eq!(to_pascal_case("hello_world"), "HelloWorld");
        assert_eq!(to_pascal_case("zig"), "Zig");
        assert_eq!(to_pascal_case("my_parser"), "MyParser");
    }

    #[test]
    fn test_transform_captures_to_vars() {
        let pattern = vec![
            PatternElement { index: 1, kind: PatternKind::Rule("identifier".to_string()), name: None, optional: false, repeated: false },
            PatternElement { index: 2, kind: PatternKind::Rule("expr".to_string()), name: None, optional: false, repeated: false },
        ];
        // Basic capture reference replacement - now uses rule-name-based variables
        assert!(transform_captures_to_vars("$1", &pattern).contains("child_identifier"));
        assert!(transform_captures_to_vars("$1 + $2", &pattern).contains("child_identifier"));
        assert!(transform_captures_to_vars("$1 + $2", &pattern).contains("child_expr"));
    }

    #[test]
    fn test_analyze_fn_decl_pattern() {
        // This pattern is from fn_decl in zig.zyn
        let pattern = r#""fn" ~ identifier ~
    "(" ~ fn_params? ~ ")" ~ type_expr ~
    block"#;

        let elements = analyze_pattern(pattern);

        // Debug: print what was parsed
        for elem in &elements {
            println!("index={}, kind={:?}, optional={}", elem.index, elem.kind, elem.optional);
        }

        // There should be 7 elements total
        assert_eq!(elements.len(), 7, "Expected 7 pattern elements");

        // Check each element
        assert!(matches!(elements[0].kind, PatternKind::Literal(_)), "Element 0 should be 'fn' literal");
        assert!(matches!(elements[1].kind, PatternKind::Rule(ref n) if n == "identifier"), "Element 1 should be identifier");
        assert!(matches!(elements[2].kind, PatternKind::Literal(_)), "Element 2 should be '(' literal");
        assert!(matches!(elements[3].kind, PatternKind::Rule(ref n) if n == "fn_params"), "Element 3 should be fn_params");
        assert!(elements[3].optional, "fn_params should be optional");
        assert!(matches!(elements[4].kind, PatternKind::Literal(_)), "Element 4 should be ')' literal");
        assert!(matches!(elements[5].kind, PatternKind::Rule(ref n) if n == "type_expr"), "Element 5 should be type_expr");
        assert!(matches!(elements[6].kind, PatternKind::Rule(ref n) if n == "block"), "Element 6 should be block");
    }

    #[test]
    fn test_generate_pest_grammar() {
        let grammar = ZynGrammar {
            language: crate::LanguageInfo {
                name: "Test".to_string(),
                ..Default::default()
            },
            rules: vec![
                RuleDef {
                    name: "number".to_string(),
                    modifier: Some(RuleModifier::Atomic),
                    pattern: "ASCII_DIGIT+".to_string(),
                    action: None,
                },
                RuleDef {
                    name: "expr".to_string(),
                    modifier: None,
                    pattern: "number | \"(\" ~ expr ~ \")\"".to_string(),
                    action: None,
                },
            ],
            ..Default::default()
        };

        let pest = generate_pest_grammar(&grammar).unwrap();
        assert!(pest.contains("number = @{ ASCII_DIGIT+ }"));
        assert!(pest.contains("expr = { number | \"(\" ~ expr ~ \")\" }"));
        assert!(pest.contains("WHITESPACE"));
    }
}
