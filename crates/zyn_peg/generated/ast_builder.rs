//! Generated AST builder
//!
//! This file provides the AST builder context that transforms pest parse trees
//! into TypedAST nodes. The full generated version has issues that need to be
//! fixed in the generator - this is a minimal working implementation.

#![allow(dead_code, unused_variables, unused_imports, unused_mut)]

use super::typed_ast::*;
use super::Rule;

/// TypedAST builder context
pub struct AstBuilderContext;

impl AstBuilderContext {
    pub fn new() -> Self {
        Self
    }

    /// Build a TypedProgram from a parsed program rule
    pub fn build_program(
        &mut self,
        pair: pest::iterators::Pair<Rule>,
    ) -> Result<TypedProgram, ParseError> {
        let span = Span::from_pest(pair.as_span());
        let mut declarations = Vec::new();

        for inner in pair.into_inner() {
            if inner.as_rule() == Rule::declaration {
                if let Ok(decl) = self.build_declaration(inner) {
                    declarations.push(decl);
                }
            }
        }

        Ok(TypedProgram {
            declarations,
            span,
        })
    }

    /// Build a TypedDeclaration from a declaration rule
    fn build_declaration(
        &mut self,
        pair: pest::iterators::Pair<Rule>,
    ) -> Result<TypedDeclaration, ParseError> {
        let span = Span::from_pest(pair.as_span());

        // Get the inner declaration type
        if let Some(inner) = pair.into_inner().next() {
            match inner.as_rule() {
                Rule::const_decl => self.build_const_decl(inner),
                Rule::var_decl => self.build_var_decl(inner),
                Rule::fn_decl => self.build_fn_decl(inner),
                Rule::struct_decl => self.build_struct_decl(inner),
                _ => Ok(TypedDeclaration {
                    decl: Declaration::Const(ConstDecl {
                        name: String::new(),
                        ty: None,
                        value: TypedExpression::default(),
                        is_pub: false,
                    }),
                    span,
                }),
            }
        } else {
            Err(ParseError("Empty declaration".to_string()))
        }
    }

    /// Build a const declaration
    fn build_const_decl(
        &mut self,
        pair: pest::iterators::Pair<Rule>,
    ) -> Result<TypedDeclaration, ParseError> {
        let span = Span::from_pest(pair.as_span());
        let mut children = pair.into_inner();

        // Get identifier
        let name = children
            .next()
            .map(|p| p.as_str().to_string())
            .unwrap_or_default();

        // Get type if present (optional)
        let mut ty = None;
        let mut next = children.next();
        if let Some(ref p) = next {
            if p.as_rule() == Rule::type_expr {
                ty = Some(self.build_type(p.clone()));
                next = children.next();
            }
        }

        // Get expression value
        let value = if let Some(expr_pair) = next {
            self.build_expr(expr_pair)
        } else {
            TypedExpression::default()
        };

        Ok(TypedDeclaration {
            decl: Declaration::Const(ConstDecl {
                name,
                ty,
                value,
                is_pub: false,
            }),
            span,
        })
    }

    /// Build a var declaration
    fn build_var_decl(
        &mut self,
        pair: pest::iterators::Pair<Rule>,
    ) -> Result<TypedDeclaration, ParseError> {
        let span = Span::from_pest(pair.as_span());
        let mut children = pair.into_inner();

        let name = children
            .next()
            .map(|p| p.as_str().to_string())
            .unwrap_or_default();

        let mut ty = None;
        let mut next = children.next();
        if let Some(ref p) = next {
            if p.as_rule() == Rule::type_expr {
                ty = Some(self.build_type(p.clone()));
                next = children.next();
            }
        }

        let value = next.map(|expr_pair| self.build_expr(expr_pair));

        Ok(TypedDeclaration {
            decl: Declaration::Var(VarDecl {
                name,
                ty,
                value,
                is_pub: false,
            }),
            span,
        })
    }

    /// Build a function declaration
    fn build_fn_decl(
        &mut self,
        pair: pest::iterators::Pair<Rule>,
    ) -> Result<TypedDeclaration, ParseError> {
        let span = Span::from_pest(pair.as_span());
        let mut children = pair.into_inner().peekable();

        let name = children
            .next()
            .map(|p| p.as_str().to_string())
            .unwrap_or_default();

        // Parse parameters - fn_params contains fn_param children
        let mut params = Vec::new();
        while let Some(p) = children.peek() {
            if p.as_rule() == Rule::fn_params {
                let params_pair = children.next().unwrap();
                for param_pair in params_pair.into_inner() {
                    if param_pair.as_rule() == Rule::fn_param {
                        // fn_param is either comptime_param or regular_param
                        if let Some(inner) = param_pair.into_inner().next() {
                            match inner.as_rule() {
                                Rule::regular_param => {
                                    let mut param_children = inner.into_inner();
                                    let param_name = param_children
                                        .next()
                                        .map(|p| p.as_str().to_string())
                                        .unwrap_or_default();
                                    let param_ty = param_children
                                        .next()
                                        .map(|p| self.build_type(p))
                                        .unwrap_or(Type::Unknown);
                                    params.push(Param {
                                        name: param_name,
                                        ty: param_ty,
                                        is_comptime: false,
                                    });
                                }
                                Rule::comptime_param => {
                                    let mut param_children = inner.into_inner();
                                    let param_name = param_children
                                        .next()
                                        .map(|p| p.as_str().to_string())
                                        .unwrap_or_default();
                                    params.push(Param {
                                        name: param_name,
                                        ty: Type::Unknown, // Comptime type
                                        is_comptime: true,
                                    });
                                }
                                _ => {}
                            }
                        }
                    }
                }
            } else {
                break;
            }
        }

        // Parse return type
        let return_type = children
            .next()
            .filter(|p| p.as_rule() == Rule::type_expr)
            .map(|p| self.build_type(p))
            .unwrap_or(Type::Void);

        // Parse body (block)
        let body = children
            .next()
            .filter(|p| p.as_rule() == Rule::block)
            .map(|p| self.build_block(p));

        Ok(TypedDeclaration {
            decl: Declaration::Function(FnDecl {
                name,
                params,
                return_type,
                body,
                is_pub: false,
                is_export: false,
                is_extern: false,
            }),
            span,
        })
    }

    /// Build a struct declaration
    fn build_struct_decl(
        &mut self,
        pair: pest::iterators::Pair<Rule>,
    ) -> Result<TypedDeclaration, ParseError> {
        let span = Span::from_pest(pair.as_span());
        let mut children = pair.into_inner();

        let name = children
            .next()
            .map(|p| p.as_str().to_string())
            .unwrap_or_default();

        let mut fields = Vec::new();
        for child in children {
            if child.as_rule() == Rule::field_decl {
                let mut field_children = child.into_inner();
                let field_name = field_children
                    .next()
                    .map(|p| p.as_str().to_string())
                    .unwrap_or_default();
                let field_ty = field_children
                    .next()
                    .map(|p| self.build_type(p))
                    .unwrap_or(Type::Unknown);
                fields.push(FieldDecl {
                    name: field_name,
                    ty: field_ty,
                    default: None,
                });
            }
        }

        Ok(TypedDeclaration {
            decl: Declaration::Struct(StructDecl {
                name,
                fields,
                is_packed: false,
            }),
            span,
        })
    }

    /// Build a type from a type expression
    fn build_type(&mut self, pair: pest::iterators::Pair<Rule>) -> Type {
        let text = pair.as_str().trim();

        // Handle primitive types
        match text {
            "i8" => Type::I8,
            "i16" => Type::I16,
            "i32" => Type::I32,
            "i64" => Type::I64,
            "i128" => Type::I128,
            "u8" => Type::U8,
            "u16" => Type::U16,
            "u32" => Type::U32,
            "u64" => Type::U64,
            "u128" => Type::U128,
            "f32" => Type::F32,
            "f64" => Type::F64,
            "bool" => Type::Bool,
            "void" => Type::Void,
            "usize" => Type::Usize,
            "isize" => Type::Isize,
            _ => {
                // Check for complex types
                if text.starts_with('*') {
                    // Pointer type
                    if let Some(inner) = pair.into_inner().next() {
                        Type::Pointer(Box::new(self.build_type(inner)))
                    } else {
                        Type::Pointer(Box::new(Type::Unknown))
                    }
                } else if text.starts_with('?') {
                    // Optional type
                    if let Some(inner) = pair.into_inner().next() {
                        Type::Optional(Box::new(self.build_type(inner)))
                    } else {
                        Type::Optional(Box::new(Type::Unknown))
                    }
                } else if text.starts_with('[') {
                    // Array type
                    Type::Array(Box::new(Type::Unknown), None)
                } else {
                    // Named type
                    Type::Named(text.to_string())
                }
            }
        }
    }

    /// Build a block of statements
    fn build_block(&mut self, pair: pest::iterators::Pair<Rule>) -> Vec<TypedStatement> {
        let mut statements = Vec::new();

        for child in pair.into_inner() {
            if child.as_rule() == Rule::statement {
                if let Some(stmt) = self.build_statement(child) {
                    statements.push(stmt);
                }
            }
        }

        statements
    }

    /// Build a statement
    fn build_statement(&mut self, pair: pest::iterators::Pair<Rule>) -> Option<TypedStatement> {
        let span = Span::from_pest(pair.as_span());

        if let Some(inner) = pair.into_inner().next() {
            let stmt = match inner.as_rule() {
                Rule::return_stmt => {
                    let value = inner.into_inner().next().map(|p| self.build_expr(p));
                    Statement::Return(value)
                }
                Rule::expr_stmt => {
                    let expr = inner.into_inner().next().map(|p| self.build_expr(p)).unwrap_or_default();
                    Statement::Expression(expr)
                }
                Rule::assignment => {
                    let mut children = inner.into_inner();
                    let target = children.next().map(|p| self.build_expr(p)).unwrap_or_default();
                    let _ = children.next(); // skip assign_op
                    let value = children.next().map(|p| self.build_expr(p)).unwrap_or_default();
                    Statement::Assign(target, value)
                }
                _ => Statement::Expression(TypedExpression::default()),
            };

            Some(TypedStatement { stmt, span })
        } else {
            None
        }
    }

    /// Build an expression
    fn build_expr(&mut self, pair: pest::iterators::Pair<Rule>) -> TypedExpression {
        let span = Span::from_pest(pair.as_span());
        let text = pair.as_str().trim();

        // Try to parse as integer literal
        if let Ok(n) = text.parse::<i64>() {
            return TypedExpression {
                expr: Expression::IntLiteral(n),
                ty: Type::I64,
                span,
            };
        }

        // Try to parse as float literal
        if let Ok(n) = text.parse::<f64>() {
            return TypedExpression {
                expr: Expression::FloatLiteral(n),
                ty: Type::F64,
                span,
            };
        }

        // Check for boolean literals
        if text == "true" {
            return TypedExpression {
                expr: Expression::BoolLiteral(true),
                ty: Type::Bool,
                span,
            };
        }
        if text == "false" {
            return TypedExpression {
                expr: Expression::BoolLiteral(false),
                ty: Type::Bool,
                span,
            };
        }

        // Check for string literal
        if text.starts_with('"') && text.ends_with('"') {
            return TypedExpression {
                expr: Expression::StringLiteral(text[1..text.len()-1].to_string()),
                ty: Type::String,
                span,
            };
        }

        // Otherwise treat as identifier
        TypedExpression {
            expr: Expression::Identifier(text.to_string()),
            ty: Type::Unknown,
            span,
        }
    }
}
