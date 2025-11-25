// TypedAST Builder for Calculator Grammar
//
// Converts pest parse trees into Zyntax TypedAST.
// This demonstrates the pattern that will be automated in Phase 3 (.zyn format).

use pest::iterators::{Pair, Pairs};
use zyntax_typed_ast::{
    BinaryOp, PrimitiveType, Span, Type, TypedASTBuilder, TypedDeclaration, TypedExpression, TypedNode, UnaryOp, typed_builder::FluentFunctionBuilder
};
use crate::Rule;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum BuildError {
    #[error("Failed to parse number: {0}")]
    ParseNumber(String),

    #[error("Unexpected rule: expected {expected}, got {actual:?}")]
    UnexpectedRule {
        expected: String,
        actual: Rule,
    },

    #[error("Empty expression")]
    EmptyExpression,
}

pub type BuildResult<T> = Result<T, BuildError>;

/// Builds TypedAST from pest parse tree
pub struct TypedAstBuilder {
    builder: TypedASTBuilder,
}

impl TypedAstBuilder {
    pub fn new() -> Self {
        Self {
            builder: TypedASTBuilder::new(),
        }
    }

    pub fn build_fn(&mut self, expr:TypedNode<TypedExpression>, name: &str, span: Span) -> BuildResult<TypedNode<TypedDeclaration>> {
        let expr_stmt = self.builder.expression_statement(expr, span);
        let params = vec![];
        let return_type = Type::Primitive(PrimitiveType::Unit);
        let body = self.builder.block(vec![expr_stmt], span);
        let visibility = zyntax_typed_ast::Visibility::Public;
        let is_async = false;
       Ok(self.builder.function(name, params, return_type, body, visibility, is_async, span))
    }

    /// Convert pest pairs to TypedAST program
    pub fn build_program(&mut self, mut pairs: Pairs<Rule>) -> BuildResult<TypedNode<TypedExpression>> {
        // program = { SOI ~ expr ~ EOI }
        let program_pair = pairs.next().ok_or(BuildError::EmptyExpression)?;

        if program_pair.as_rule() != Rule::program {
            return Err(BuildError::UnexpectedRule {
                expected: "program".to_string(),
                actual: program_pair.as_rule(),
            });
        }

        // Extract the expr from inside program
        let mut inner = program_pair.into_inner();
        let expr_pair = inner.next().ok_or(BuildError::EmptyExpression)?;

        self.build_expr(expr_pair)
    }

    /// Build an expression
    fn build_expr(&mut self, pair: Pair<Rule>) -> BuildResult<TypedNode<TypedExpression>> {
        match pair.as_rule() {
            Rule::expr => self.build_expr_with_ops(pair),
            Rule::term => self.build_term(pair),
            Rule::factor => self.build_factor(pair),
            Rule::number => self.build_number(pair),
            _ => Err(BuildError::UnexpectedRule {
                expected: "expression".to_string(),
                actual: pair.as_rule(),
            }),
        }
    }

    /// Build expr = { term ~ (add_op ~ term)* }
    fn build_expr_with_ops(&mut self, pair: Pair<Rule>) -> BuildResult<TypedNode<TypedExpression>> {
        let mut inner = pair.clone().into_inner();

        // Start with first term
        let mut left = self.build_term(inner.next().ok_or(BuildError::EmptyExpression)?)?;

        // Process remaining (add_op ~ term) pairs
        while let Some(op_pair) = inner.next() {
            let op = self.parse_add_op(&op_pair)?;
            let right = self.build_term(inner.next().ok_or(BuildError::EmptyExpression)?)?;

            // Infer result type based on operands
            let result_type = self.infer_binary_type(&left.ty, &right.ty);
            let span = self.combine_spans(&left.span, &right.span);

            left = self.builder.binary(op, left, right, result_type, span);
        }

        Ok(left)
    }

    /// Build term = { factor ~ (mul_op ~ factor)* }
    fn build_term(&mut self, pair: Pair<Rule>) -> BuildResult<TypedNode<TypedExpression>> {
        let mut inner = pair.into_inner();

        // Start with first factor
        let mut left = self.build_factor(inner.next().ok_or(BuildError::EmptyExpression)?)?;

        // Process remaining (mul_op ~ factor) pairs
        while let Some(op_pair) = inner.next() {
            let op = self.parse_mul_op(&op_pair)?;
            let right = self.build_factor(inner.next().ok_or(BuildError::EmptyExpression)?)?;

            let result_type = self.infer_binary_type(&left.ty, &right.ty);
            let span = self.combine_spans(&left.span, &right.span);

            left = self.builder.binary(op, left, right, result_type, span);
        }

        Ok(left)
    }

    /// Build factor = { number | "(" ~ expr ~ ")" | unary_op ~ factor }
    fn build_factor(&mut self, pair: Pair<Rule>) -> BuildResult<TypedNode<TypedExpression>> {
        let mut inner = pair.into_inner();
        let first = inner.next().ok_or(BuildError::EmptyExpression)?;

        match first.as_rule() {
            Rule::number => self.build_number(first),
            Rule::expr => self.build_expr_with_ops(first),  // Parenthesized expression
            Rule::unary_op => {
                let op = self.parse_unary_op(&first)?;
                let operand = self.build_factor(inner.next().ok_or(BuildError::EmptyExpression)?)?;

                let result_type = operand.ty.clone();
                let span = Span::new(
                    first.as_span().start(),
                    operand.span.end,
                );

                Ok(self.builder.unary(op, operand, result_type, span))
            }
            _ => Err(BuildError::UnexpectedRule {
                expected: "number, expr, or unary_op".to_string(),
                actual: first.as_rule(),
            }),
        }
    }

    /// Build number literal
    fn build_number(&mut self, pair: Pair<Rule>) -> BuildResult<TypedNode<TypedExpression>> {
        let text = pair.as_str();
        let span_range = pair.as_span();
        let span = Span::new(span_range.start(), span_range.end());

        // Check if it's a float or int
        if text.contains('.') {
            // Float literal
            let value: f64 = text.parse()
                .map_err(|_| BuildError::ParseNumber(text.to_string()))?;

            // For now, create as integer and note we'll need float support
            // In a real implementation, we'd use builder.float_literal()
            // For POC, convert to int (will fix in Phase 2)
            let int_value = value as i128;
            Ok(self.builder.int_literal(int_value, span))
        } else {
            // Integer literal
            let value: i128 = text.parse()
                .map_err(|_| BuildError::ParseNumber(text.to_string()))?;
            Ok(self.builder.int_literal(value, span))
        }
    }

    /// Parse addition/subtraction operator
    fn parse_add_op(&self, pair: &Pair<Rule>) -> BuildResult<BinaryOp> {
        match pair.as_str() {
            "+" => Ok(BinaryOp::Add),
            "-" => Ok(BinaryOp::Sub),
            _ => Err(BuildError::UnexpectedRule {
                expected: "add_op".to_string(),
                actual: pair.as_rule(),
            }),
        }
    }

    /// Parse multiplication/division/modulo operator
    fn parse_mul_op(&self, pair: &Pair<Rule>) -> BuildResult<BinaryOp> {
        match pair.as_str() {
            "*" => Ok(BinaryOp::Mul),
            "/" => Ok(BinaryOp::Div),
            "%" => Ok(BinaryOp::Rem),  // Remainder, not Mod
            _ => Err(BuildError::UnexpectedRule {
                expected: "mul_op".to_string(),
                actual: pair.as_rule(),
            }),
        }
    }

    /// Parse unary operator
    fn parse_unary_op(&self, pair: &Pair<Rule>) -> BuildResult<UnaryOp> {
        match pair.as_str() {
            "-" => Ok(UnaryOp::Minus),  // Minus, not Neg
            "!" => Ok(UnaryOp::Not),
            _ => Err(BuildError::UnexpectedRule {
                expected: "unary_op".to_string(),
                actual: pair.as_rule(),
            }),
        }
    }

    /// Infer result type of binary operation
    fn infer_binary_type(&self, left: &Type, right: &Type) -> Type {
        // For calculator POC, we use simple rules:
        // - If either operand is F64, result is F64
        // - Otherwise, result is I32
        match (left, right) {
            (Type::Primitive(PrimitiveType::F64), _) |
            (_, Type::Primitive(PrimitiveType::F64)) => {
                Type::Primitive(PrimitiveType::F64)
            }
            _ => Type::Primitive(PrimitiveType::I32),
        }
    }

    /// Combine two spans into a larger span
    fn combine_spans(&self, left: &Span, right: &Span) -> Span {
        Span::new(left.start, right.end)
    }

    /// Get the underlying TypedASTBuilder
    pub fn into_builder(self) -> TypedASTBuilder {
        self.builder
    }

    /// Get a reference to the type registry
    pub fn registry(&self) -> &zyntax_typed_ast::TypeRegistry {
        &self.builder.registry
    }

    /// Intern a string
    pub fn intern(&mut self, s: &str) -> zyntax_typed_ast::InternedString {
        self.builder.intern(s)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::CalculatorParser;
    use pest::Parser;

    #[test]
    fn test_build_number() {
        let pairs = CalculatorParser::parse(Rule::program, "42").unwrap();
        let mut builder = TypedAstBuilder::new();
        let result = builder.build_program(pairs);

        assert!(result.is_ok());
        let expr = result.unwrap();
        assert!(matches!(expr.ty, Type::Primitive(PrimitiveType::I32)));
    }

    #[test]
    fn test_build_addition() {
        let pairs = CalculatorParser::parse(Rule::program, "2 + 3").unwrap();
        let mut builder = TypedAstBuilder::new();
        let result = builder.build_program(pairs);

        assert!(result.is_ok());
        let expr = result.unwrap();
        assert!(matches!(expr.ty, Type::Primitive(PrimitiveType::I32)));
    }

    #[test]
    fn test_build_complex_expr() {
        let pairs = CalculatorParser::parse(Rule::program, "(2 + 3) * 4").unwrap();
        let mut builder = TypedAstBuilder::new();
        let result = builder.build_program(pairs);

        assert!(result.is_ok());
    }

    #[test]
    fn test_build_unary() {
        let pairs = CalculatorParser::parse(Rule::program, "-42").unwrap();
        let mut builder = TypedAstBuilder::new();
        let result = builder.build_program(pairs);

        assert!(result.is_ok());
        let expr = result.unwrap();
        assert!(matches!(expr.node, TypedExpression::Unary(_)));
    }
}
