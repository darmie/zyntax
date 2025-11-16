//! TypedAST Generation Tests for ZynPEG Phase 1 POC
//!
//! Tests: Source → TypedAST verification

use zyn_parser::{CalculatorParser, TypedAstBuilder};
use pest::Parser;
use zyntax_typed_ast::{TypedNode, TypedExpression, BinaryOp, Type, PrimitiveType};

#[test]
fn test_parse_to_typed_ast() {
    let source = "2 + 3";

    let pairs = CalculatorParser::parse(zyn_parser::Rule::program, source)
        .expect("Failed to parse");

    let mut builder = TypedAstBuilder::new();
    let result = builder.build_program(pairs);

    assert!(result.is_ok(), "Failed to build TypedAST");
    let typed_expr = result.unwrap();

    assert_binary_op(&typed_expr, BinaryOp::Add);
}

#[test]
fn test_parse_complex_expression() {
    let source = "(2 + 3) * 4";

    let pairs = CalculatorParser::parse(zyn_parser::Rule::program, source)
        .expect("Failed to parse");

    let mut builder = TypedAstBuilder::new();
    let result = builder.build_program(pairs);

    assert!(result.is_ok());
    let typed_expr = result.unwrap();

    assert_binary_op(&typed_expr, BinaryOp::Mul);
}

#[test]
fn test_operator_precedence() {
    let source = "1 + 2 * 3";

    let pairs = CalculatorParser::parse(zyn_parser::Rule::program, source)
        .expect("Failed to parse");

    let mut builder = TypedAstBuilder::new();
    let result = builder.build_program(pairs);

    assert!(result.is_ok());
    let typed_expr = result.unwrap();

    // Root should be addition
    if let TypedExpression::Binary(bin) = &typed_expr.node {
        assert_eq!(bin.op, BinaryOp::Add);

        // Right operand should be multiplication (2 * 3)
        if let TypedExpression::Binary(right_bin) = &bin.right.node {
            assert_eq!(right_bin.op, BinaryOp::Mul);
        } else {
            panic!("Expected right operand to be binary expression");
        }
    } else {
        panic!("Expected binary expression at root");
    }
}

#[test]
fn test_unary_negation() {
    let source = "-42";

    let pairs = CalculatorParser::parse(zyn_parser::Rule::program, source)
        .expect("Failed to parse");

    let mut builder = TypedAstBuilder::new();
    let result = builder.build_program(pairs);

    assert!(result.is_ok());
    let typed_expr = result.unwrap();

    assert!(matches!(typed_expr.node, TypedExpression::Unary(_)));
}

#[test]
fn test_type_inference() {
    let source = "10 + 20";

    let pairs = CalculatorParser::parse(zyn_parser::Rule::program, source)
        .expect("Failed to parse");

    let mut builder = TypedAstBuilder::new();
    let result = builder.build_program(pairs);

    assert!(result.is_ok());
    let typed_expr = result.unwrap();

    assert!(matches!(typed_expr.ty, Type::Primitive(PrimitiveType::I32)));
}

// Helper function
fn assert_binary_op(expr: &TypedNode<TypedExpression>, expected_op: BinaryOp) {
    if let TypedExpression::Binary(bin) = &expr.node {
        assert_eq!(bin.op, expected_op, "Expected {:?} but got {:?}", expected_op, bin.op);
    } else {
        panic!("Expected binary expression, got {:?}", expr.node);
    }
}
