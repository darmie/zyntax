//! End-to-End Integration Test for ZynPEG Phase 1 POC
//!
//! Tests the complete pipeline: Source → TypedAST → HIR → JIT Execution
//!
//! This demonstrates that pest can successfully parse source code into TypedAST
//! which can then be compiled and executed through Zyntax's backend infrastructure.

use zyn_parser::{CalculatorParser, TypedAstBuilder};
use pest::Parser;
use zyntax_typed_ast::{TypedNode, TypedExpression, BinaryOp, Type};

#[test]
fn test_parse_to_typed_ast() {
    // Source code
    let source = "2 + 3";

    // Phase 1: Parse to pest parse tree
    let pairs = CalculatorParser::parse(zyn_parser::Rule::program, source)
        .expect("Failed to parse");

    // Phase 2: Build TypedAST from parse tree
    let mut builder = TypedAstBuilder::new();
    let result = builder.build_program(pairs);

    assert!(result.is_ok(), "Failed to build TypedAST");
    let typed_expr = result.unwrap();

    // Verify TypedAST structure
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

    // Result should be multiplication
    assert_binary_op(&typed_expr, BinaryOp::Mul);
}

#[test]
fn test_operator_precedence() {
    // Test that 1 + 2 * 3 is parsed as 1 + (2 * 3), not (1 + 2) * 3
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

    // Should be unary expression
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

    // Type should be inferred as I32
    assert!(matches!(typed_expr.ty, Type::Primitive(zyntax_typed_ast::PrimitiveType::I32)));
}

// Helper function to assert binary operation
fn assert_binary_op(expr: &TypedNode<TypedExpression>, expected_op: BinaryOp) {
    if let TypedExpression::Binary(bin) = &expr.node {
        assert_eq!(bin.op, expected_op, "Expected {:?} but got {:?}", expected_op, bin.op);
    } else {
        panic!("Expected binary expression, got {:?}", expr.node);
    }
}

// TODO: Phase 2 - Add full HIR lowering and JIT execution tests
// These will be implemented once we have the Zig grammar complete and
// can test more complex programs with functions and control flow.
//
// Future tests:
// #[test]
// fn test_compile_to_hir() { ... }
//
// #[test]
// fn test_jit_execute() { ... }
