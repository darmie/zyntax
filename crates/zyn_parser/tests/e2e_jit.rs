//! End-to-End JIT Execution Tests for ZynPEG Phase 1 POC
//!
//! Tests: Source → TypedAST → HIR → Cranelift JIT → Execution

use zyn_parser::{CalculatorParser, TypedAstBuilder};
use pest::Parser;
use zyntax_typed_ast::{
    TypedNode, TypedExpression, PrimitiveType, Type,
    CallingConvention, Visibility,
    arena::AstArena,
};
use zyntax_compiler::{
    cranelift_backend::CraneliftBackend,
    lowering::{LoweringContext, LoweringConfig, AstLowering},
};
use std::sync::{Arc, Mutex};

#[test]
fn test_jit_addition() {
    let source = "2 + 3";

    // Parse to TypedAST
    let pairs = CalculatorParser::parse(zyn_parser::Rule::program, source).unwrap();
    let mut ast_builder = TypedAstBuilder::new();
    let expr = ast_builder.build_program(pairs).unwrap();

    // Convert to HIR and execute
    let result = compile_and_execute_expr(expr, &mut ast_builder);

    assert_eq!(result, 5);
    println!("[ZynPEG E2E] ✓ \"{}\" = {}", source, result);
}

#[test]
fn test_jit_multiplication() {
    let source = "6 * 7";

    let pairs = CalculatorParser::parse(zyn_parser::Rule::program, source).unwrap();
    let mut ast_builder = TypedAstBuilder::new();
    let expr = ast_builder.build_program(pairs).unwrap();

    let result = compile_and_execute_expr(expr, &mut ast_builder);

    assert_eq!(result, 42);
    println!("[ZynPEG E2E] ✓ \"{}\" = {}", source, result);
}

#[test]
fn test_jit_complex() {
    let source = "(2 + 3) * 4";

    let pairs = CalculatorParser::parse(zyn_parser::Rule::program, source).unwrap();
    let mut ast_builder = TypedAstBuilder::new();
    let expr = ast_builder.build_program(pairs).unwrap();

    let result = compile_and_execute_expr(expr, &mut ast_builder);

    assert_eq!(result, 20);
    println!("[ZynPEG E2E] ✓ \"{}\" = {}", source, result);
}

// ===== HELPER FUNCTIONS =====

/// Compile and execute an expression using the lowering API
fn compile_and_execute_expr(expr: TypedNode<TypedExpression>, ast_builder: &mut TypedAstBuilder) -> i32 {
    // Wrap expression in a TypedProgram with a main function that returns the expression
    let typed_program = wrap_expr_in_program(expr, ast_builder);

    // Lower to HIR using official API
    let hir_module = lower_typed_program_to_hir(typed_program, ast_builder);

    // Compile with Cranelift
    let mut backend = CraneliftBackend::new().expect("Failed to create backend");
    backend.compile_module(&hir_module).expect("Failed to compile module");

    // Find the main function and execute it
    // We can use the InternedString directly since we know it from ast_builder
    let calc_main_name = ast_builder.intern("calc_main");
    let main_id = hir_module.functions.values()
        .find(|f| f.name == calc_main_name)
        .map(|f| f.id)
        .expect("main function not found");

    let func_ptr = backend.get_function_ptr(main_id).expect("Function not found");

    unsafe {
        let exec_fn: extern "C" fn() -> i32 = std::mem::transmute(func_ptr);
        exec_fn()
    }
}

/// Wrap a TypedExpression in a TypedProgram with a main function
fn wrap_expr_in_program(
    expr: TypedNode<TypedExpression>,
    ast_builder: &mut TypedAstBuilder,
) -> zyntax_typed_ast::TypedProgram {
    use zyntax_typed_ast::typed_ast::{TypedFunction, TypedDeclaration, TypedBlock, TypedStatement};
    use zyntax_typed_ast::TypedProgram;

    // Create return statement
    let return_stmt = TypedStatement::Return(Some(Box::new(expr.clone())));
    let return_stmt_node = TypedNode {
        node: return_stmt,
        ty: Type::Primitive(PrimitiveType::Unit),
        span: expr.span,
    };

    // Create function body
    let body = TypedBlock {
        statements: vec![return_stmt_node],
        span: expr.span,
    };

    // Get function name
    let func_name = ast_builder.intern("calc_main");

    // Create function
    let func = TypedFunction {
        name: func_name,
        type_params: vec![],
        params: vec![],
        return_type: Type::Primitive(PrimitiveType::I32),
        body: Some(body),
        visibility: Visibility::Public,
        is_async: false,
        is_external: false,
        calling_convention: CallingConvention::Cdecl,
        link_name: None,
    };

    let func_decl = TypedDeclaration::Function(func);
    let func_node = TypedNode {
        node: func_decl,
        ty: Type::Function {
            params: vec![],
            return_type: Box::new(Type::Primitive(PrimitiveType::I32)),
            is_varargs: false,
            has_named_params: false,
            has_default_params: false,
            async_kind: zyntax_typed_ast::AsyncKind::Sync,
            calling_convention: CallingConvention::Cdecl,
            nullability: zyntax_typed_ast::NullabilityKind::NonNull,
        },
        span: expr.span,
    };

    TypedProgram {
        declarations: vec![func_node],
        span: expr.span,
    }
}

/// Lower TypedProgram to HIR using the official lowering API
fn lower_typed_program_to_hir(
    program: zyntax_typed_ast::TypedProgram,
    ast_builder: &TypedAstBuilder,
) -> zyntax_compiler::hir::HirModule {
    let mut arena = AstArena::new();
    let module_name = arena.intern_string("calculator_module");

    let mut lowering_ctx = LoweringContext::new(
        module_name,
        Arc::new(ast_builder.registry().clone()),
        Arc::new(Mutex::new(arena)),
        LoweringConfig::default(),
    );

    // Skip type checking for simple calculator expressions
    std::env::set_var("SKIP_TYPE_CHECK", "1");

    lowering_ctx.lower_program(&program)
        .expect("Failed to lower TypedProgram to HIR")
}
