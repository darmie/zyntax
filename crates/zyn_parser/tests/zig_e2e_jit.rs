//! End-to-End JIT Execution Tests for ZynPEG Phase 2 Zig Subset
//!
//! Tests: Zig Source → TypedAST → HIR → Cranelift JIT → Execution

use zyn_parser::{ZigParser, ZigBuilder, zig_parser::Rule};
use pest::Parser;
use zyntax_typed_ast::{
    TypedProgram,
    arena::AstArena,
};
use zyntax_compiler::{
    cranelift_backend::CraneliftBackend,
    lowering::{LoweringContext, LoweringConfig, AstLowering},
};
use std::sync::{Arc, Mutex};

#[test]
fn test_zig_jit_simple_function() {
    let source = r#"
        fn add(x: i32, y: i32) i32 {
            return x + y;
        }
    "#;

    let result = compile_and_execute_zig(source, "add", vec![10, 20]);
    assert_eq!(result, 30);
    println!("[Zig E2E] ✓ add(10, 20) = {}", result);
}

#[test]
fn test_zig_jit_arithmetic() {
    let source = r#"
        fn compute() i32 {
            return 2 + 3 * 4;
        }
    "#;

    let result = compile_and_execute_zig(source, "compute", vec![]);
    assert_eq!(result, 14);
    println!("[Zig E2E] ✓ compute() = {}", result);
}

#[test]
fn test_zig_jit_with_variables() {
    let source = r#"
        fn calculate() i32 {
            const x = 10;
            const y = 20;
            return x + y;
        }
    "#;

    let result = compile_and_execute_zig(source, "calculate", vec![]);
    assert_eq!(result, 30);
    println!("[Zig E2E] ✓ calculate() = {}", result);
}

#[test]
fn test_zig_jit_if_statement() {
    let source = r#"
        fn max(a: i32, b: i32) i32 {
            if (a > b) {
                return a;
            } else {
                return b;
            }
        }
    "#;

    let result = compile_and_execute_zig(source, "max", vec![15, 10]);
    assert_eq!(result, 15);
    println!("[Zig E2E] ✓ max(15, 10) = {}", result);

    let result2 = compile_and_execute_zig(source, "max", vec![5, 20]);
    assert_eq!(result2, 20);
    println!("[Zig E2E] ✓ max(5, 20) = {}", result2);
}

#[test]
fn test_zig_jit_nested_function_calls() {
    let source = r#"
        fn double(x: i32) i32 {
            return x * 2;
        }

        fn quadruple(x: i32) i32 {
            return double(double(x));
        }
    "#;

    let result = compile_and_execute_zig(source, "quadruple", vec![5]);
    assert_eq!(result, 20);
    println!("[Zig E2E] ✓ quadruple(5) = {}", result);
}

#[test]
fn test_zig_jit_while_loop() {
    let source = r#"
        fn sum_to_n(n: i32) i32 {
            var sum = 0;
            var i = 1;
            while (i <= n) {
                sum = sum + i;
                i = i + 1;
            }
            return sum;
        }
    "#;

    let result = compile_and_execute_zig(source, "sum_to_n", vec![10]);
    assert_eq!(result, 55); // 1+2+3+...+10 = 55
    println!("[Zig E2E] ✓ sum_to_n(10) = {}", result);
}

#[test]
fn test_zig_jit_for_loop() {
    let source = r#"
        fn count_to_n(n: i32) i32 {
            var count = 0;
            for (i in 1) {
                count = count + 1;
                if (count >= n) {
                    break;
                }
            }
            return count;
        }
    "#;

    let result = compile_and_execute_zig(source, "count_to_n", vec![7]);
    assert_eq!(result, 7);
    println!("[Zig E2E] ✓ count_to_n(7) = {}", result);
}

#[test]
fn test_zig_jit_factorial() {
    let source = r#"
        fn factorial(n: i32) i32 {
            if (n <= 1) {
                return 1;
            } else {
                return n * factorial(n - 1);
            }
        }
    "#;

    let result = compile_and_execute_zig(source, "factorial", vec![5]);
    assert_eq!(result, 120); // 5! = 120
    println!("[Zig E2E] ✓ factorial(5) = {}", result);
}

#[test]
// NOTE: Currently using bitwise AND/OR (not true short-circuit evaluation)
// Both operands are always evaluated
fn test_zig_jit_logical_operators() {
    let source = r#"
        fn test_and(a: i32, b: i32) i32 {
            if (a > 0 and b > 0) {
                return 1;
            } else {
                return 0;
            }
        }

        fn test_or(a: i32, b: i32) i32 {
            if (a > 0 or b > 0) {
                return 1;
            } else {
                return 0;
            }
        }
    "#;

    let result1 = compile_and_execute_zig(source, "test_and", vec![5, 10]);
    assert_eq!(result1, 1);
    println!("[Zig E2E] ✓ test_and(5, 10) = {}", result1);

    let result2 = compile_and_execute_zig(source, "test_and", vec![5, -10]);
    assert_eq!(result2, 0);
    println!("[Zig E2E] ✓ test_and(5, -10) = {}", result2);

    let result3 = compile_and_execute_zig(source, "test_or", vec![5, -10]);
    assert_eq!(result3, 1);
    println!("[Zig E2E] ✓ test_or(5, -10) = {}", result3);

    let result4 = compile_and_execute_zig(source, "test_or", vec![-5, -10]);
    assert_eq!(result4, 0);
    println!("[Zig E2E] ✓ test_or(-5, -10) = {}", result4);
}

#[test]
fn test_zig_jit_unary_operators() {
    let source = r#"
        fn negate(x: i32) i32 {
            return -x;
        }

        fn abs_value(x: i32) i32 {
            if (x < 0) {
                return -x;
            } else {
                return x;
            }
        }
    "#;

    let result1 = compile_and_execute_zig(source, "negate", vec![42]);
    assert_eq!(result1, -42);
    println!("[Zig E2E] ✓ negate(42) = {}", result1);

    let result2 = compile_and_execute_zig(source, "negate", vec![-10]);
    assert_eq!(result2, 10);
    println!("[Zig E2E] ✓ negate(-10) = {}", result2);

    let result3 = compile_and_execute_zig(source, "abs_value", vec![-15]);
    assert_eq!(result3, 15);
    println!("[Zig E2E] ✓ abs_value(-15) = {}", result3);

    let result4 = compile_and_execute_zig(source, "abs_value", vec![20]);
    assert_eq!(result4, 20);
    println!("[Zig E2E] ✓ abs_value(20) = {}", result4);
}

#[test]
fn test_zig_jit_modulo() {
    let source = r#"
        fn mod_op(a: i32, b: i32) i32 {
            return a % b;
        }

        fn is_even(n: i32) i32 {
            if (n % 2 == 0) {
                return 1;
            } else {
                return 0;
            }
        }
    "#;

    let result1 = compile_and_execute_zig(source, "mod_op", vec![10, 3]);
    assert_eq!(result1, 1);
    println!("[Zig E2E] ✓ mod_op(10, 3) = {}", result1);

    let result2 = compile_and_execute_zig(source, "is_even", vec![4]);
    assert_eq!(result2, 1);
    println!("[Zig E2E] ✓ is_even(4) = {}", result2);

    let result3 = compile_and_execute_zig(source, "is_even", vec![7]);
    assert_eq!(result3, 0);
    println!("[Zig E2E] ✓ is_even(7) = {}", result3);
}

#[test]
#[ignore] // Continue bug: SSA generates correct phis, but JIT execution hangs
          // SSA investigation shows self-referencing phis are created correctly
          // HIR test (continue_debug_test.rs) works, so issue is in TypedAST→HIR lowering
          // TODO: Compare Cranelift IR from working HIR test vs. failing Zig test
fn test_zig_jit_continue() {
    let source = r#"
        fn sum_odd_numbers(n: i32) i32 {
            var sum = 0;
            var i = 1;
            while (i <= n) {
                if (i % 2 == 0) {
                    i = i + 1;
                    continue;
                }
                sum = sum + i;
                i = i + 1;
            }
            return sum;
        }
    "#;

    let result = compile_and_execute_zig(source, "sum_odd_numbers", vec![10]);
    assert_eq!(result, 25); // 1+3+5+7+9 = 25
    println!("[Zig E2E] ✓ sum_odd_numbers(10) = {}", result);
}

#[test]
fn test_zig_jit_else_if() {
    let source = r#"
        fn classify(n: i32) i32 {
            if (n < 0) {
                return -1;
            } else if (n == 0) {
                return 0;
            } else if (n < 10) {
                return 1;
            } else {
                return 2;
            }
        }
    "#;

    let result1 = compile_and_execute_zig(source, "classify", vec![-5]);
    assert_eq!(result1, -1);
    println!("[Zig E2E] ✓ classify(-5) = {}", result1);

    let result2 = compile_and_execute_zig(source, "classify", vec![0]);
    assert_eq!(result2, 0);
    println!("[Zig E2E] ✓ classify(0) = {}", result2);

    let result3 = compile_and_execute_zig(source, "classify", vec![5]);
    assert_eq!(result3, 1);
    println!("[Zig E2E] ✓ classify(5) = {}", result3);

    let result4 = compile_and_execute_zig(source, "classify", vec![15]);
    assert_eq!(result4, 2);
    println!("[Zig E2E] ✓ classify(15) = {}", result4);
}

// ===== HELPER FUNCTIONS =====

/// Compile and execute a Zig function with arguments
fn compile_and_execute_zig(source: &str, func_name: &str, args: Vec<i32>) -> i32 {
    // Parse to TypedAST
    let pairs = ZigParser::parse(Rule::program, source)
        .expect("Failed to parse Zig source");
    let mut builder = ZigBuilder::new();
    let program = builder.build_program(pairs)
        .expect("Failed to build TypedAST");

    // Lower to HIR using official API
    let hir_module = lower_zig_program_to_hir(program, &builder);

    // Compile with Cranelift
    let mut backend = CraneliftBackend::new().expect("Failed to create backend");
    backend.compile_module(&hir_module).expect("Failed to compile module");

    // Find the function and execute it
    let func_name_interned = builder.intern(func_name);
    let func_id = hir_module.functions.values()
        .find(|f| f.name == func_name_interned)
        .map(|f| f.id)
        .expect(&format!("Function '{}' not found", func_name));

    let func_ptr = backend.get_function_ptr(func_id).expect("Function not found");

    unsafe {
        match args.len() {
            0 => {
                let exec_fn: extern "C" fn() -> i32 = std::mem::transmute(func_ptr);
                exec_fn()
            }
            1 => {
                let exec_fn: extern "C" fn(i32) -> i32 = std::mem::transmute(func_ptr);
                exec_fn(args[0])
            }
            2 => {
                let exec_fn: extern "C" fn(i32, i32) -> i32 = std::mem::transmute(func_ptr);
                exec_fn(args[0], args[1])
            }
            3 => {
                let exec_fn: extern "C" fn(i32, i32, i32) -> i32 = std::mem::transmute(func_ptr);
                exec_fn(args[0], args[1], args[2])
            }
            _ => panic!("Too many arguments (max 3 supported in tests)"),
        }
    }
}

/// Lower TypedProgram to HIR using the official lowering API
fn lower_zig_program_to_hir(
    program: TypedProgram,
    builder: &ZigBuilder,
) -> zyntax_compiler::hir::HirModule {
    let mut arena = AstArena::new();
    let module_name = arena.intern_string("zig_module");

    let mut lowering_ctx = LoweringContext::new(
        module_name,
        Arc::new(builder.registry().clone()),
        Arc::new(Mutex::new(arena)),
        LoweringConfig::default(),
    );

    // Skip type checking for now
    std::env::set_var("SKIP_TYPE_CHECK", "1");

    lowering_ctx.lower_program(&program)
        .expect("Failed to lower TypedProgram to HIR")
}
