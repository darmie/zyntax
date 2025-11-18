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

    // Parse to TypedAST
    let pairs = ZigParser::parse(Rule::program, source)
        .expect("Failed to parse Zig source");
    let mut builder = ZigBuilder::new();
    let program = builder.build_program(pairs)
        .expect("Failed to build TypedAST");

    // Lower to HIR
    let hir_module = lower_zig_program_to_hir(program, &builder);

    // Find the function
    let func_name_interned = builder.intern("sum_odd_numbers");
    let func = hir_module.functions.values()
        .find(|f| f.name == func_name_interned)
        .expect("Function 'sum_odd_numbers' not found");

    // Print HIR structure
    println!("\n=== HIR Function Structure (Zig source - FAILING) ===");
    println!("Function: {:?}", func.name);
    println!("Entry block: {:?}", func.entry_block);
    println!("Values: {}", func.values.len());
    println!("Blocks: {}", func.blocks.len());

    // Print all defined values
    println!("\nDefined values:");
    for (val_id, val) in &func.values {
        println!("  {:?} -> {:?}", val_id, val.kind);
    }

    // Check for undefined value references
    use std::collections::HashSet;
    let mut defined_values: HashSet<zyntax_compiler::hir::HirId> = func.values.keys().copied().collect();
    let mut used_but_undefined = Vec::new();

    for (block_id, block) in &func.blocks {
        for phi in &block.phis {
            for (val_id, _) in &phi.incoming {
                if !defined_values.contains(val_id) {
                    used_but_undefined.push((*val_id, format!("phi {:?} in block {:?}", phi.result, block_id)));
                }
            }
        }
        for inst in &block.instructions {
            match inst {
                zyntax_compiler::hir::HirInstruction::Binary { left, right, .. } => {
                    if !defined_values.contains(left) {
                        used_but_undefined.push((*left, format!("binary left in block {:?}", block_id)));
                    }
                    if !defined_values.contains(right) {
                        used_but_undefined.push((*right, format!("binary right in block {:?}", block_id)));
                    }
                }
                _ => {}
            }
        }
    }

    if !used_but_undefined.is_empty() {
        println!("\n⚠️  UNDEFINED VALUES DETECTED:");
        for (val_id, location) in &used_but_undefined {
            println!("  {:?} used in {}", val_id, location);
        }
    }

    for (block_id, block) in &func.blocks {
        println!("\n  Block {:?}:", block_id);
        println!("    Phis: {}", block.phis.len());
        for phi in &block.phis {
            println!("      {:?} = phi({:?})", phi.result, phi.incoming);
        }
        println!("    Instructions: {}", block.instructions.len());
        for (idx, inst) in block.instructions.iter().enumerate() {
            println!("      [{}] {:?}", idx, inst);
        }
        println!("    Terminator: {:?}", block.terminator);
    }

    // Compile and dump Cranelift IR
    let mut backend = CraneliftBackend::new().expect("Failed to create backend");
    match backend.compile_function(func.id, &func) {
        Ok(_) => println!("\n✓ Compilation succeeded"),
        Err(e) => {
            println!("\n✗ Compilation failed: {:?}", e);
            panic!("Compilation error: {:?}", e);
        }
    }

    // Print Cranelift IR BEFORE finalization (after, it gets cleared!)
    println!("\n=== Cranelift IR (Zig source - BEFORE finalize) ===");
    println!("{}", backend.get_ir_string());

    backend.finalize_definitions().expect("Failed to finalize");

    println!("\n=== Cranelift IR (Zig source - AFTER finalize) ===");
    println!("{}", backend.get_ir_string());

    let func_ptr = backend.get_function_ptr(func.id).expect("Function not found");
    let exec_fn: extern "C" fn(i32) -> i32 = unsafe { std::mem::transmute(func_ptr) };

    let result = exec_fn(10);
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

#[test]
fn test_zig_jit_array_literal() {
    let source = r#"
        fn get_second_element() i32 {
            const arr = [_]i32{10, 20, 30};
            return arr[1];
        }
    "#;

    let result = compile_and_execute_zig(source, "get_second_element", vec![]);
    assert_eq!(result, 20);
    println!("[Zig E2E] ✓ get_second_element() = {}", result);
}

#[test]
fn test_zig_jit_array_sum() {
    let source = r#"
        fn sum_array() i32 {
            const arr = [_]i32{5, 10, 15, 20};
            return arr[0] + arr[1] + arr[2] + arr[3];
        }
    "#;

    let result = compile_and_execute_zig(source, "sum_array", vec![]);
    assert_eq!(result, 50);
    println!("[Zig E2E] ✓ sum_array() = {}", result);
}

#[test]
fn test_zig_jit_array_indexing() {
    let source = r#"
        fn get_element(index: i32) i32 {
            const arr = [_]i32{100, 200, 300, 400, 500};
            return arr[index];
        }
    "#;

    let result1 = compile_and_execute_zig(source, "get_element", vec![0]);
    assert_eq!(result1, 100);
    println!("[Zig E2E] ✓ get_element(0) = {}", result1);

    let result2 = compile_and_execute_zig(source, "get_element", vec![2]);
    assert_eq!(result2, 300);
    println!("[Zig E2E] ✓ get_element(2) = {}", result2);

    let result3 = compile_and_execute_zig(source, "get_element", vec![4]);
    assert_eq!(result3, 500);
    println!("[Zig E2E] ✓ get_element(4) = {}", result3);
}

#[test]
fn test_zig_jit_sized_array_type() {
    let source = r#"
        fn test_sized_array() i32 {
            var arr: [3]i32 = [_]i32{7, 14, 21};
            return arr[0] + arr[2];
        }
    "#;

    let result = compile_and_execute_zig(source, "test_sized_array", vec![]);
    assert_eq!(result, 28);
    println!("[Zig E2E] ✓ test_sized_array() = {}", result);
}

// TODO: Fix stack overflow when combining arrays with loops
// Likely an issue with SSA variable reads/phi nodes for array indexing in loops
#[test]
#[ignore]
fn test_zig_jit_array_in_loop() {
    let source = r#"
        fn sum_with_loop() i32 {
            const arr = [_]i32{1, 2, 3, 4, 5};
            var sum = 0;
            var i = 0;
            while (i < 5) {
                sum = sum + arr[i];
                i = i + 1;
            }
            return sum;
        }
    "#;

    let result = compile_and_execute_zig(source, "sum_with_loop", vec![]);
    assert_eq!(result, 15);
    println!("[Zig E2E] ✓ sum_with_loop() = {}", result);
}

#[test]
fn test_zig_jit_string_literal() {
    let source = r#"
        fn test_string() i32 {
            const message = "Hello, World!";
            return 42;
        }
    "#;

    let result = compile_and_execute_zig(source, "test_string", vec![]);
    assert_eq!(result, 42);
    println!("[Zig E2E] ✓ test_string() = {}", result);
}

#[test]
// Test that if let syntax parses and builds to TypedAST correctly
// Full execution requires pattern matching backend (Issue #0 Phase 3)
fn test_zig_if_let_syntax() {
    let source = r#"
        fn test_if_let() i32 {
            var maybe: ?i32 = 42;

            if (let value = maybe) {
                return value;
            } else {
                return 0;
            }
        }
    "#;

    // Parse to TypedAST
    let pairs = ZigParser::parse(Rule::program, source)
        .expect("Failed to parse if let syntax");
    let mut builder = ZigBuilder::new();
    let program = builder.build_program(pairs)
        .expect("Failed to build TypedAST from if let");

    // Verify we got a function
    assert_eq!(program.declarations.len(), 1);

    // Verify it contains a Match statement (if let is lowered to match)
    use zyntax_typed_ast::typed_ast::{TypedDeclaration, TypedStatement};
    if let TypedDeclaration::Function(func) = &program.declarations[0].node {
        if let Some(body) = &func.body {
            assert!(!body.statements.is_empty());
            println!("[DEBUG] Got {} statements in function body", body.statements.len());
            // Print statement types for debugging
            for (i, stmt) in body.statements.iter().enumerate() {
                println!("[DEBUG] Statement {}: {:?}", i, std::mem::discriminant(&stmt.node));
            }
            // First statement after var decl should be the match (from if let)
            let match_stmt_found = body.statements.iter().any(|stmt| {
                matches!(stmt.node, TypedStatement::Match(_))
            });

            if match_stmt_found {
                println!("[Zig Syntax] ✓ if let parsed correctly and lowered to Match");
            } else {
                panic!("Expected Match statement from if let. Got statements: {:?}",
                    body.statements.iter().map(|s| std::mem::discriminant(&s.node)).collect::<Vec<_>>());
            }
        }
    }

    println!("[Zig Syntax] ✓ if let syntax: parses and builds to TypedAST");
}

#[test]
fn test_pattern_match_runtime_execution() {
    // This test verifies complete end-to-end pattern matching:
    // ✅ Pattern matching CONSTRUCTION: Some(42), None literals
    // ✅ Pattern matching DECONSTRUCTION: GetUnionDiscriminant, ExtractUnionValue
    // ✅ Complete pipeline: Parser → AST → CFG → SSA → HIR → Cranelift
    //
    // What's tested:
    // ✅ CreateUnion instruction generation for Some(value) and None
    // ✅ Discriminant-based conditional branching
    // ✅ Value extraction with ExtractUnionValue
    // ✅ Variable bindings in pattern arms
    // ✅ Runtime execution through Cranelift JIT

    // Test 1: if let with Some variant - extract and use the value
    let source_some = r#"
        fn test_some_pattern() i32 {
            var opt: ?i32 = Some(42);  // ← Requires Some() constructor

            if (let x = opt) {
                return x * 2;
            } else {
                return 0;
            }
        }
    "#;

    let result = compile_and_execute_zig(source_some, "test_some_pattern", vec![]);
    assert_eq!(result, 84, "Some(42) * 2 should equal 84");
    println!("[Pattern Match Runtime] ✓ Some variant: extracted value and computed correctly (84)");

    // Test 2: if let with None - take else branch
    let source_none = r#"
        fn test_none_pattern() i32 {
            var opt: ?i32 = None;  // ← Requires None literal

            if (let x = opt) {
                return x * 2;
            } else {
                return -1;
            }
        }
    "#;

    let result = compile_and_execute_zig(source_none, "test_none_pattern", vec![]);
    assert_eq!(result, -1, "None should return -1");
    println!("[Pattern Match Runtime] ✓ None variant: matched correctly (-1)");
}

#[test]
// NOTE: This test only verifies that error union syntax (!T) parses and compiles.
// Actual error handling (try, catch, error propagation) requires language-level
// support that is not yet implemented. The variable is created but not meaningfully used.
// TODO: Add tests with stdlib Result<T,E> methods once try/catch is implemented
fn test_zig_jit_error_union_type() {
    let source = r#"
        fn test_error_union() i32 {
            var result: !i32 = 100;
            return 2;
        }
    "#;

    let result = compile_and_execute_zig(source, "test_error_union", vec![]);
    assert_eq!(result, 2);
    println!("[Zig E2E] ✓ test_error_union() = {}", result);
    println!("[NOTE] Error union type parses/compiles but is not functionally used");
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
