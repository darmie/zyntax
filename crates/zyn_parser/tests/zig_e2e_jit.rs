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
fn test_zig_jit_switch_expression() {
    let source = r#"
        fn get_name(x: i32) i32 {
            return switch (x) {
                1 => 100,
                2 => 200,
                3 => 300,
                else => 999
            };
        }
    "#;

    let result1 = compile_and_execute_zig(source, "get_name", vec![1]);
    assert_eq!(result1, 100);
    println!("[Zig E2E] ✓ switch(1) = {}", result1);

    let result2 = compile_and_execute_zig(source, "get_name", vec![2]);
    assert_eq!(result2, 200);
    println!("[Zig E2E] ✓ switch(2) = {}", result2);

    let result3 = compile_and_execute_zig(source, "get_name", vec![99]);
    assert_eq!(result3, 999);
    println!("[Zig E2E] ✓ switch(99) = {} (else)", result3);
}

#[test]
#[ignore] // Generic functions parse correctly, but monomorphization not yet connected
fn test_zig_jit_generic_function() {
    // Test that generic function syntax parses correctly
    // Note: Full monomorphization support requires connecting the existing monomorphize.rs
    let source = r#"
        fn identity(comptime T: type, x: T) T {
            return x;
        }

        fn test_generic() i32 {
            return identity(i32, 42);
        }
    "#;

    let result = compile_and_execute_zig(source, "test_generic", vec![]);
    assert_eq!(result, 42);
    println!("[Zig E2E] ✓ generic identity(i32, 42) = {}", result);
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

#[test]
fn test_zig_jit_try_expression() {
    // Test that try expression syntax parses correctly
    // Note: Full error propagation requires SSA translate_try_operator to be connected
    let source = r#"
        fn might_fail() !i32 {
            return 42;
        }

        fn use_try() i32 {
            const result = try might_fail();
            return result;
        }
    "#;

    let result = compile_and_execute_zig(source, "use_try", vec![]);
    assert_eq!(result, 42);
    println!("[Zig E2E] ✓ try expression = {}", result);
}

// ===== CLOSURE-LIKE CAPTURE TESTS =====
// These tests verify that loop variables are correctly captured/copied
// when stored into arrays (simulating closure capture semantics)

#[test]
fn test_zig_jit_loop_capture_to_array() {
    // Classic closure capture problem: store loop variable values into array
    // Each iteration should capture the CURRENT value of i, not a reference
    let source = r#"
        fn capture_loop_values() i32 {
            var results: [5]i32 = [_]i32{0, 0, 0, 0, 0};
            var i: i32 = 0;
            while (i < 5) {
                results[i] = i * 10;
                i = i + 1;
            }
            // Should return 0 + 10 + 20 + 30 + 40 = 100
            return results[0] + results[1] + results[2] + results[3] + results[4];
        }
    "#;

    let result = compile_and_execute_zig(source, "capture_loop_values", vec![]);
    assert_eq!(result, 100);
    println!("[Zig E2E] ✓ capture_loop_values() = {} (loop capture correct)", result);
}

#[test]
fn test_zig_jit_nested_loop_capture() {
    // Nested loops - both loop variables should be correctly captured
    let source = r#"
        fn nested_capture() i32 {
            var sum: i32 = 0;
            var i: i32 = 0;
            while (i < 3) {
                var j: i32 = 0;
                while (j < 3) {
                    sum = sum + i * 10 + j;
                    j = j + 1;
                }
                i = i + 1;
            }
            // i=0: 0+1+2=3, i=1: 10+11+12=33, i=2: 20+21+22=63 = 99
            return sum;
        }
    "#;

    let result = compile_and_execute_zig(source, "nested_capture", vec![]);
    assert_eq!(result, 99);
    println!("[Zig E2E] ✓ nested_capture() = {} (nested loop capture correct)", result);
}

#[test]
fn test_zig_jit_loop_with_conditional_capture() {
    // Loop with conditional - tests that captured value is correct even with branching
    let source = r#"
        fn conditional_capture() i32 {
            var evens: [3]i32 = [_]i32{0, 0, 0};
            var even_idx: i32 = 0;
            var i: i32 = 0;
            while (i < 6) {
                if (i % 2 == 0) {
                    evens[even_idx] = i;
                    even_idx = even_idx + 1;
                }
                i = i + 1;
            }
            // evens = [0, 2, 4], sum = 6
            return evens[0] + evens[1] + evens[2];
        }
    "#;

    let result = compile_and_execute_zig(source, "conditional_capture", vec![]);
    assert_eq!(result, 6);
    println!("[Zig E2E] ✓ conditional_capture() = {} (conditional capture correct)", result);
}

// ===== LAMBDA/CLOSURE TESTS =====
// These tests verify that lambda expressions parse and basic closure infrastructure works

#[test]
fn test_zig_jit_lambda_basic() {
    // Basic lambda without captures - just tests parsing
    let source = r#"
        fn test_lambda() i32 {
            const add = |x: i32, y: i32| x + y;
            return 42;
        }
    "#;

    let result = compile_and_execute_zig(source, "test_lambda", vec![]);
    assert_eq!(result, 42);
    println!("[Zig E2E] ✓ test_lambda() = {} (lambda parses)", result);
}

#[test]
fn test_zig_jit_lambda_with_capture() {
    // Lambda that captures outer variable - tests capture detection
    let source = r#"
        fn test_capture() i32 {
            const multiplier = 10;
            const scale = |x: i32| x * multiplier;
            return 42;
        }
    "#;

    let result = compile_and_execute_zig(source, "test_capture", vec![]);
    assert_eq!(result, 42);
    println!("[Zig E2E] ✓ test_capture() = {} (lambda with capture parses)", result);
}

#[test]
fn test_zig_jit_lambda_call() {
    // Lambda that we actually call - verifies end-to-end lambda execution
    let source = r#"
        fn test_call() i32 {
            const add = |x: i32, y: i32| x + y;
            return add(3, 5);
        }
    "#;

    let result = compile_and_execute_zig(source, "test_call", vec![]);
    assert_eq!(result, 8);
    println!("[Zig E2E] ✓ test_call() = {} (lambda call works)", result);
}

#[test]
fn test_zig_jit_lambda_capture_call() {
    // Lambda that captures outer variable and gets called
    // This tests that captured environment is correctly passed
    let source = r#"
        fn test_capture_call() i32 {
            const multiplier = 10;
            const scale = |x: i32| x * multiplier;
            return scale(7);
        }
    "#;

    let result = compile_and_execute_zig(source, "test_capture_call", vec![]);
    assert_eq!(result, 70);  // 7 * 10 = 70
    println!("[Zig E2E] ✓ test_capture_call() = {} (lambda with capture works)", result);
}

#[test]
fn test_zig_jit_lambda_call_in_loop() {
    // Lambda called multiple times in a loop
    // This tests that the lambda function pointer remains valid across iterations
    // Similar to thread spawns: each iteration should get the same function
    let source = r#"
        fn test_loop_call() i32 {
            const double = |x: i32| x * 2;
            var sum: i32 = 0;
            var i: i32 = 1;
            while (i <= 5) {
                sum = sum + double(i);
                i = i + 1;
            }
            return sum;
        }
    "#;

    // double(1) + double(2) + double(3) + double(4) + double(5)
    // = 2 + 4 + 6 + 8 + 10 = 30
    let result = compile_and_execute_zig(source, "test_loop_call", vec![]);
    assert_eq!(result, 30);
    println!("[Zig E2E] ✓ test_loop_call() = {} (lambda called in loop)", result);
}

#[test]
fn test_zig_jit_lambda_capture_in_loop() {
    // Lambda capturing a variable, called in loop with different arguments
    // This is the classic "closure capturing loop variable" scenario
    // The captured value should be fixed at lambda creation time
    let source = r#"
        fn test_capture_loop() i32 {
            const base = 100;
            const add_base = |x: i32| x + base;
            var sum: i32 = 0;
            var i: i32 = 1;
            while (i <= 3) {
                sum = sum + add_base(i);
                i = i + 1;
            }
            return sum;
        }
    "#;

    // add_base(1) + add_base(2) + add_base(3)
    // = (1+100) + (2+100) + (3+100) = 101 + 102 + 103 = 306
    let result = compile_and_execute_zig(source, "test_capture_loop", vec![]);
    assert_eq!(result, 306);
    println!("[Zig E2E] ✓ test_capture_loop() = {} (captured lambda in loop)", result);
}

#[test]
fn test_zig_jit_lambda_accumulator() {
    // Multiple lambda calls accumulating results
    // Simulates thread-like pattern: spawn work, collect results
    let source = r#"
        fn test_accumulator() i32 {
            const square = |x: i32| x * x;
            var total: i32 = 0;
            var i: i32 = 1;
            while (i <= 4) {
                total = total + square(i);
                i = i + 1;
            }
            return total;
        }
    "#;

    // 1^2 + 2^2 + 3^2 + 4^2 = 1 + 4 + 9 + 16 = 30
    let result = compile_and_execute_zig(source, "test_accumulator", vec![]);
    assert_eq!(result, 30);
    println!("[Zig E2E] ✓ test_accumulator() = {} (lambda as accumulator)", result);
}

#[test]
fn test_zig_jit_multiple_lambdas_in_loop() {
    // Multiple different lambdas called in the same loop
    // Like spawning different types of tasks
    let source = r#"
        fn test_multi_lambda() i32 {
            const inc = |x: i32| x + 1;
            const dec = |x: i32| x - 1;
            var val: i32 = 10;
            var i: i32 = 0;
            while (i < 3) {
                val = inc(val);
                val = dec(val);
                val = inc(val);
                i = i + 1;
            }
            return val;
        }
    "#;

    // Start: 10
    // Each iteration: +1, -1, +1 = net +1
    // After 3 iterations: 10 + 3 = 13
    let result = compile_and_execute_zig(source, "test_multi_lambda", vec![]);
    assert_eq!(result, 13);
    println!("[Zig E2E] ✓ test_multi_lambda() = {} (multiple lambdas in loop)", result);
}

// ===== BITWISE OPERATOR TESTS =====

#[test]
fn test_zig_jit_bitwise_and() {
    let source = r#"
        fn test_bitand() i32 {
            return 15 & 10;
        }
    "#;

    let result = compile_and_execute_zig(source, "test_bitand", vec![]);
    assert_eq!(result, 10);  // 0b1111 & 0b1010 = 0b1010 = 10
    println!("[Zig E2E] ✓ test_bitand() = {} (bitwise AND)", result);
}

#[test]
fn test_zig_jit_bitwise_or() {
    let source = r#"
        fn test_bitor() i32 {
            return 12 | 3;
        }
    "#;

    let result = compile_and_execute_zig(source, "test_bitor", vec![]);
    assert_eq!(result, 15);  // 0b1100 | 0b0011 = 0b1111 = 15
    println!("[Zig E2E] ✓ test_bitor() = {} (bitwise OR)", result);
}

#[test]
fn test_zig_jit_bitwise_xor() {
    let source = r#"
        fn test_bitxor() i32 {
            return 15 ^ 10;
        }
    "#;

    let result = compile_and_execute_zig(source, "test_bitxor", vec![]);
    assert_eq!(result, 5);  // 0b1111 ^ 0b1010 = 0b0101 = 5
    println!("[Zig E2E] ✓ test_bitxor() = {} (bitwise XOR)", result);
}

#[test]
fn test_zig_jit_bit_shift_left() {
    let source = r#"
        fn test_shl() i32 {
            return 1 << 4;
        }
    "#;

    let result = compile_and_execute_zig(source, "test_shl", vec![]);
    assert_eq!(result, 16);  // 1 << 4 = 16
    println!("[Zig E2E] ✓ test_shl() = {} (bit shift left)", result);
}

#[test]
fn test_zig_jit_bit_shift_right() {
    let source = r#"
        fn test_shr() i32 {
            return 32 >> 2;
        }
    "#;

    let result = compile_and_execute_zig(source, "test_shr", vec![]);
    assert_eq!(result, 8);  // 32 >> 2 = 8
    println!("[Zig E2E] ✓ test_shr() = {} (bit shift right)", result);
}

#[test]
fn test_zig_jit_bitwise_complex() {
    // Complex bitwise expression: (a | b) & (c ^ 15)
    let source = r#"
        fn test_complex() i32 {
            const a = 12;
            const b = 3;
            const c = 5;
            return (a | b) & (c ^ 15);
        }
    "#;

    // a | b = 15, c ^ 15 = 10, result = 15 & 10 = 10
    let result = compile_and_execute_zig(source, "test_complex", vec![]);
    assert_eq!(result, 10);
    println!("[Zig E2E] ✓ test_complex() = {} (complex bitwise)", result);
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
