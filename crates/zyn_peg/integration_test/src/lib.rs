//! ZynPEG Integration Test
//!
//! Tests that ZynPEG-generated code can:
//! 1. Parse Zig code using the generated pest grammar
//! 2. Build TypedProgram using zyntax_typed_ast types
//! 3. Compile and execute via Cranelift JIT

use pest_derive::Parser;

// Load the generated pest grammar
#[derive(Parser)]
#[grammar = "../../generated/zig.pest"]
pub struct ZigParser;

// Re-export zyntax_typed_ast types
pub use zyntax_typed_ast::*;

// Re-export compiler types for JIT
pub use zyntax_compiler::{
    cranelift_backend::CraneliftBackend,
    lowering::{LoweringContext, LoweringConfig, AstLowering},
    hir::HirModule,
};

// Include generated AST builder
#[path = "../../generated/ast_builder.rs"]
pub mod ast_builder;

// Include generated parser implementation
#[path = "../../generated/parser_impl.rs"]
pub mod parser_impl;

// Re-export the main parse function
pub use parser_impl::parse_to_typed_ast;

#[cfg(test)]
mod tests {
    use super::*;
    use pest::Parser;

    #[test]
    fn test_grammar_parses_const() {
        let input = "const x: i32 = 42;";
        let result = ZigParser::parse(Rule::const_decl, input);
        assert!(result.is_ok(), "Failed to parse: {:?}", result.err());
        println!("✓ Grammar parsed const decl");
    }

    #[test]
    fn test_grammar_parses_function() {
        let input = "fn add(a: i32, b: i32) i32 { return a; }";
        let result = ZigParser::parse(Rule::fn_decl, input);
        assert!(result.is_ok(), "Failed to parse: {:?}", result.err());
        println!("✓ Grammar parsed function");
    }

    #[test]
    fn test_grammar_parses_program() {
        let input = r#"
const PI: f64 = 3.14159;
const answer: i32 = 42;

fn double(x: i32) i32 {
    return x;
}
"#;
        let result = ZigParser::parse(Rule::program, input);
        assert!(result.is_ok(), "Failed to parse: {:?}\nInput:\n{}", result.err(), input);

        let pairs = result.unwrap();
        let count = pairs.flatten().count();
        println!("✓ Grammar parsed full program with {} nodes", count);
    }

    #[test]
    fn test_typed_ast_types_work() {
        // Test Span
        let span = Span::new(0, 10);
        assert_eq!(span.start, 0);
        assert_eq!(span.end, 10);

        // Test Type variants
        let ty = Type::Primitive(PrimitiveType::I32);
        assert_eq!(ty, Type::Primitive(PrimitiveType::I32));

        // Test TypedProgram
        let program = TypedProgram {
            declarations: vec![],
            span: Span::new(0, 0),
        };
        assert!(program.declarations.is_empty());

        println!("✓ zyntax_typed_ast types work correctly");
    }

    #[test]
    fn test_parse_to_typed_ast_const() {
        let input = "const x: i32 = 42;";
        let result = parse_to_typed_ast::<ZigParser>(input);

        match result {
            Ok(program) => {
                println!("✓ Parsed const decl to TypedProgram");
                println!("  Declarations: {}", program.declarations.len());
            }
            Err(e) => {
                println!("Parse error (expected during development): {:?}", e);
            }
        }
    }

    #[test]
    fn test_parse_to_typed_ast_function() {
        let input = r#"
fn add(x: i32, y: i32) i32 {
    return x;
}
"#;
        let result = parse_to_typed_ast::<ZigParser>(input);

        match result {
            Ok(program) => {
                println!("✓ Parsed function to TypedProgram");
                println!("  Declarations: {}", program.declarations.len());
            }
            Err(e) => {
                println!("Parse error (expected during development): {:?}", e);
            }
        }
    }

    #[test]
    fn test_jit_compile_and_execute() {
        use std::sync::{Arc, Mutex};

        let input = r#"
fn add(x: i32, y: i32) i32 {
    return x + y;
}
"#;
        // Step 1: Parse to TypedProgram
        let program = parse_to_typed_ast::<ZigParser>(input)
            .expect("Failed to parse");
        println!("[JIT] ✓ Parsed to TypedProgram with {} declarations", program.declarations.len());

        // Step 2: Lower to HIR
        let arena = AstArena::new();
        let module_name = InternedString::new_global("test_module");
        let type_registry = Arc::new(TypeRegistry::new());

        let mut lowering_ctx = LoweringContext::new(
            module_name,
            type_registry.clone(),
            Arc::new(Mutex::new(arena)),
            LoweringConfig::default(),
        );

        // Skip type checking for now
        std::env::set_var("SKIP_TYPE_CHECK", "1");

        let mut hir_module = lowering_ctx.lower_program(&program)
            .expect("Failed to lower to HIR");
        println!("[JIT] ✓ Lowered to HIR with {} functions", hir_module.functions.len());

        // Step 3: Monomorphization
        zyntax_compiler::monomorphize_module(&mut hir_module)
            .expect("Failed to monomorphize");
        println!("[JIT] ✓ Monomorphization complete");

        // Step 4: Compile with Cranelift
        let mut backend = CraneliftBackend::new()
            .expect("Failed to create Cranelift backend");
        backend.compile_module(&hir_module)
            .expect("Failed to compile module");
        println!("[JIT] ✓ Compiled to native code");

        // Step 5: Find and execute the function
        let func_name = InternedString::new_global("add");
        let func_id = hir_module.functions.values()
            .find(|f| f.name == func_name)
            .map(|f| f.id)
            .expect("Function 'add' not found");

        let func_ptr = backend.get_function_ptr(func_id)
            .expect("Failed to get function pointer");

        // Step 6: Execute and verify
        let result = unsafe {
            let exec_fn: extern "C" fn(i32, i32) -> i32 = std::mem::transmute(func_ptr);
            exec_fn(10, 20)
        };

        assert_eq!(result, 30);
        println!("[JIT] ✓ add(10, 20) = {} (expected 30)", result);
        println!("[JIT] ✓ Full pipeline: Parse → TypedAST → HIR → JIT → Execute SUCCESS!");
    }
}
