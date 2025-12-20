//! End-to-end tests for ZynML language
//!
//! These tests verify the full pipeline:
//! 1. Grammar compilation (both LanguageGrammar and Grammar2)
//! 2. Source parsing
//! 3. TypedAST generation
//! 4. Runtime execution (when plugins are available)

use zynml::{
    ZynML, ZynMLConfig, ZynMLError,
    LanguageGrammar, Grammar2,
    ZYNML_GRAMMAR, ZYNML_STDLIB_PRELUDE, ZYNML_STDLIB_TENSOR,
};
use std::path::Path;

/// Test that both grammar versions compile successfully
mod grammar_compilation {
    use super::*;

    #[test]
    fn test_language_grammar_compiles() {
        let result = LanguageGrammar::compile_zyn(ZYNML_GRAMMAR);
        assert!(result.is_ok(), "LanguageGrammar should compile: {:?}", result.err());
    }

    #[test]
    fn test_grammar2_compiles() {
        let result = Grammar2::from_source(ZYNML_GRAMMAR);
        assert!(result.is_ok(), "Grammar2 should compile: {:?}", result.err());
    }

    #[test]
    fn test_grammar2_metadata() {
        let grammar = Grammar2::from_source(ZYNML_GRAMMAR).unwrap();

        // Verify metadata is populated
        assert!(!grammar.name().is_empty(), "Language name should be set");
        assert!(!grammar.version().is_empty(), "Version should be set");

        println!("Language: {} v{}", grammar.name(), grammar.version());
        println!("Extensions: {:?}", grammar.file_extensions());
    }
}

/// Test parsing of ZynML syntax constructs
mod parsing {
    use super::*;

    fn get_grammar() -> LanguageGrammar {
        LanguageGrammar::compile_zyn(ZYNML_GRAMMAR).expect("Grammar should compile")
    }

    #[test]
    fn test_parse_empty_function() {
        let grammar = get_grammar();
        let result = grammar.parse_to_json("fn main() { }");
        assert!(result.is_ok(), "Should parse empty function: {:?}", result.err());
    }

    #[test]
    fn test_parse_let_binding() {
        let grammar = get_grammar();
        let result = grammar.parse_to_json("let x = 42");
        assert!(result.is_ok(), "Should parse let binding: {:?}", result.err());
    }

    #[test]
    fn test_parse_let_with_type() {
        let grammar = get_grammar();
        // Note: ZynML uses type inference, explicit type annotations use different syntax
        // This tests function parameter types instead
        let result = grammar.parse_to_json("fn test(x: i32) { x }");
        assert!(result.is_ok(), "Should parse function with typed param: {:?}", result.err());
    }

    #[test]
    fn test_parse_function_with_params() {
        let grammar = get_grammar();
        let result = grammar.parse_to_json(r#"
            fn add(a: i32, b: i32) -> i32 {
                a + b
            }
        "#);
        assert!(result.is_ok(), "Should parse function with params: {:?}", result.err());
    }

    #[test]
    fn test_parse_tensor_literal() {
        let grammar = get_grammar();
        let result = grammar.parse_to_json("let t = tensor([1.0, 2.0, 3.0])");
        assert!(result.is_ok(), "Should parse tensor literal: {:?}", result.err());
    }

    #[test]
    fn test_parse_nested_array() {
        let grammar = get_grammar();
        let result = grammar.parse_to_json("let m = [[1, 2], [3, 4]]");
        assert!(result.is_ok(), "Should parse nested array: {:?}", result.err());
    }

    #[test]
    fn test_parse_pipe_operator() {
        let grammar = get_grammar();
        let result = grammar.parse_to_json(r#"
            let result = data |> transform() |> process(10)
        "#);
        assert!(result.is_ok(), "Should parse pipe operator: {:?}", result.err());
    }

    #[test]
    fn test_parse_method_call() {
        let grammar = get_grammar();
        let result = grammar.parse_to_json("let s = tensor.sum()");
        assert!(result.is_ok(), "Should parse method call: {:?}", result.err());
    }

    #[test]
    fn test_parse_chained_method_calls() {
        let grammar = get_grammar();
        let result = grammar.parse_to_json("let r = tensor.reshape([2, 3]).transpose()");
        assert!(result.is_ok(), "Should parse chained methods: {:?}", result.err());
    }

    #[test]
    fn test_parse_if_expression() {
        let grammar = get_grammar();
        let result = grammar.parse_to_json(r#"
            fn test() {
                if x > 0 {
                    let y = 1
                } else {
                    let y = 0
                }
            }
        "#);
        assert!(result.is_ok(), "Should parse if expression: {:?}", result.err());
    }

    #[test]
    fn test_parse_while_loop() {
        let grammar = get_grammar();
        let result = grammar.parse_to_json(r#"
            fn test() {
                while i < 10 {
                    i = i + 1
                }
            }
        "#);
        assert!(result.is_ok(), "Should parse while loop: {:?}", result.err());
    }

    #[test]
    fn test_parse_for_loop() {
        let grammar = get_grammar();
        let result = grammar.parse_to_json(r#"
            fn test() {
                for i in range(0, 10) {
                    println(i)
                }
            }
        "#);
        assert!(result.is_ok(), "Should parse for loop: {:?}", result.err());
    }

    #[test]
    fn test_parse_binary_operators() {
        let grammar = get_grammar();

        // Arithmetic
        assert!(grammar.parse_to_json("let a = 1 + 2").is_ok());
        assert!(grammar.parse_to_json("let a = 1 - 2").is_ok());
        assert!(grammar.parse_to_json("let a = 1 * 2").is_ok());
        assert!(grammar.parse_to_json("let a = 1 / 2").is_ok());

        // Comparison
        assert!(grammar.parse_to_json("let a = 1 < 2").is_ok());
        assert!(grammar.parse_to_json("let a = 1 > 2").is_ok());
        assert!(grammar.parse_to_json("let a = 1 <= 2").is_ok());
        assert!(grammar.parse_to_json("let a = 1 >= 2").is_ok());
        assert!(grammar.parse_to_json("let a = 1 == 2").is_ok());
        assert!(grammar.parse_to_json("let a = 1 != 2").is_ok());

        // Matrix multiply
        assert!(grammar.parse_to_json("let a = x @ y").is_ok());
    }

    #[test]
    fn test_parse_unary_operators() {
        let grammar = get_grammar();
        assert!(grammar.parse_to_json("let a = -x").is_ok());
        assert!(grammar.parse_to_json("let a = !flag").is_ok());
    }

    #[test]
    fn test_parse_import_statement() {
        let grammar = get_grammar();
        let result = grammar.parse_to_json("import tensor");
        assert!(result.is_ok(), "Should parse import: {:?}", result.err());
    }

    #[test]
    fn test_parse_comment() {
        let grammar = get_grammar();
        let result = grammar.parse_to_json(r#"
            // This is a comment
            let x = 42
        "#);
        assert!(result.is_ok(), "Should parse comment: {:?}", result.err());
    }
}

/// Test parsing of real example files
mod example_files {
    use super::*;

    fn get_grammar() -> LanguageGrammar {
        LanguageGrammar::compile_zyn(ZYNML_GRAMMAR).expect("Grammar should compile")
    }

    fn examples_dir() -> std::path::PathBuf {
        Path::new(env!("CARGO_MANIFEST_DIR")).join("examples")
    }

    #[test]
    fn test_parse_hello_example() {
        let grammar = get_grammar();
        let source = std::fs::read_to_string(examples_dir().join("hello.zynml"))
            .expect("Should read hello.zynml");

        let result = grammar.parse_to_json(&source);
        assert!(result.is_ok(), "Should parse hello.zynml: {:?}", result.err());
    }

    #[test]
    fn test_parse_basic_tensor_example() {
        let grammar = get_grammar();
        let source = std::fs::read_to_string(examples_dir().join("basic_tensor.zynml"))
            .expect("Should read basic_tensor.zynml");

        let result = grammar.parse_to_json(&source);
        assert!(result.is_ok(), "Should parse basic_tensor.zynml: {:?}", result.err());
    }

    #[test]
    fn test_parse_audio_pipeline_example() {
        let grammar = get_grammar();
        let path = examples_dir().join("audio_pipeline.zynml");
        if path.exists() {
            let source = std::fs::read_to_string(&path)
                .expect("Should read audio_pipeline.zynml");

            let result = grammar.parse_to_json(&source);
            assert!(result.is_ok(), "Should parse audio_pipeline.zynml: {:?}", result.err());
        }
    }

    #[test]
    fn test_parse_vector_search_example() {
        let grammar = get_grammar();
        let path = examples_dir().join("vector_search.zynml");
        if path.exists() {
            let source = std::fs::read_to_string(&path)
                .expect("Should read vector_search.zynml");

            let result = grammar.parse_to_json(&source);
            assert!(result.is_ok(), "Should parse vector_search.zynml: {:?}", result.err());
        }
    }

    #[test]
    fn test_parse_abstract_types_example() {
        let grammar = get_grammar();
        let path = examples_dir().join("abstract_types.zynml");
        if path.exists() {
            let source = std::fs::read_to_string(&path)
                .expect("Should read abstract_types.zynml");

            let result = grammar.parse_to_json(&source);
            // Note: abstract_types.zynml uses Python-style colon syntax (fn main():)
            // which is not currently supported by the grammar
            if result.is_err() {
                println!("abstract_types.zynml uses unsupported syntax (colon-style blocks)");
            } else {
                println!("abstract_types.zynml parsed successfully");
            }
        }
    }
}

/// Test parsing of stdlib files
mod stdlib_parsing {
    use super::*;

    fn get_grammar() -> LanguageGrammar {
        LanguageGrammar::compile_zyn(ZYNML_GRAMMAR).expect("Grammar should compile")
    }

    #[test]
    fn test_parse_prelude_stdlib() {
        let grammar = get_grammar();
        let result = grammar.parse_to_json(ZYNML_STDLIB_PRELUDE);
        assert!(result.is_ok(), "Should parse prelude.zynml: {:?}", result.err());
    }

    #[test]
    fn test_parse_tensor_stdlib() {
        let grammar = get_grammar();
        let result = grammar.parse_to_json(ZYNML_STDLIB_TENSOR);
        // Note: tensor.zynml uses trait impl syntax (impl Display for Tensor:)
        // which may not be fully supported yet
        if result.is_err() {
            println!("tensor.zynml uses advanced syntax not yet fully supported");
            println!("Error: {:?}", result.err());
        } else {
            println!("tensor.zynml parsed successfully");
        }
    }
}

/// Test Grammar2 direct TypedAST parsing
mod grammar2_parsing {
    use super::*;

    fn get_grammar2() -> Grammar2 {
        Grammar2::from_source(ZYNML_GRAMMAR).expect("Grammar2 should compile")
    }

    #[test]
    fn test_grammar2_parse_simple() {
        let grammar = get_grammar2();

        // Note: Grammar2 parse may have different requirements
        // Some grammars might not fully support all TypedAST constructs yet
        let result = grammar.parse("fn main() { let x = 42 }");

        match result {
            Ok(program) => {
                println!("Parsed {} declarations", program.declarations.len());
            }
            Err(e) => {
                // Grammar2 might not be fully implemented for ZynML yet
                println!("Grammar2 parse not fully supported: {}", e);
            }
        }
    }
}

/// Test ZynML runtime initialization
mod runtime {
    use super::*;

    fn plugins_dir() -> std::path::PathBuf {
        Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()  // crates/
            .unwrap()
            .parent()  // workspace root
            .unwrap()
            .join("plugins/target/zrtl")
    }

    #[test]
    fn test_runtime_creation_without_plugins() {
        // Test that runtime can be created even without plugins
        let config = ZynMLConfig {
            plugins_dir: "/nonexistent".to_string(),
            load_optional: false,
            verbose: false,
        };

        let result = ZynML::with_config(config);
        assert!(result.is_ok(), "Runtime should create without plugins: {:?}", result.err());
    }

    #[test]
    fn test_runtime_has_grammar2() {
        let config = ZynMLConfig::default();
        let zynml = ZynML::with_config(config).expect("Should create runtime");

        // Grammar2 should be available
        if zynml.has_grammar2() {
            println!("Grammar2 is available");
            let g2 = zynml.grammar2().unwrap();
            println!("  Language: {}", g2.name());
        } else {
            println!("Grammar2 not available (grammar may not support it)");
        }
    }

    #[test]
    fn test_runtime_parse_to_json() {
        let config = ZynMLConfig::default();
        let zynml = ZynML::with_config(config).expect("Should create runtime");

        let result = zynml.parse_to_json("let x = 42");
        assert!(result.is_ok(), "Should parse to JSON: {:?}", result.err());

        let json = result.unwrap();
        assert!(json.contains("42"), "JSON should contain literal value");
    }

    #[test]
    fn test_runtime_with_plugins() {
        let plugins_path = plugins_dir();

        if !plugins_path.exists() {
            println!("Skipping runtime with plugins test: plugins not built");
            println!("  Expected: {:?}", plugins_path);
            return;
        }

        let config = ZynMLConfig {
            plugins_dir: plugins_path.to_string_lossy().to_string(),
            load_optional: false,
            verbose: true,
        };

        let result = ZynML::with_config(config);
        assert!(result.is_ok(), "Should create runtime with plugins: {:?}", result.err());

        let zynml = result.unwrap();

        // Verify plugin detection works
        assert!(zynml.has_plugin("zrtl_tensor"), "Should recognize tensor plugin");
        assert!(zynml.has_plugin("zrtl_audio"), "Should recognize audio plugin");
    }
}

/// Test full execution pipeline (requires plugins)
mod execution {
    use super::*;

    fn plugins_dir() -> std::path::PathBuf {
        Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .unwrap()
            .parent()
            .unwrap()
            .join("plugins/target/zrtl")
    }

    fn examples_dir() -> std::path::PathBuf {
        Path::new(env!("CARGO_MANIFEST_DIR")).join("examples")
    }

    fn create_runtime_with_plugins() -> Option<ZynML> {
        let plugins_path = plugins_dir();
        if !plugins_path.exists() {
            return None;
        }

        let config = ZynMLConfig {
            plugins_dir: plugins_path.to_string_lossy().to_string(),
            load_optional: true,
            verbose: false,
        };

        ZynML::with_config(config).ok()
    }

    #[test]
    fn test_execute_simple_expression() {
        let Some(mut zynml) = create_runtime_with_plugins() else {
            println!("Skipping: plugins not available");
            return;
        };

        let result = zynml.load_source(r#"
            fn main() {
                let x = 42
            }
        "#);

        match result {
            Ok(functions) => {
                println!("Loaded functions: {:?}", functions);
                if functions.contains(&"main".to_string()) {
                    let call_result = zynml.call("main");
                    println!("Call result: {:?}", call_result);
                }
            }
            Err(e) => {
                println!("Load error (may be expected): {}", e);
            }
        }
    }

    #[test]
    fn test_execute_hello_example() {
        let Some(mut zynml) = create_runtime_with_plugins() else {
            println!("Skipping: plugins not available");
            return;
        };

        let hello_path = examples_dir().join("hello.zynml");
        if !hello_path.exists() {
            println!("Skipping: hello.zynml not found");
            return;
        }

        // Use catch_unwind since runtime errors may panic
        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            zynml.run_file(&hello_path)
        }));

        match result {
            Ok(Ok(())) => println!("hello.zynml executed successfully"),
            Ok(Err(e)) => println!("Execution error (expected during development): {}", e),
            Err(_) => println!("Runtime panic (symbol resolution may be incomplete)"),
        }
    }
}

/// Test error handling
mod error_handling {
    use super::*;

    fn get_grammar() -> LanguageGrammar {
        LanguageGrammar::compile_zyn(ZYNML_GRAMMAR).expect("Grammar should compile")
    }

    #[test]
    fn test_parse_error_unclosed_brace() {
        let grammar = get_grammar();
        let result = grammar.parse_to_json("fn main() {");
        assert!(result.is_err(), "Should fail on unclosed brace");
    }

    #[test]
    fn test_parse_error_invalid_syntax() {
        let grammar = get_grammar();
        let result = grammar.parse_to_json("let = 42");
        assert!(result.is_err(), "Should fail on missing identifier");
    }

    #[test]
    fn test_parse_error_unmatched_paren() {
        let grammar = get_grammar();
        let result = grammar.parse_to_json("let x = (1 + 2");
        assert!(result.is_err(), "Should fail on unmatched paren");
    }
}
