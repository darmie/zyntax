//! ZynPEG grammar-based compilation format
//!
//! Workflow (No Rust compilation required!):
//! 1. Parse .zyn grammar with ZynPEG
//! 2. Compile grammar to zpeg module (pest grammar + JSON commands)
//! 3. Parse source code with pest_vm (dynamic grammar)
//! 4. Execute JSON commands via host functions to build TypedAST
//! 5. Lower TypedAST to HIR and compile
//!
//! Usage:
//! ```bash
//! zyntax compile --source my_code.lang --grammar my_lang.zyn --format zyn -o output --run
//! ```

use colored::Colorize;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};

use zyntax_compiler::hir::HirModule;
use zyntax_compiler::lowering::{AstLowering, LoweringConfig, LoweringContext};
use zyntax_typed_ast::{AstArena, InternedString, TypeRegistry, TypedProgram};

/// Load and compile source using a ZynPEG grammar
pub fn load(
    grammar_path: &PathBuf,
    source_path: &PathBuf,
    verbose: bool,
) -> Result<HirModule, Box<dyn std::error::Error>> {
    if verbose {
        println!("{} Grammar file: {:?}", "info:".blue(), grammar_path);
        println!("{} Source file: {:?}", "info:".blue(), source_path);
    }

    // Verify files exist
    if !grammar_path.exists() {
        return Err(format!("Grammar file not found: {:?}", grammar_path).into());
    }
    if !source_path.exists() {
        return Err(format!("Source file not found: {:?}", source_path).into());
    }

    // Read source code
    let source_code = std::fs::read_to_string(source_path)?;
    if verbose {
        println!("{} Read {} bytes of source code", "info:".blue(), source_code.len());
    }

    // Read grammar
    let grammar_code = std::fs::read_to_string(grammar_path)?;
    if verbose {
        println!("{} Read {} bytes of grammar", "info:".blue(), grammar_code.len());
    }

    // Get the grammar name from the file
    let grammar_name = grammar_path
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("unknown");

    // Step 1: Compile .zyn grammar to zpeg module
    let zpeg_module = compile_grammar(&grammar_code, grammar_name, verbose)?;

    // Step 2: Parse source code using zpeg runtime
    let typed_ast_json = parse_with_zpeg(&zpeg_module, &source_code, verbose)?;

    // Step 3: Deserialize TypedAST JSON to TypedProgram
    if verbose {
        // Pretty print the JSON for debugging
        if let Ok(parsed) = serde_json::from_str::<serde_json::Value>(&typed_ast_json) {
            println!("{} TypedAST:\n{}", "debug:".yellow(), serde_json::to_string_pretty(&parsed).unwrap_or_default());
        }
    }

    let typed_program: TypedProgram = serde_json::from_str(&typed_ast_json)
        .map_err(|e| format!("Failed to deserialize TypedAST: {}", e))?;

    if verbose {
        println!(
            "{} Parsed to TypedProgram with {} declarations",
            "info:".blue(),
            typed_program.declarations.len()
        );
    }

    // Step 4: Lower TypedProgram to HIR
    let hir_module = lower_to_hir(typed_program, verbose)?;

    if verbose {
        println!(
            "{} Lowered to HIR with {} functions",
            "info:".blue(),
            hir_module.functions.len()
        );
    }

    Ok(hir_module)
}

/// Compile .zyn grammar to zpeg module
fn compile_grammar(
    grammar_code: &str,
    grammar_name: &str,
    verbose: bool,
) -> Result<zyn_peg::runtime::ZpegModule, Box<dyn std::error::Error>> {
    use pest::Parser;
    use zyn_peg::{ZynGrammarParser, Rule as ZynRule};
    use zyn_peg::ast::build_grammar;
    use zyn_peg::runtime::ZpegCompiler;

    if verbose {
        println!("{} Parsing .zyn grammar with ZynPEG...", "info:".blue());
    }

    // Parse the .zyn grammar file
    let pairs = ZynGrammarParser::parse(ZynRule::program, grammar_code)
        .map_err(|e| format!("Failed to parse .zyn grammar: {}", e))?;

    let grammar = build_grammar(pairs)
        .map_err(|e| format!("Failed to build grammar: {}", e))?;

    if verbose {
        let name = if grammar.language.name.is_empty() {
            grammar_name.to_string()
        } else {
            grammar.language.name.clone()
        };
        println!("{} Grammar '{}' parsed successfully", "info:".blue(), name);
        println!("{} Found {} rules", "info:".blue(), grammar.rules.len());
    }

    // Compile to zpeg module
    let zpeg_module = ZpegCompiler::compile(&grammar)
        .map_err(|e| format!("Failed to compile grammar to zpeg: {}", e))?;

    if verbose {
        println!("{} Compiled to zpeg module", "info:".blue());
        println!("{} Pest grammar: {} bytes", "info:".blue(), zpeg_module.pest_grammar.len());
        println!("{} Rule commands: {} rules", "info:".blue(), zpeg_module.rules.len());
    }

    Ok(zpeg_module)
}

/// Parse source code using zpeg module (via pest_vm)
fn parse_with_zpeg(
    zpeg_module: &zyn_peg::runtime::ZpegModule,
    source_code: &str,
    verbose: bool,
) -> Result<String, Box<dyn std::error::Error>> {
    use pest::iterators::{Pair, Pairs};
    use pest_meta::parser;
    use pest_meta::optimizer;
    use pest_vm::Vm;
    use zyn_peg::runtime::{TypedAstBuilder, AstHostFunctions, CommandInterpreter, RuntimeValue};

    if verbose {
        println!("{} Parsing source with pest_vm...", "info:".blue());
    }

    // Parse the pest grammar to get the grammar AST
    let pest_grammar = &zpeg_module.pest_grammar;
    let pairs = parser::parse(parser::Rule::grammar_rules, pest_grammar)
        .map_err(|e| format!("Failed to parse pest grammar: {:?}", e))?;

    // Convert to AST and optimize
    let ast = parser::consume_rules(pairs)
        .map_err(|e| format!("Failed to consume grammar rules: {:?}", e))?;
    let optimized = optimizer::optimize(ast);

    // Create VM and parse source
    let vm = Vm::new(optimized);
    let parse_result: Pairs<'_, &str> = vm.parse("program", source_code)
        .map_err(|e| format!("Failed to parse source: {}", e))?;

    if verbose {
        println!("{} Source parsed successfully", "info:".blue());
    }

    // Create interpreter with TypedAstBuilder host
    let builder = TypedAstBuilder::new();
    let mut interpreter = CommandInterpreter::new(zpeg_module, builder);

    // Walk the parse tree and execute commands
    let result = walk_parse_tree(&mut interpreter, parse_result, verbose)?;

    // Finalize the AST
    let json = match result {
        RuntimeValue::Node(handle) => {
            interpreter.host_mut().finalize_program(handle)
        }
        _ => {
            // Create empty program if we got something unexpected
            let handle = interpreter.host_mut().create_program();
            interpreter.host_mut().finalize_program(handle)
        }
    };

    if verbose {
        println!("{} Generated TypedAST JSON ({} bytes)", "info:".blue(), json.len());
    }

    Ok(json)
}

/// Recursively walk the pest parse tree and execute zpeg commands
fn walk_parse_tree<'a, H: zyn_peg::runtime::AstHostFunctions>(
    interpreter: &mut zyn_peg::runtime::CommandInterpreter<'_, H>,
    pairs: pest::iterators::Pairs<'a, &'a str>,
    verbose: bool,
) -> Result<zyn_peg::runtime::RuntimeValue, Box<dyn std::error::Error>> {
    use zyn_peg::runtime::RuntimeValue;

    let mut results = Vec::new();

    for pair in pairs {
        let rule_name = pair.as_rule().to_string();
        let text = pair.as_str().to_string();
        let span_start = pair.as_span().start();
        let span_end = pair.as_span().end();

        if verbose {
            println!(
                "  {} Processing rule '{}' at {}..{}: {:?}",
                "trace:".cyan(),
                rule_name,
                span_start,
                span_end,
                if text.len() > 40 { format!("{}...", &text[..40]) } else { text.clone() }
            );
        }

        // Recursively process children first
        let children: Vec<RuntimeValue> = pair.into_inner()
            .map(|child| walk_pair_to_value(child, interpreter, verbose))
            .collect();

        if verbose && !children.is_empty() {
            println!("    {} children: {}", "trace:".cyan(), children.len());
        }

        // Execute commands for this rule
        let result = interpreter.execute_rule(&rule_name, &text, children)
            .map_err(|e| format!("Error executing rule '{}': {}", rule_name, e))?;

        if verbose {
            println!("    {} result: {:?}", "trace:".cyan(), result);
        }

        results.push(result);
    }

    // Return the last result (typically the program node)
    Ok(results.into_iter().last().unwrap_or(RuntimeValue::Null))
}

/// Recursively walk a single pair and return its RuntimeValue
fn walk_pair_to_value<'a, H: zyn_peg::runtime::AstHostFunctions>(
    pair: pest::iterators::Pair<'a, &'a str>,
    interpreter: &mut zyn_peg::runtime::CommandInterpreter<'_, H>,
    verbose: bool,
) -> zyn_peg::runtime::RuntimeValue {
    use zyn_peg::runtime::RuntimeValue;

    let rule_name = pair.as_rule().to_string();
    let text = pair.as_str().to_string();

    // Always trace for debugging
    log::trace!("[walk_pair] rule='{}', text='{}'", rule_name, text.chars().take(50).collect::<String>());

    if verbose {
        println!("      {} walk_pair '{}': {:?}", "trace:".cyan(), rule_name,
            if text.len() > 30 { format!("{}...", &text[..30]) } else { text.clone() });
    }

    // Recursively process children
    let children: Vec<RuntimeValue> = pair.into_inner()
        .map(|c| walk_pair_to_value(c, interpreter, verbose))
        .collect();

    // Execute commands for this rule
    interpreter.execute_rule(&rule_name, &text, children)
        .unwrap_or(RuntimeValue::Null)
}

/// Lower TypedProgram to HIR module
fn lower_to_hir(
    program: TypedProgram,
    verbose: bool,
) -> Result<HirModule, Box<dyn std::error::Error>> {
    let arena = AstArena::new();
    let module_name = InternedString::new_global("main");
    let type_registry = Arc::new(TypeRegistry::new());

    let mut lowering_ctx = LoweringContext::new(
        module_name,
        type_registry.clone(),
        Arc::new(Mutex::new(arena)),
        LoweringConfig::default(),
    );

    // Skip type checking (parser already produced typed AST)
    std::env::set_var("SKIP_TYPE_CHECK", "1");

    let mut hir_module = lowering_ctx
        .lower_program(&program)
        .map_err(|e| format!("Lowering error: {:?}", e))?;

    if verbose {
        println!("{} Lowering complete", "info:".blue());
    }

    // Monomorphization
    zyntax_compiler::monomorphize_module(&mut hir_module)
        .map_err(|e| format!("Monomorphization error: {:?}", e))?;

    if verbose {
        println!("{} Monomorphization complete", "info:".blue());
    }

    Ok(hir_module)
}
