//! Command execution logic

use colored::Colorize;
use std::path::PathBuf;

use crate::backends::{self, Backend};
use crate::formats::{self, InputFormat};

/// Execute the compile command
pub fn compile(
    inputs: Vec<PathBuf>,
    source: Option<PathBuf>,
    grammar: Option<PathBuf>,
    output: Option<PathBuf>,
    backend_str: String,
    opt_level: u8,
    format_str: String,
    run: bool,
    verbose: bool,
) -> Result<(), Box<dyn std::error::Error>> {
    // Detect input format
    let input_format = formats::detect_format(
        &format_str,
        &inputs,
        grammar.as_ref(),
        source.as_ref(),
    )?;

    if verbose {
        println!("{} Input format: {:?}", "info:".blue(), input_format);
    }

    // Load HIR module based on input format
    let hir_module = match input_format {
        InputFormat::HirBytecode => {
            if inputs.is_empty() {
                return Err("No input files specified".into());
            }
            formats::hir_bytecode::load(&inputs, verbose)?
        }
        InputFormat::TypedAst => {
            if inputs.is_empty() {
                return Err("No input files specified".into());
            }
            formats::typed_ast_json::load(&inputs, verbose)?
        }
        InputFormat::ZynGrammar => {
            let grammar_path = grammar.ok_or("--grammar is required for zyn format")?;
            let source_path = source.ok_or("--source is required for zyn format")?;
            formats::zyn_grammar::load(&grammar_path, &source_path, verbose)?
        }
    };

    // Parse backend
    let backend = Backend::from_str(&backend_str)?;

    if verbose {
        println!(
            "{} Compiling with {:?} backend (opt level {})...",
            "info:".blue(),
            backend,
            opt_level
        );
    }

    // Compile with selected backend
    backends::compile(hir_module, backend, output, opt_level, run, verbose)
}

/// Display version information
pub fn version() -> Result<(), Box<dyn std::error::Error>> {
    println!("Zyntax Compiler v{}", env!("CARGO_PKG_VERSION"));
    println!("Backends: Cranelift JIT, LLVM AOT");
    println!("Formats: TypedAST JSON, HIR Bytecode, ZynPEG Grammar");
    Ok(())
}

/// Start an interactive REPL with a ZynPEG grammar
pub fn repl(
    grammar_path: PathBuf,
    backend_str: String,
    opt_level: u8,
    verbose: bool,
) -> Result<(), Box<dyn std::error::Error>> {
    use rustyline::error::ReadlineError;
    use rustyline::DefaultEditor;

    // Verify grammar file exists
    if !grammar_path.exists() {
        return Err(format!("Grammar file not found: {:?}", grammar_path).into());
    }

    // Read and compile grammar once at startup
    let grammar_code = std::fs::read_to_string(&grammar_path)?;
    let grammar_name = grammar_path
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("unknown");

    println!("{}", "Zyntax REPL".green().bold());
    println!("Grammar: {}", grammar_path.display());
    println!("Type expressions to evaluate. Commands: :help, :quit, :verbose");
    println!();

    // Compile grammar
    let zpeg_module = compile_grammar_for_repl(&grammar_code, grammar_name, verbose)?;

    let lang_name = if zpeg_module.metadata.name.is_empty() {
        grammar_name.to_string()
    } else {
        zpeg_module.metadata.name.clone()
    };

    println!("{} {} grammar loaded ({} rules)",
        "✓".green(),
        lang_name,
        zpeg_module.rules.len()
    );
    println!();

    // Parse backend
    let backend = Backend::from_str(&backend_str)?;

    // Create readline editor
    let mut rl = DefaultEditor::new()?;
    let history_file = dirs::home_dir()
        .map(|h| h.join(".zyntax_repl_history"))
        .unwrap_or_else(|| PathBuf::from(".zyntax_repl_history"));
    let _ = rl.load_history(&history_file);

    let prompt = format!("{}> ", lang_name.cyan());
    let mut verbose_mode = verbose;
    let mut line_number = 1;

    loop {
        let readline = rl.readline(&prompt);
        match readline {
            Ok(line) => {
                let trimmed = line.trim();

                // Handle empty input
                if trimmed.is_empty() {
                    continue;
                }

                // Handle REPL commands
                if trimmed.starts_with(':') {
                    match trimmed {
                        ":quit" | ":q" | ":exit" => {
                            println!("Goodbye!");
                            break;
                        }
                        ":help" | ":h" | ":?" => {
                            print_repl_help();
                            continue;
                        }
                        ":verbose" | ":v" => {
                            verbose_mode = !verbose_mode;
                            println!("Verbose mode: {}", if verbose_mode { "on" } else { "off" });
                            continue;
                        }
                        ":clear" | ":c" => {
                            // Clear screen (ANSI escape)
                            print!("\x1B[2J\x1B[1;1H");
                            continue;
                        }
                        _ => {
                            println!("{} Unknown command: {}", "error:".red(), trimmed);
                            println!("Type :help for available commands");
                            continue;
                        }
                    }
                }

                // Add to history
                let _ = rl.add_history_entry(&line);

                // Compile and run the input
                match eval_input(&zpeg_module, trimmed, &backend, opt_level, verbose_mode) {
                    Ok(result) => {
                        if let Some(value) = result {
                            println!("{} = {}", format!("[{}]", line_number).dimmed(), value.to_string().yellow());
                        }
                        line_number += 1;
                    }
                    Err(e) => {
                        println!("{} {}", "error:".red(), e);
                    }
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("^C");
                continue;
            }
            Err(ReadlineError::Eof) => {
                println!("Goodbye!");
                break;
            }
            Err(err) => {
                println!("{} {:?}", "error:".red(), err);
                break;
            }
        }
    }

    // Save history
    let _ = rl.save_history(&history_file);

    Ok(())
}

fn print_repl_help() {
    println!("{}", "REPL Commands:".green().bold());
    println!("  :help, :h, :?    Show this help message");
    println!("  :quit, :q, :exit Exit the REPL");
    println!("  :verbose, :v     Toggle verbose mode");
    println!("  :clear, :c       Clear the screen");
    println!();
    println!("Enter expressions to evaluate them.");
    println!("The result will be compiled and executed with JIT.");
}

/// Compile grammar for REPL use
fn compile_grammar_for_repl(
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
        println!("{} Found {} rules", "info:".blue(), grammar.rules.len());
    }

    // Compile to zpeg module
    let zpeg_module = ZpegCompiler::compile(&grammar)
        .map_err(|e| format!("Failed to compile grammar to zpeg: {}", e))?;

    Ok(zpeg_module)
}

/// Evaluate input in the REPL
fn eval_input(
    zpeg_module: &zyn_peg::runtime::ZpegModule,
    input: &str,
    backend: &Backend,
    opt_level: u8,
    verbose: bool,
) -> Result<Option<i64>, Box<dyn std::error::Error>> {
    use std::sync::{Arc, Mutex};
    use pest::iterators::Pairs;
    use pest_meta::parser;
    use pest_meta::optimizer;
    use pest_vm::Vm;
    use zyn_peg::runtime::{TypedAstBuilder, AstHostFunctions, CommandInterpreter, RuntimeValue};
    use zyntax_compiler::hir::HirModule;
    use zyntax_compiler::lowering::{AstLowering, LoweringConfig, LoweringContext};
    use zyntax_typed_ast::{AstArena, InternedString, TypeRegistry, TypedProgram};

    // Parse the pest grammar
    let pest_grammar = &zpeg_module.pest_grammar;
    let pairs = parser::parse(parser::Rule::grammar_rules, pest_grammar)
        .map_err(|e| format!("Failed to parse pest grammar: {:?}", e))?;

    let ast = parser::consume_rules(pairs)
        .map_err(|e| format!("Failed to consume grammar rules: {:?}", e))?;
    let optimized = optimizer::optimize(ast);

    // Create VM and parse input
    let vm = Vm::new(optimized);
    let parse_result: Pairs<'_, &str> = vm.parse("program", input)
        .map_err(|e| format!("Parse error: {}", e))?;

    if verbose {
        println!("{} Input parsed successfully", "info:".blue());
    }

    // Create interpreter with TypedAstBuilder host
    let builder = TypedAstBuilder::new();
    let mut interpreter = CommandInterpreter::new(zpeg_module, builder);

    // Walk the parse tree and execute commands
    let result = walk_parse_tree_repl(&mut interpreter, parse_result)?;

    // Finalize the AST
    let json = match result {
        RuntimeValue::Node(handle) => {
            interpreter.host_mut().finalize_program(handle)
        }
        _ => {
            let handle = interpreter.host_mut().create_program();
            interpreter.host_mut().finalize_program(handle)
        }
    };

    if verbose {
        println!("{} Generated TypedAST JSON ({} bytes)", "info:".blue(), json.len());
    }

    // Deserialize to TypedProgram
    let typed_program: TypedProgram = serde_json::from_str(&json)
        .map_err(|e| format!("Failed to deserialize TypedAST: {}", e))?;

    // Lower to HIR
    let arena = AstArena::new();
    let module_name = InternedString::new_global("repl");
    let type_registry = Arc::new(TypeRegistry::new());

    let mut lowering_ctx = LoweringContext::new(
        module_name,
        type_registry.clone(),
        Arc::new(Mutex::new(arena)),
        LoweringConfig::default(),
    );

    std::env::set_var("SKIP_TYPE_CHECK", "1");

    let mut hir_module = lowering_ctx
        .lower_program(&typed_program)
        .map_err(|e| format!("Lowering error: {:?}", e))?;

    // Monomorphization
    zyntax_compiler::monomorphize_module(&mut hir_module)
        .map_err(|e| format!("Monomorphization error: {:?}", e))?;

    if verbose {
        println!("{} HIR module ready ({} functions)", "info:".blue(), hir_module.functions.len());
    }

    // Compile and run
    let result = backends::compile_and_run_repl(hir_module, backend, opt_level, verbose)?;

    Ok(Some(result))
}

/// Walk parse tree for REPL evaluation
fn walk_parse_tree_repl<H: zyn_peg::runtime::AstHostFunctions>(
    interpreter: &mut zyn_peg::runtime::CommandInterpreter<'_, H>,
    pairs: pest::iterators::Pairs<'_, &str>,
) -> Result<zyn_peg::runtime::RuntimeValue, Box<dyn std::error::Error>> {
    use zyn_peg::runtime::RuntimeValue;

    let mut results = Vec::new();

    for pair in pairs {
        let rule_name = pair.as_rule().to_string();
        let text = pair.as_str().to_string();

        // Recursively process children first
        let children: Vec<RuntimeValue> = pair.into_inner()
            .map(|child| walk_pair_to_value_repl(child, interpreter))
            .collect();

        // Execute commands for this rule
        let result = interpreter.execute_rule(&rule_name, &text, children)
            .map_err(|e| format!("Error executing rule '{}': {}", rule_name, e))?;

        results.push(result);
    }

    Ok(results.into_iter().last().unwrap_or(RuntimeValue::Null))
}

/// Walk a single pair for REPL evaluation
fn walk_pair_to_value_repl<H: zyn_peg::runtime::AstHostFunctions>(
    pair: pest::iterators::Pair<'_, &str>,
    interpreter: &mut zyn_peg::runtime::CommandInterpreter<'_, H>,
) -> zyn_peg::runtime::RuntimeValue {
    use zyn_peg::runtime::RuntimeValue;

    let rule_name = pair.as_rule().to_string();
    let text = pair.as_str().to_string();

    let children: Vec<RuntimeValue> = pair.into_inner()
        .map(|c| walk_pair_to_value_repl(c, interpreter))
        .collect();

    interpreter.execute_rule(&rule_name, &text, children)
        .unwrap_or(RuntimeValue::Null)
}
