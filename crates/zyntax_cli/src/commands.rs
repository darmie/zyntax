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
    let continuation_prompt = format!("{}| ", "...".dimmed());
    let mut verbose_mode = verbose;
    let mut line_number = 1;
    let mut input_buffer = String::new();
    let mut in_multiline = false;

    loop {
        let current_prompt = if in_multiline { &continuation_prompt } else { &prompt };
        let readline = rl.readline(current_prompt);
        match readline {
            Ok(line) => {
                let trimmed = line.trim();

                // Handle empty input in single-line mode
                if trimmed.is_empty() && !in_multiline {
                    continue;
                }

                // Handle REPL commands (only in single-line mode)
                if !in_multiline && trimmed.starts_with(':') {
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
                        ":{" => {
                            // Start explicit multi-line input
                            in_multiline = true;
                            input_buffer.clear();
                            continue;
                        }
                        _ => {
                            println!("{} Unknown command: {}", "error:".red(), trimmed);
                            println!("Type :help for available commands");
                            continue;
                        }
                    }
                }

                // Handle multi-line input
                if in_multiline {
                    // Check for end of multi-line block
                    if trimmed == ":}" || trimmed == "}" && is_block_complete(&input_buffer) {
                        in_multiline = false;
                        let input = std::mem::take(&mut input_buffer);

                        // Add complete input to history
                        let _ = rl.add_history_entry(&input);

                        // Compile and run the multi-line input
                        match eval_input(&zpeg_module, &input, &backend, opt_level, verbose_mode) {
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
                        continue;
                    }

                    // Handle backslash continuation in multi-line mode
                    let line_to_append = if trimmed.ends_with('\\') {
                        // Continue on next line, strip the backslash
                        &trimmed[..trimmed.len()-1]
                    } else {
                        &line
                    };

                    // Append line to buffer
                    if !input_buffer.is_empty() {
                        input_buffer.push('\n');
                    }
                    input_buffer.push_str(line_to_append);

                    // If line ended with backslash, continue collecting
                    if trimmed.ends_with('\\') {
                        continue;
                    }

                    // Check if we should auto-complete (balanced braces)
                    if is_block_complete(&input_buffer) && !trimmed.is_empty() {
                        // Auto-complete if braces are balanced and we're not starting a new block
                        let open_count = input_buffer.chars().filter(|&c| c == '{').count();
                        let close_count = input_buffer.chars().filter(|&c| c == '}').count();

                        if open_count > 0 && open_count == close_count {
                            in_multiline = false;
                            let input = std::mem::take(&mut input_buffer);

                            let _ = rl.add_history_entry(&input);

                            match eval_input(&zpeg_module, &input, &backend, opt_level, verbose_mode) {
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
                    }
                    continue;
                }

                // Check for backslash continuation (line ends with \)
                if trimmed.ends_with('\\') {
                    in_multiline = true;
                    // Remove the trailing backslash
                    input_buffer = trimmed[..trimmed.len()-1].to_string();
                    continue;
                }

                // Check if input starts a multi-line block (has unclosed braces)
                let open_count = trimmed.chars().filter(|&c| c == '{').count();
                let close_count = trimmed.chars().filter(|&c| c == '}').count();

                if open_count > close_count {
                    // Start multi-line mode
                    in_multiline = true;
                    input_buffer = line.clone();
                    continue;
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
                if in_multiline {
                    // Cancel multi-line input
                    println!("^C (cancelled multi-line input)");
                    in_multiline = false;
                    input_buffer.clear();
                } else {
                    println!("^C");
                }
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
    println!("  :{{              Start multi-line input (end with :}})");
    println!();
    println!("{}", "Multi-line Input:".green().bold());
    println!("  - End a line with \\ to continue on the next line");
    println!("  - Lines with unclosed {{ automatically continue");
    println!("  - Use :{{ to start explicit multi-line mode, :}} to execute");
    println!("  - Press Ctrl+C to cancel multi-line input");
    println!();
    println!("Enter expressions to evaluate them.");
    println!("The result will be compiled and executed with JIT.");
}

/// Check if braces are balanced in the input
fn is_block_complete(input: &str) -> bool {
    let mut depth = 0i32;
    let mut in_string = false;
    let mut escape_next = false;

    for ch in input.chars() {
        if escape_next {
            escape_next = false;
            continue;
        }

        match ch {
            '\\' if in_string => escape_next = true,
            '"' => in_string = !in_string,
            '{' if !in_string => depth += 1,
            '}' if !in_string => depth -= 1,
            _ => {}
        }
    }

    depth == 0
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
