//! Command execution logic

use colored::Colorize;
use std::path::PathBuf;
use std::collections::HashMap;

use crate::backends::{self, Backend};
use crate::cli::{ModuleArch, CacheAction, default_cache_dir};
use crate::formats::{self, InputFormat};

use zyntax_typed_ast::{
    ImportContext, ModuleArchitecture,
};

/// Convert CLI ModuleArch to typed_ast ModuleArchitecture
fn to_module_architecture(resolver: ModuleArch, cache_dir: &PathBuf) -> ModuleArchitecture {
    match resolver {
        ModuleArch::Haxe => ModuleArchitecture::haxe(),
        ModuleArch::Java => ModuleArchitecture::java(),
        ModuleArch::Rust => ModuleArchitecture::rust(),
        ModuleArch::Python => ModuleArchitecture::python(),
        ModuleArch::Typescript => ModuleArchitecture::typescript(),
        ModuleArch::Go => {
            let gopath = std::env::var("GOPATH")
                .map(PathBuf::from)
                .unwrap_or_else(|_| {
                    dirs::home_dir()
                        .map(|h| h.join("go"))
                        .unwrap_or_else(|| PathBuf::from("/usr/local/go"))
                });
            ModuleArchitecture::go(gopath)
        }
        ModuleArch::Deno => ModuleArchitecture::deno(cache_dir.clone()),
    }
}

/// Build import context from CLI arguments
fn build_import_context(
    resolver: ModuleArch,
    source_roots: Vec<PathBuf>,
    lib_paths: Vec<PathBuf>,
    import_map: Option<PathBuf>,
    cache_dir: &PathBuf,
    source_file: Option<&PathBuf>,
    verbose: bool,
) -> Result<ImportContext, Box<dyn std::error::Error>> {
    let architecture = to_module_architecture(resolver, cache_dir);

    let mut ctx = ImportContext::new()
        .with_architecture(architecture);

    // Add source roots
    for root in source_roots {
        if verbose {
            println!("{} Source root: {}", "info:".blue(), root.display());
        }
        ctx = ctx.with_source_root(root);
    }

    // Add library paths
    for path in lib_paths {
        if verbose {
            println!("{} Library path: {}", "info:".blue(), path.display());
        }
        ctx = ctx.with_search_path(path);
    }

    // If source file specified, add its directory as a source root
    if let Some(source) = source_file {
        if let Some(parent) = source.parent() {
            if verbose {
                println!("{} Auto-added source root: {}", "info:".blue(), parent.display());
            }
            ctx = ctx.with_source_root(parent.to_path_buf());
        }
    }

    // Load import map if specified (for Deno-style imports)
    if let Some(import_map_path) = import_map {
        if verbose {
            println!("{} Loading import map: {}", "info:".blue(), import_map_path.display());
        }
        let import_map_content = std::fs::read_to_string(&import_map_path)?;
        let map: HashMap<String, String> = serde_json::from_str(&import_map_content)?;
        for (alias, target) in map {
            ctx = ctx.with_alias(alias, vec![target]);
        }
    }

    Ok(ctx)
}

/// Execute the compile command
#[allow(clippy::too_many_arguments)]
pub fn compile(
    inputs: Vec<PathBuf>,
    source: Option<PathBuf>,
    grammar: Option<PathBuf>,
    output: Option<PathBuf>,
    backend_str: String,
    opt_level: u8,
    format_str: String,
    jit: bool,
    entry_point: Option<String>,
    resolver: ModuleArch,
    source_roots: Vec<PathBuf>,
    lib_paths: Vec<PathBuf>,
    import_map: Option<PathBuf>,
    cache_dir: Option<PathBuf>,
    no_cache: bool,
    verbose: bool,
) -> Result<(), Box<dyn std::error::Error>> {
    // Determine cache directory
    let cache_dir = cache_dir.unwrap_or_else(default_cache_dir);

    // Build import context
    let import_context = build_import_context(
        resolver,
        source_roots,
        lib_paths,
        import_map,
        &cache_dir,
        source.as_ref(),
        verbose,
    )?;

    if verbose {
        println!("{} Module resolver architecture: {:?}", "info:".blue(), resolver);
        if let Some(ref entry) = entry_point {
            println!("{} Entry point: {}", "info:".blue(), entry);
        }
        if !no_cache {
            println!("{} Cache directory: {}", "info:".blue(), cache_dir.display());
        } else {
            println!("{} Caching disabled", "info:".blue());
        }
    }

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

    // Get module architecture for entry point resolution
    let module_arch = to_module_architecture(resolver, &cache_dir);

    // Compile with selected backend
    backends::compile(hir_module, backend, output, opt_level, jit, entry_point, &module_arch, verbose)
}

/// Display version information
pub fn version() -> Result<(), Box<dyn std::error::Error>> {
    println!("Zyntax Compiler v{}", env!("CARGO_PKG_VERSION"));
    println!();
    println!("{}", "Backends:".green().bold());
    println!("  Cranelift JIT - Fast compilation for development");
    println!("  LLVM AOT      - Optimized compilation for production");
    println!();
    println!("{}", "Formats:".green().bold());
    println!("  TypedAST JSON   - Language-agnostic IR from frontends");
    println!("  HIR Bytecode    - Pre-compiled intermediate representation");
    println!("  ZynPEG Grammar  - Grammar-based parsing for custom languages");
    println!();
    println!("{}", "Module Resolver Architectures:".green().bold());
    println!("  haxe       - Java/Haxe style (com.example.Class)");
    println!("  java       - Java packages");
    println!("  rust       - Rust style (mod.rs)");
    println!("  python     - Python style (__init__.py)");
    println!("  typescript - TypeScript/Node (index.ts)");
    println!("  go         - Go style (domain-based imports)");
    println!("  deno       - Deno style (URL imports)");
    Ok(())
}

/// Execute cache management commands
pub fn cache(action: &CacheAction, verbose: bool) -> Result<(), Box<dyn std::error::Error>> {
    match action {
        CacheAction::Clear { cache_dir } => {
            let dir = cache_dir.clone().unwrap_or_else(default_cache_dir);
            cache_clear(&dir, verbose)
        }
        CacheAction::Stats { cache_dir } => {
            let dir = cache_dir.clone().unwrap_or_else(default_cache_dir);
            cache_stats(&dir, verbose)
        }
        CacheAction::List { cache_dir, verbose: list_verbose } => {
            let dir = cache_dir.clone().unwrap_or_else(default_cache_dir);
            cache_list(&dir, *list_verbose || verbose)
        }
    }
}

/// Clear the compilation cache
fn cache_clear(cache_dir: &PathBuf, verbose: bool) -> Result<(), Box<dyn std::error::Error>> {
    if !cache_dir.exists() {
        println!("{} Cache directory does not exist: {}", "info:".blue(), cache_dir.display());
        return Ok(());
    }

    if verbose {
        println!("{} Clearing cache: {}", "info:".blue(), cache_dir.display());
    }

    let mut files_removed = 0;
    let mut bytes_freed = 0u64;

    for entry in std::fs::read_dir(cache_dir)? {
        let entry = entry?;
        let path = entry.path();
        if path.is_file() {
            let metadata = std::fs::metadata(&path)?;
            bytes_freed += metadata.len();
            std::fs::remove_file(&path)?;
            files_removed += 1;
            if verbose {
                println!("  Removed: {}", path.display());
            }
        } else if path.is_dir() {
            let dir_size = dir_size(&path)?;
            bytes_freed += dir_size;
            std::fs::remove_dir_all(&path)?;
            files_removed += 1;
            if verbose {
                println!("  Removed: {}/", path.display());
            }
        }
    }

    println!(
        "{} Cache cleared: {} items, {} freed",
        "✓".green(),
        files_removed,
        format_bytes(bytes_freed)
    );

    Ok(())
}

/// Show cache statistics
fn cache_stats(cache_dir: &PathBuf, _verbose: bool) -> Result<(), Box<dyn std::error::Error>> {
    println!("{}", "Cache Statistics".green().bold());
    println!("Location: {}", cache_dir.display());
    println!();

    if !cache_dir.exists() {
        println!("Cache directory does not exist.");
        return Ok(());
    }

    let mut total_size = 0u64;
    let mut file_count = 0;
    let mut zbc_count = 0;
    let mut json_count = 0;
    let mut other_count = 0;

    for entry in walkdir::WalkDir::new(cache_dir)
        .into_iter()
        .filter_map(|e| e.ok())
    {
        if entry.file_type().is_file() {
            let metadata = entry.metadata()?;
            total_size += metadata.len();
            file_count += 1;

            if let Some(ext) = entry.path().extension() {
                match ext.to_str() {
                    Some("zbc") => zbc_count += 1,
                    Some("json") => json_count += 1,
                    _ => other_count += 1,
                }
            } else {
                other_count += 1;
            }
        }
    }

    println!("Total size:   {}", format_bytes(total_size));
    println!("Total files:  {}", file_count);
    println!();
    println!("{}", "By type:".dimmed());
    println!("  ZBC (bytecode):  {}", zbc_count);
    println!("  JSON (TypedAST): {}", json_count);
    println!("  Other:           {}", other_count);

    Ok(())
}

/// List cached modules
fn cache_list(cache_dir: &PathBuf, verbose: bool) -> Result<(), Box<dyn std::error::Error>> {
    println!("{}", "Cached Modules".green().bold());
    println!("Location: {}", cache_dir.display());
    println!();

    if !cache_dir.exists() {
        println!("Cache directory does not exist.");
        return Ok(());
    }

    let mut entries: Vec<_> = walkdir::WalkDir::new(cache_dir)
        .into_iter()
        .filter_map(|e| e.ok())
        .filter(|e| e.file_type().is_file())
        .collect();

    // Sort by modification time (newest first)
    entries.sort_by(|a, b| {
        let a_time = a.metadata().ok().and_then(|m| m.modified().ok());
        let b_time = b.metadata().ok().and_then(|m| m.modified().ok());
        b_time.cmp(&a_time)
    });

    if entries.is_empty() {
        println!("No cached modules found.");
        return Ok(());
    }

    for entry in entries {
        let path = entry.path();
        let rel_path = path.strip_prefix(cache_dir).unwrap_or(path);

        if verbose {
            let metadata = entry.metadata()?;
            let size = metadata.len();
            let modified = metadata.modified()
                .ok()
                .and_then(|t| t.duration_since(std::time::UNIX_EPOCH).ok())
                .map(|d| {
                    let secs = d.as_secs();
                    let hours_ago = (std::time::SystemTime::now()
                        .duration_since(std::time::UNIX_EPOCH)
                        .unwrap()
                        .as_secs() - secs) / 3600;
                    if hours_ago < 24 {
                        format!("{}h ago", hours_ago)
                    } else {
                        format!("{}d ago", hours_ago / 24)
                    }
                })
                .unwrap_or_else(|| "unknown".to_string());

            println!(
                "  {} {} ({})",
                rel_path.display(),
                format!("[{}]", format_bytes(size)).dimmed(),
                modified.dimmed()
            );
        } else {
            println!("  {}", rel_path.display());
        }
    }

    Ok(())
}

/// Calculate directory size recursively
fn dir_size(path: &PathBuf) -> Result<u64, std::io::Error> {
    let mut size = 0;
    for entry in std::fs::read_dir(path)? {
        let entry = entry?;
        let metadata = entry.metadata()?;
        if metadata.is_file() {
            size += metadata.len();
        } else if metadata.is_dir() {
            size += dir_size(&entry.path())?;
        }
    }
    Ok(size)
}

/// Format bytes to human readable string
fn format_bytes(bytes: u64) -> String {
    const KB: u64 = 1024;
    const MB: u64 = KB * 1024;
    const GB: u64 = MB * 1024;

    if bytes >= GB {
        format!("{:.2} GB", bytes as f64 / GB as f64)
    } else if bytes >= MB {
        format!("{:.2} MB", bytes as f64 / MB as f64)
    } else if bytes >= KB {
        format!("{:.2} KB", bytes as f64 / KB as f64)
    } else {
        format!("{} B", bytes)
    }
}

/// Start an interactive REPL with a ZynPEG grammar
#[allow(clippy::too_many_arguments)]
pub fn repl(
    grammar_path: PathBuf,
    backend_str: String,
    opt_level: u8,
    resolver: ModuleArch,
    source_roots: Vec<PathBuf>,
    lib_paths: Vec<PathBuf>,
    verbose: bool,
) -> Result<(), Box<dyn std::error::Error>> {
    use rustyline::error::ReadlineError;
    use rustyline::DefaultEditor;

    // Verify grammar file exists
    if !grammar_path.exists() {
        return Err(format!("Grammar file not found: {:?}", grammar_path).into());
    }

    // Build import context for REPL
    let cache_dir = default_cache_dir();
    let _import_context = build_import_context(
        resolver,
        source_roots,
        lib_paths,
        None,
        &cache_dir,
        Some(&grammar_path),
        verbose,
    )?;

    // Read and compile grammar once at startup
    let grammar_code = std::fs::read_to_string(&grammar_path)?;
    let grammar_name = grammar_path
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("unknown");

    println!("{}", "Zyntax REPL".green().bold());
    println!("Grammar: {}", grammar_path.display());
    println!("Module resolver: {:?}", resolver);
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

    eprintln!("[walk_pair] rule='{}', text='{}'", rule_name, text.chars().take(50).collect::<String>());

    let children: Vec<RuntimeValue> = pair.into_inner()
        .map(|c| walk_pair_to_value_repl(c, interpreter))
        .collect();

    interpreter.execute_rule(&rule_name, &text, children)
        .unwrap_or(RuntimeValue::Null)
}
