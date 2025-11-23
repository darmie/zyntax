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
