//! CLI argument parsing and command definitions

use clap::{Parser, Subcommand};
use std::path::PathBuf;

#[derive(Parser)]
#[command(name = "zyntax")]
#[command(about = "Zyntax compiler - Compile typed AST to native code", long_about = None)]
pub struct Cli {
    #[command(subcommand)]
    pub command: Commands,

    /// Verbose output
    #[arg(short, long, global = true)]
    pub verbose: bool,
}

#[derive(Subcommand)]
pub enum Commands {
    /// Compile source files to native code
    Compile {
        /// Input file(s) or directory (.json for TypedAST, .zbc for HIR bytecode)
        /// For grammar-based compilation, use --source and --grammar instead
        #[arg(value_name = "INPUT")]
        input: Vec<PathBuf>,

        /// Source code file to compile (used with --grammar for ZynPEG-based parsing)
        #[arg(short, long, value_name = "SOURCE")]
        source: Option<PathBuf>,

        /// ZynPEG grammar file (.zyn) defining the language parser
        #[arg(short, long, value_name = "GRAMMAR")]
        grammar: Option<PathBuf>,

        /// Output file path
        #[arg(short, long, value_name = "OUTPUT")]
        output: Option<PathBuf>,

        /// Backend to use (jit, llvm)
        #[arg(short, long, default_value = "jit")]
        backend: String,

        /// Optimization level (0-3)
        #[arg(short = 'O', long, default_value = "2")]
        opt_level: u8,

        /// Input format (auto, typed-ast, hir-bytecode, zyn)
        /// - auto: Auto-detect from file extension
        /// - typed-ast: TypedAST JSON files
        /// - hir-bytecode: HIR bytecode (.zbc) files
        /// - zyn: ZynPEG grammar-based parsing (requires --grammar and --source)
        #[arg(short = 'f', long, default_value = "auto")]
        format: String,

        /// Run the compiled program immediately (JIT only)
        #[arg(long)]
        run: bool,
    },

    /// Start an interactive REPL with a ZynPEG grammar
    Repl {
        /// ZynPEG grammar file (.zyn) defining the language parser
        #[arg(short, long, value_name = "GRAMMAR")]
        grammar: PathBuf,

        /// Backend to use (jit only for REPL)
        #[arg(short, long, default_value = "jit")]
        backend: String,

        /// Optimization level (0-3)
        #[arg(short = 'O', long, default_value = "0")]
        opt_level: u8,
    },

    /// Display version information
    Version,
}

impl Commands {
    pub fn compile_args(&self) -> Option<CompileArgs> {
        match self {
            Commands::Compile {
                input,
                source,
                grammar,
                output,
                backend,
                opt_level,
                format,
                run,
            } => Some(CompileArgs {
                input: input.clone(),
                source: source.clone(),
                grammar: grammar.clone(),
                output: output.clone(),
                backend: backend.clone(),
                opt_level: *opt_level,
                format: format.clone(),
                run: *run,
            }),
            _ => None,
        }
    }

    pub fn repl_args(&self) -> Option<ReplArgs> {
        match self {
            Commands::Repl {
                grammar,
                backend,
                opt_level,
            } => Some(ReplArgs {
                grammar: grammar.clone(),
                backend: backend.clone(),
                opt_level: *opt_level,
            }),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct CompileArgs {
    pub input: Vec<PathBuf>,
    pub source: Option<PathBuf>,
    pub grammar: Option<PathBuf>,
    pub output: Option<PathBuf>,
    pub backend: String,
    pub opt_level: u8,
    pub format: String,
    pub run: bool,
}

#[derive(Debug, Clone)]
pub struct ReplArgs {
    pub grammar: PathBuf,
    pub backend: String,
    pub opt_level: u8,
}
