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
        #[arg(value_name = "INPUT")]
        input: Vec<PathBuf>,

        /// Output file path
        #[arg(short, long, value_name = "OUTPUT")]
        output: Option<PathBuf>,

        /// Backend to use (jit, llvm)
        #[arg(short, long, default_value = "jit")]
        backend: String,

        /// Optimization level (0-3)
        #[arg(short = 'O', long, default_value = "2")]
        opt_level: u8,

        /// Input format (auto, typed-ast, hir-bytecode)
        #[arg(short = 'f', long, default_value = "auto")]
        format: String,

        /// Run the compiled program immediately (JIT only)
        #[arg(long)]
        run: bool,
    },

    /// Display version information
    Version,
}

impl Commands {
    pub fn compile_args(&self) -> Option<CompileArgs> {
        match self {
            Commands::Compile {
                input,
                output,
                backend,
                opt_level,
                format,
                run,
            } => Some(CompileArgs {
                input: input.clone(),
                output: output.clone(),
                backend: backend.clone(),
                opt_level: *opt_level,
                format: format.clone(),
                run: *run,
            }),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct CompileArgs {
    pub input: Vec<PathBuf>,
    pub output: Option<PathBuf>,
    pub backend: String,
    pub opt_level: u8,
    pub format: String,
    pub run: bool,
}
