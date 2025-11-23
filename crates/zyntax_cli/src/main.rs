//! Zyntax CLI - Command-line interface for the Zyntax compiler
//!
//! Supports multiple input formats:
//! - HIR bytecode (.zbc files) - Direct HIR deserialization
//! - TypedAST JSON (.json files) - Language-agnostic IR from frontends
//! - ZynPEG grammar (.zyn files) - Grammar-based parsing for custom languages
//!
//! Multiple backends:
//! - Cranelift JIT - Fast compilation for development
//! - LLVM AOT - Optimized compilation for production

mod backends;
mod cli;
mod commands;
mod formats;

use clap::Parser;
use colored::Colorize;
use std::process;

use cli::{Cli, Commands};

fn main() {
    env_logger::init();

    let cli = Cli::parse();

    let result = match &cli.command {
        Commands::Compile { .. } => {
            if let Some(args) = cli.command.compile_args() {
                commands::compile(
                    args.input,
                    args.source,
                    args.grammar,
                    args.output,
                    args.backend,
                    args.opt_level,
                    args.format,
                    args.run,
                    cli.verbose,
                )
            } else {
                unreachable!("Compile command should have compile args")
            }
        }

        Commands::Repl { .. } => {
            if let Some(args) = cli.command.repl_args() {
                commands::repl(
                    args.grammar,
                    args.backend,
                    args.opt_level,
                    cli.verbose,
                )
            } else {
                unreachable!("Repl command should have repl args")
            }
        }

        Commands::Version => commands::version(),
    };

    if let Err(e) = result {
        eprintln!("{} {}", "error:".red().bold(), e);
        process::exit(1);
    }
}
