//! LLVM AOT backend compilation

use colored::Colorize;
use std::path::PathBuf;
use zyntax_compiler::hir::HirModule;

/// Compile HIR module with LLVM AOT backend
pub fn compile_llvm(
    _module: HirModule,
    output: Option<PathBuf>,
    _opt_level: u8,
    _verbose: bool,
) -> Result<(), Box<dyn std::error::Error>> {
    let output_path = output.unwrap_or_else(|| PathBuf::from("a.out"));

    // TODO: Implement LLVM backend compilation
    eprintln!("{} LLVM backend not yet implemented", "error:".red());
    eprintln!("Output would be: {}", output_path.display());

    Err("LLVM backend not implemented".into())
}
