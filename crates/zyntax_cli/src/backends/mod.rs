//! Backend compilation (Cranelift and LLVM)

pub mod cranelift_jit;
pub mod llvm_aot;

use std::path::PathBuf;
use zyntax_compiler::hir::HirModule;

pub use cranelift_jit::compile_jit;
pub use llvm_aot::{compile_llvm, compile_and_run_llvm};

/// Backend type (compiler infrastructure)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Backend {
    /// Cranelift - Fast compilation, good for development
    Cranelift,
    /// LLVM - Maximum optimization, good for production
    Llvm,
}

impl Backend {
    pub fn from_str(s: &str) -> Result<Self, Box<dyn std::error::Error>> {
        match s.to_lowercase().as_str() {
            "cranelift" => Ok(Backend::Cranelift),
            "llvm" => Ok(Backend::Llvm),
            _ => Err(format!("Unknown backend: {}. Use: cranelift, llvm", s).into()),
        }
    }
}

/// Compile HIR module with the specified backend
///
/// - `jit`: If true, JIT compile and run immediately
/// - If false, compile to native executable (AOT)
pub fn compile(
    module: HirModule,
    backend: Backend,
    output: Option<PathBuf>,
    opt_level: u8,
    jit: bool,
    verbose: bool,
) -> Result<(), Box<dyn std::error::Error>> {
    match (backend, jit) {
        // JIT execution
        (Backend::Cranelift, true) => compile_jit(module, opt_level, true, verbose),
        (Backend::Llvm, true) => {
            compile_and_run_llvm(module, opt_level, verbose)?;
            Ok(())
        }
        // AOT compilation
        (Backend::Cranelift, false) => compile_jit(module, opt_level, false, verbose),
        (Backend::Llvm, false) => compile_llvm(module, output, opt_level, verbose),
    }
}

/// Compile and run HIR module for REPL, returning the result value
pub fn compile_and_run_repl(
    module: HirModule,
    backend: &Backend,
    opt_level: u8,
    verbose: bool,
) -> Result<i64, Box<dyn std::error::Error>> {
    match backend {
        Backend::Cranelift => cranelift_jit::compile_and_run_repl(module, opt_level, verbose),
        Backend::Llvm => compile_and_run_llvm(module, opt_level, verbose),
    }
}
