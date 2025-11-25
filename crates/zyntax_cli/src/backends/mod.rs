//! Backend compilation (JIT and LLVM)

pub mod cranelift_jit;
pub mod llvm_aot;

use std::path::PathBuf;
use zyntax_compiler::hir::HirModule;

pub use cranelift_jit::compile_jit;
pub use llvm_aot::{compile_llvm, compile_and_run_llvm};

/// Backend type
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Backend {
    CraneliftJit,
    LlvmAot,
    LlvmJit,
}

impl Backend {
    pub fn from_str(s: &str) -> Result<Self, Box<dyn std::error::Error>> {
        match s {
            "jit" | "cranelift" => Ok(Backend::CraneliftJit),
            "llvm" | "aot" | "llvm-aot" => Ok(Backend::LlvmAot),
            "llvm-jit" => Ok(Backend::LlvmJit),
            _ => Err(format!("Unknown backend: {}. Use: jit, cranelift, llvm, llvm-aot, llvm-jit", s).into()),
        }
    }
}

/// Compile HIR module with the specified backend
pub fn compile(
    module: HirModule,
    backend: Backend,
    output: Option<PathBuf>,
    opt_level: u8,
    run: bool,
    verbose: bool,
) -> Result<(), Box<dyn std::error::Error>> {
    match backend {
        Backend::CraneliftJit => compile_jit(module, opt_level, run, verbose),
        Backend::LlvmAot => compile_llvm(module, output, opt_level, verbose),
        Backend::LlvmJit => {
            if run {
                compile_and_run_llvm(module, opt_level, verbose)?;
                Ok(())
            } else {
                Err("LLVM JIT backend requires --run flag".into())
            }
        }
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
        Backend::CraneliftJit => cranelift_jit::compile_and_run_repl(module, opt_level, verbose),
        Backend::LlvmAot => Err("LLVM AOT backend does not support REPL mode. Use --backend llvm-jit".into()),
        Backend::LlvmJit => compile_and_run_llvm(module, opt_level, verbose),
    }
}
