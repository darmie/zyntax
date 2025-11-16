//! Backend compilation (JIT and LLVM)

pub mod cranelift_jit;
pub mod llvm_aot;

use std::path::PathBuf;
use zyntax_compiler::hir::HirModule;

pub use cranelift_jit::compile_jit;
pub use llvm_aot::compile_llvm;

/// Backend type
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Backend {
    CraneliftJit,
    LlvmAot,
}

impl Backend {
    pub fn from_str(s: &str) -> Result<Self, Box<dyn std::error::Error>> {
        match s {
            "jit" | "cranelift" => Ok(Backend::CraneliftJit),
            "llvm" | "aot" => Ok(Backend::LlvmAot),
            _ => Err(format!("Unknown backend: {}", s).into()),
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
    }
}
