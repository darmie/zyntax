//! LLVM AOT backend compilation
//!
//! Compiles HIR modules to native executables using LLVM's optimizing compiler.
//! This backend produces highly optimized machine code suitable for production use.

use colored::Colorize;
use log::{debug, error, info};
use std::path::PathBuf;
use zyntax_compiler::hir::HirModule;

/// Compile HIR module with LLVM AOT backend
#[cfg(feature = "llvm-backend")]
pub fn compile_llvm(
    module: HirModule,
    output: Option<PathBuf>,
    opt_level: u8,
    _entry_point: Option<&str>,
    verbose: bool,
) -> Result<(), Box<dyn std::error::Error>> {
    use inkwell::context::Context;
    use inkwell::targets::{
        CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine,
    };
    use inkwell::OptimizationLevel;
    use zyntax_compiler::llvm_backend::LLVMBackend;

    let output_path = output.unwrap_or_else(|| PathBuf::from("a.out"));

    if verbose {
        info!("Initializing LLVM backend...");
    }

    // Initialize LLVM targets
    Target::initialize_native(&InitializationConfig::default())
        .map_err(|e| format!("Failed to initialize LLVM target: {}", e))?;

    // Create LLVM context and backend
    let context = Context::create();
    let mut backend = LLVMBackend::new(&context, "zyntax_aot");

    if verbose {
        info!("Compiling HIR to LLVM IR...");
    }

    // Compile HIR module to LLVM IR
    let llvm_ir = backend
        .compile_module(&module)
        .map_err(|e| format!("Compilation failed: {}", e))?;

    if verbose {
        info!("Generated {} bytes of LLVM IR", llvm_ir.len());
        debug!("LLVM IR:\n{}", llvm_ir);
        // Also write LLVM IR to file for debugging
        let ir_path = output_path.with_extension("ll");
        std::fs::write(&ir_path, &llvm_ir).unwrap_or_else(|e| {
            error!("Failed to write LLVM IR: {}", e);
        });
        info!("Wrote LLVM IR to {:?}", ir_path);
    }

    // Get target triple for the host machine
    let triple = TargetMachine::get_default_triple();
    let target = Target::from_triple(&triple)
        .map_err(|e| format!("Failed to get target: {}", e))?;

    // Map optimization level
    let llvm_opt = match opt_level {
        0 => OptimizationLevel::None,
        1 => OptimizationLevel::Less,
        2 => OptimizationLevel::Default,
        _ => OptimizationLevel::Aggressive,
    };

    if verbose {
        info!(
            "Target: {}, Optimization: {:?}",
            triple.as_str().to_str().unwrap_or("unknown"),
            llvm_opt
        );
    }

    // Create target machine
    let target_machine = target
        .create_target_machine(
            &triple,
            "generic",
            "",
            llvm_opt,
            RelocMode::Default,
            CodeModel::Default,
        )
        .ok_or("Failed to create target machine")?;

    // Write object file
    let obj_path = output_path.with_extension("o");

    if verbose {
        info!("Writing object file to {:?}...", obj_path);
    }

    target_machine
        .write_to_file(backend.module(), FileType::Object, &obj_path)
        .map_err(|e| format!("Failed to write object file: {}", e))?;

    // Link to create executable
    if verbose {
        info!("Linking executable...");
    }

    let status = std::process::Command::new("cc")
        .arg(&obj_path)
        .arg("-o")
        .arg(&output_path)
        .status()
        .map_err(|e| format!("Failed to run linker: {}", e))?;

    if !status.success() {
        return Err(format!("Linker failed with status: {}", status).into());
    }

    // Clean up object file
    let _ = std::fs::remove_file(&obj_path);

    println!(
        "{} Successfully compiled to {}",
        "success:".green().bold(),
        output_path.display()
    );

    Ok(())
}

/// Compile HIR module with LLVM AOT backend (stub when feature not enabled)
#[cfg(not(feature = "llvm-backend"))]
pub fn compile_llvm(
    _module: HirModule,
    output: Option<PathBuf>,
    _opt_level: u8,
    _entry_point: Option<&str>,
    _verbose: bool,
) -> Result<(), Box<dyn std::error::Error>> {
    let output_path = output.unwrap_or_else(|| PathBuf::from("a.out"));

    error!("LLVM backend not enabled");
    error!("Rebuild with: cargo build --release --features llvm-backend");
    debug!("Output would be: {}", output_path.display());

    Err("LLVM backend not enabled. Rebuild with --features llvm-backend".into())
}

/// Compile and run with LLVM JIT backend
#[cfg(feature = "llvm-backend")]
pub fn compile_and_run_llvm(
    module: HirModule,
    opt_level: u8,
    entry_point: Option<&str>,
    verbose: bool,
) -> Result<i64, Box<dyn std::error::Error>> {
    use inkwell::context::Context;
    use inkwell::OptimizationLevel;
    use zyntax_compiler::llvm_jit_backend::LLVMJitBackend;

    if verbose {
        info!("Initializing LLVM JIT backend...");
    }

    // Create plugin registry and register all available runtime plugins
    let mut registry = zyntax_compiler::plugin::PluginRegistry::new();

    // Register standard library plugin (generic I/O functions)
    registry.register(zyntax_runtime::get_plugin())
        .map_err(|e| format!("Failed to register stdlib plugin: {}", e))?;

    // Register Haxe plugin (frontend-specific runtime)
    registry.register(haxe_zyntax_runtime::get_plugin())
        .map_err(|e| format!("Failed to register Haxe plugin: {}", e))?;

    if verbose {
        info!("Registered plugins: {:?}", registry.list_plugins());
    }

    // Collect all runtime symbols from registered plugins
    let runtime_symbols = registry.collect_symbols();

    if verbose {
        info!("Collected {} runtime symbols", runtime_symbols.len());
        for (name, _) in &runtime_symbols {
            debug!("  - {}", name);
        }
    }

    // Create LLVM context
    let context = Context::create();

    // Map optimization level
    let llvm_opt = match opt_level {
        0 => OptimizationLevel::None,
        1 => OptimizationLevel::Less,
        2 => OptimizationLevel::Default,
        _ => OptimizationLevel::Aggressive,
    };

    // Create JIT backend
    let mut backend = LLVMJitBackend::with_opt_level(&context, llvm_opt)
        .map_err(|e| format!("Failed to create JIT backend: {}", e))?;

    // Register runtime symbols with the LLVM JIT backend
    for (name, ptr) in &runtime_symbols {
        backend.register_symbol(*name, *ptr);
    }

    if verbose {
        info!("Compiling module with LLVM JIT...");
    }

    // Compile module
    backend
        .compile_module(&module)
        .map_err(|e| format!("JIT compilation failed: {}", e))?;

    // Find main function
    let (main_id, main_fn) = module
        .functions
        .iter()
        .find(|(_, func)| {
            func.name
                .resolve_global()
                .map(|name| name == "main")
                .unwrap_or(false)
        })
        .ok_or("No 'main' function found in module")?;

    let main_id = *main_id;

    // Get function pointer
    let fn_ptr = backend
        .get_function_pointer(main_id)
        .ok_or("Failed to get main function pointer")?;

    debug!("Got main function pointer: {:p}", fn_ptr);
    debug!("About to execute main()...");

    // Execute main function
    // IMPORTANT: Use extern "C" calling convention to match LLVM's default
    let result = unsafe {
        if main_fn.signature.returns.is_empty() {
            let f: extern "C" fn() = std::mem::transmute(fn_ptr);
            f();
            0
        } else {
            match &main_fn.signature.returns[0] {
                zyntax_compiler::hir::HirType::I32 => {
                    let f: extern "C" fn() -> i32 = std::mem::transmute(fn_ptr);
                    f() as i64
                }
                zyntax_compiler::hir::HirType::I64 => {
                    let f: extern "C" fn() -> i64 = std::mem::transmute(fn_ptr);
                    f()
                }
                zyntax_compiler::hir::HirType::F32 => {
                    let f: extern "C" fn() -> f32 = std::mem::transmute(fn_ptr);
                    f() as i64
                }
                zyntax_compiler::hir::HirType::F64 => {
                    let f: extern "C" fn() -> f64 = std::mem::transmute(fn_ptr);
                    f() as i64
                }
                zyntax_compiler::hir::HirType::Void => {
                    let f: extern "C" fn() = std::mem::transmute(fn_ptr);
                    f();
                    0
                }
                _ => {
                    return Err(format!(
                        "Unsupported return type: {:?}",
                        main_fn.signature.returns[0]
                    )
                    .into());
                }
            }
        }
    };

    println!("{} main() returned: {}", "result:".green().bold(), result);

    Ok(result)
}

/// Compile and run with LLVM JIT backend (stub when feature not enabled)
#[cfg(not(feature = "llvm-backend"))]
pub fn compile_and_run_llvm(
    _module: HirModule,
    _opt_level: u8,
    _entry_point: Option<&str>,
    _verbose: bool,
) -> Result<i64, Box<dyn std::error::Error>> {
    error!("LLVM JIT backend not enabled");
    error!("Rebuild with: cargo build --release --features llvm-backend");

    Err("LLVM backend not enabled. Rebuild with --features llvm-backend".into())
}
