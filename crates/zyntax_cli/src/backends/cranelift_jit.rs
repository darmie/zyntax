//! Cranelift JIT backend compilation

use colored::Colorize;
use zyntax_compiler::cranelift_backend::CraneliftBackend;
use zyntax_compiler::hir::HirModule;

/// Compile HIR module with Cranelift JIT backend
pub fn compile_jit(
    module: HirModule,
    _opt_level: u8,
    run: bool,
    verbose: bool,
) -> Result<(), Box<dyn std::error::Error>> {
    // Create plugin registry and register all available runtime plugins
    let mut registry = zyntax_compiler::plugin::PluginRegistry::new();

    // Register standard library plugin (generic I/O functions)
    registry.register(zyntax_runtime::get_plugin())
        .map_err(|e| format!("Failed to register stdlib plugin: {}", e))?;

    // Register Haxe plugin (frontend-specific runtime)
    registry.register(haxe_zyntax_runtime::get_plugin())
        .map_err(|e| format!("Failed to register Haxe plugin: {}", e))?;

    if verbose {
        println!("{} Registered plugins: {:?}", "info:".blue(), registry.list_plugins());
    }

    // Collect all runtime symbols from registered plugins
    let runtime_symbols = registry.collect_symbols();

    if verbose {
        println!("{} Collected {} runtime symbols", "info:".blue(), runtime_symbols.len());
        for (name, _) in &runtime_symbols {
            println!("  - {}", name);
        }
    }

    let mut backend = CraneliftBackend::with_runtime_symbols(&runtime_symbols)
        .map_err(|e| format!("Failed to initialize backend: {}", e))?;

    if verbose {
        println!("{} Compiling functions...", "info:".blue());
    }

    backend
        .compile_module(&module)
        .map_err(|e| format!("Compilation failed: {}", e))?;

    // Finalize definitions to make function pointers available
    backend
        .finalize_definitions()
        .map_err(|e| format!("Failed to finalize: {}", e))?;

    if run {
        if verbose {
            println!("{} Running main function...", "info:".green().bold());
        }

        execute_main(&backend, &module, verbose)?;
    } else {
        println!("{} Compilation successful", "success:".green().bold());
    }

    Ok(())
}

/// Find and execute the main function
fn execute_main(
    backend: &CraneliftBackend,
    module: &HirModule,
    verbose: bool,
) -> Result<(), Box<dyn std::error::Error>> {
    // Find main function by name
    let (main_id, main_fn) = module
        .functions
        .iter()
        .find(|(_, func)| {
            // Resolve the function name from global interner
            func.name.resolve_global()
                .map(|name| name == "main")
                .unwrap_or(false)
        })
        .ok_or("No 'main' function found in module")?;

    let main_id = *main_id;

    // Get function pointer
    let fn_ptr = backend
        .get_function_ptr(main_id)
        .ok_or("Failed to get main function pointer")?;

    if verbose {
        println!(
            "{} Executing main() at {:?}...",
            "info:".cyan(),
            fn_ptr
        );
    }

    // Determine return type and call function
    let result = unsafe {
        if main_fn.signature.returns.is_empty() {
            // Void return
            let f: fn() = std::mem::transmute(fn_ptr);
            f();
            println!("{} main() completed", "result:".green().bold());
            0
        } else {
            match &main_fn.signature.returns[0] {
                zyntax_compiler::hir::HirType::I32 => {
                    let f: fn() -> i32 = std::mem::transmute(fn_ptr);
                    let ret = f();
                    println!("{} main() returned: {}", "result:".green().bold(), ret);
                    ret as i64
                }
                zyntax_compiler::hir::HirType::I64 => {
                    let f: fn() -> i64 = std::mem::transmute(fn_ptr);
                    let ret = f();
                    println!("{} main() returned: {}", "result:".green().bold(), ret);
                    ret
                }
                zyntax_compiler::hir::HirType::F32 => {
                    let f: fn() -> f32 = std::mem::transmute(fn_ptr);
                    let ret = f();
                    println!("{} main() returned: {}", "result:".green().bold(), ret);
                    0
                }
                zyntax_compiler::hir::HirType::F64 => {
                    let f: fn() -> f64 = std::mem::transmute(fn_ptr);
                    let ret = f();
                    println!("{} main() returned: {}", "result:".green().bold(), ret);
                    0
                }
                zyntax_compiler::hir::HirType::Void => {
                    let f: fn() = std::mem::transmute(fn_ptr);
                    f();
                    println!("{} main() completed", "result:".green().bold());
                    0
                }
                _ => {
                    return Err(format!(
                        "Unsupported main return type: {:?}",
                        main_fn.signature.returns[0]
                    )
                    .into());
                }
            }
        }
    };

    if verbose {
        println!("{} Execution completed with code: {}", "info:".green(), result);
    }

    Ok(())
}
