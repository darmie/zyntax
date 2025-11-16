//! HIR bytecode (.zbc) format loading

use colored::Colorize;
use std::fs;
use std::path::PathBuf;
use zyntax_compiler::bytecode::deserialize_module;
use zyntax_compiler::hir::HirModule;

/// Load HIR module from bytecode file(s)
pub fn load(inputs: &[PathBuf], verbose: bool) -> Result<HirModule, Box<dyn std::error::Error>> {
    let bytecode_files = collect_bytecode_files(inputs)?;

    if bytecode_files.len() > 1 {
        return Err(
            "Multiple .zbc files not yet supported. Please provide a single bytecode file.".into(),
        );
    }

    let bytecode_file = &bytecode_files[0];

    if verbose {
        println!(
            "{} Loading bytecode from {}",
            "info:".blue(),
            bytecode_file.display()
        );
    }

    let bytecode = fs::read(bytecode_file)
        .map_err(|e| format!("Failed to read {}: {}", bytecode_file.display(), e))?;

    let module = deserialize_module(&bytecode)
        .map_err(|e| format!("Failed to deserialize bytecode: {}", e))?;

    if verbose {
        println!("{} Loaded HIR module: {}", "info:".green(), module.name);
    }

    Ok(module)
}

fn collect_bytecode_files(inputs: &[PathBuf]) -> Result<Vec<PathBuf>, Box<dyn std::error::Error>> {
    let mut bytecode_files = Vec::new();

    for input in inputs {
        if input.is_file() {
            if input.extension().and_then(|s| s.to_str()) == Some("zbc") {
                bytecode_files.push(input.clone());
            }
        } else if input.is_dir() {
            for entry in walkdir::WalkDir::new(input)
                .into_iter()
                .filter_map(|e| e.ok())
                .filter(|e| e.path().extension().and_then(|s| s.to_str()) == Some("zbc"))
            {
                bytecode_files.push(entry.path().to_path_buf());
            }
        }
    }

    if bytecode_files.is_empty() {
        return Err("No .zbc bytecode files found".into());
    }

    Ok(bytecode_files)
}
