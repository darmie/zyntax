//! Build script to generate method mapping manifest for compiler integration
//!
//! This script runs after the runtime is built and generates a JSON file
//! containing all method mappings that the reflaxe.zyntax compiler can use
//! to automatically map Haxe methods to runtime calls.

use std::env;
use std::fs;
use std::path::PathBuf;

fn main() {
    println!("cargo:rerun-if-changed=src/");

    // The method mappings will be available at runtime via inventory
    // We'll generate a stub file that the compiler can link against

    let out_dir = PathBuf::from(env::var("OUT_DIR").unwrap());

    // Write a marker file indicating method mappings are available
    fs::write(
        out_dir.join("method_mappings_available.txt"),
        "Method mappings available via inventory::iter::<MethodMapping>()"
    ).expect("Failed to write marker file");

    // In a post-build step, we could emit the mappings, but for now
    // the compiler will call get_method_mappings() at runtime
}
