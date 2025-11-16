//! Tool to generate method mapping manifest JSON
//!
//! This binary links against the Haxe runtime and generates a JSON file
//! containing all method mappings for compiler integration.
//!
//! Usage:
//!   cargo run --example generate_manifest > ../method_mappings.json

extern crate haxe_zyntax_runtime;

use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize)]
struct MethodMappingManifest {
    version: String,
    mappings: Vec<Mapping>,
}

#[derive(Debug, Serialize, Deserialize)]
struct Mapping {
    symbol: String,
    haxe_type: String,
    haxe_name: String,
    kind: String, // "method" or "property"
    mutates: bool,
    returns_self: bool,
}

fn main() {
    // Collect all method mappings via inventory
    let mappings: Vec<Mapping> = haxe_zyntax_runtime::get_method_mappings()
        .into_iter()
        .map(|m| Mapping {
            symbol: m.symbol.to_string(),
            haxe_type: m.haxe_type.to_string(),
            haxe_name: m.haxe_name.to_string(),
            kind: if m.is_property { "property".to_string() } else { "method".to_string() },
            mutates: m.mutates,
            returns_self: m.returns_self,
        })
        .collect();

    let manifest = MethodMappingManifest {
        version: "1.0.0".to_string(),
        mappings,
    };

    let json = serde_json::to_string_pretty(&manifest).unwrap();
    println!("{}", json);
}
