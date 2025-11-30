//! ImagePipe DSL Runner
//!
//! A command-line tool for running ImagePipe programs - a domain-specific
//! language for image processing pipelines.
//!
//! ## Usage
//!
//! ```bash
//! # Run a pipeline script
//! imagepipe run examples/enhance.imgpipe
//!
//! # Parse and show the AST (for debugging)
//! imagepipe parse examples/enhance.imgpipe
//!
//! # Interactive REPL mode
//! imagepipe repl
//! ```
//!
//! ## Plugins Used
//!
//! - `zrtl_image` - Image loading, saving, and manipulation
//! - `zrtl_simd` - SIMD-accelerated pixel operations
//! - `zrtl_paint` - 2D graphics primitives
//! - `zrtl_fs` - File system operations
//! - `zrtl_io` - Console I/O

use anyhow::{Context, Result};
use clap::{Parser, Subcommand};
use std::path::PathBuf;

/// ImagePipe - Image Processing Pipeline DSL
#[derive(Parser)]
#[command(name = "imagepipe")]
#[command(about = "Run image processing pipelines written in ImagePipe DSL")]
#[command(version)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Run an ImagePipe program
    Run {
        /// Path to the .imgpipe file
        file: PathBuf,

        /// Directory containing ZRTL plugins
        #[arg(long, default_value = "plugins/target/zrtl")]
        plugins: PathBuf,

        /// Enable verbose output
        #[arg(short, long)]
        verbose: bool,
    },

    /// Parse an ImagePipe program and show the AST
    Parse {
        /// Path to the .imgpipe file
        file: PathBuf,

        /// Output format: json or tree
        #[arg(long, default_value = "tree")]
        format: String,
    },

    /// Start an interactive REPL
    Repl {
        /// Directory containing ZRTL plugins
        #[arg(long, default_value = "plugins/target/zrtl")]
        plugins: PathBuf,
    },
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Commands::Run { file, plugins, verbose } => {
            run_pipeline(&file, &plugins, verbose)
        }
        Commands::Parse { file, format } => {
            parse_and_display(&file, &format)
        }
        Commands::Repl { plugins } => {
            run_repl(&plugins)
        }
    }
}

/// Run an ImagePipe program
fn run_pipeline(file: &PathBuf, plugins_dir: &PathBuf, verbose: bool) -> Result<()> {
    use zyntax_embed::{ZyntaxRuntime, LanguageGrammar};

    if verbose {
        println!("Loading ImagePipe grammar...");
    }

    // Load the grammar
    let grammar_source = include_str!("../imagepipe.zyn");
    let grammar = LanguageGrammar::compile_zyn(grammar_source)
        .context("Failed to compile ImagePipe grammar")?;

    if verbose {
        println!("Creating runtime...");
    }

    // Create runtime
    let mut runtime = ZyntaxRuntime::new()
        .context("Failed to create Zyntax runtime")?;

    // Load required ZRTL plugins
    let plugins = ["zrtl_image", "zrtl_simd", "zrtl_paint", "zrtl_fs", "zrtl_io"];

    for plugin_name in &plugins {
        let plugin_path = plugins_dir.join(format!("{}.zrtl", plugin_name));
        if plugin_path.exists() {
            if verbose {
                println!("Loading plugin: {}", plugin_name);
            }
            runtime.load_plugin(&plugin_path)
                .with_context(|| format!("Failed to load plugin: {}", plugin_name))?;
        } else if verbose {
            println!("Plugin not found (skipping): {}", plugin_path.display());
        }
    }

    // Register grammar (returns (), not Result)
    runtime.register_grammar("imagepipe", grammar);

    // Read source file
    let source = std::fs::read_to_string(file)
        .with_context(|| format!("Failed to read file: {}", file.display()))?;

    if verbose {
        println!("Compiling pipeline...");
    }

    // Compile the source using the registered grammar
    let functions = runtime.load_module("imagepipe", &source)
        .context("Failed to compile ImagePipe program")?;

    if verbose {
        println!("Compiled functions: {:?}", functions);
        println!("Running pipeline...\n");
    }

    // Execute the main entry point function if it exists
    if functions.contains(&"main".to_string()) {
        runtime.call::<()>("main", &[])
            .context("Pipeline execution failed")?;
    } else if functions.contains(&"run_pipeline".to_string()) {
        runtime.call::<()>("run_pipeline", &[])
            .context("Pipeline execution failed")?;
    } else if !functions.is_empty() {
        // Try to call the first function as the entry point
        let entry = &functions[0];
        if verbose {
            println!("No 'main' or 'run_pipeline' function, calling '{}'", entry);
        }
        runtime.call::<()>(entry, &[])
            .context("Pipeline execution failed")?;
    } else {
        println!("No functions found in the pipeline script");
    }

    if verbose {
        println!("\nPipeline completed successfully!");
    }

    Ok(())
}

/// Parse an ImagePipe program and display the AST
fn parse_and_display(file: &PathBuf, format: &str) -> Result<()> {
    use zyntax_embed::LanguageGrammar;

    // Load the grammar
    let grammar_source = include_str!("../imagepipe.zyn");
    let grammar = LanguageGrammar::compile_zyn(grammar_source)
        .context("Failed to compile ImagePipe grammar")?;

    // Read source file
    let source = std::fs::read_to_string(file)
        .with_context(|| format!("Failed to read file: {}", file.display()))?;

    // Parse to JSON (TypedAST representation)
    let json = grammar.parse_to_json(&source)
        .context("Failed to parse ImagePipe program")?;

    match format {
        "json" => {
            println!("{}", json);
        }
        "tree" | _ => {
            // Pretty print a simplified tree view
            println!("Parsed AST for: {}\n", file.display());
            print_ast_tree(&json)?;
        }
    }

    Ok(())
}

/// Print a simplified tree view of the AST
fn print_ast_tree(json: &str) -> Result<()> {
    // Simple tree visualization from JSON
    // Parse using serde_json (re-exported through zyntax_embed dependencies)
    let parsed: serde_json::Value = serde_json::from_str(json)
        .context("Failed to parse JSON")?;

    fn print_value(value: &serde_json::Value, indent: usize) {
        let prefix = "  ".repeat(indent);
        match value {
            serde_json::Value::Object(map) => {
                for (key, val) in map {
                    if key == "kind" || key == "name" || key == "op" {
                        if let serde_json::Value::String(s) = val {
                            println!("{}{}: {}", prefix, key, s);
                        }
                    } else if key == "children" || key == "body" || key == "statements" {
                        println!("{}{}:", prefix, key);
                        print_value(val, indent + 1);
                    } else if matches!(val, serde_json::Value::Object(_) | serde_json::Value::Array(_)) {
                        println!("{}{}:", prefix, key);
                        print_value(val, indent + 1);
                    }
                }
            }
            serde_json::Value::Array(arr) => {
                for (i, item) in arr.iter().enumerate() {
                    println!("{}[{}]:", prefix, i);
                    print_value(item, indent + 1);
                }
            }
            _ => {}
        }
    }

    print_value(&parsed, 0);
    Ok(())
}

/// Run an interactive REPL
fn run_repl(plugins_dir: &PathBuf) -> Result<()> {
    use std::io::{self, BufRead, Write};
    use zyntax_embed::{ZyntaxRuntime, LanguageGrammar};

    println!("ImagePipe REPL v1.0");
    println!("Type 'help' for available commands, 'exit' to quit\n");

    // Load grammar
    let grammar_source = include_str!("../imagepipe.zyn");
    let grammar = LanguageGrammar::compile_zyn(grammar_source)
        .context("Failed to compile ImagePipe grammar")?;

    // Create runtime with plugins
    let mut runtime = ZyntaxRuntime::new()?;

    let plugins = ["zrtl_image", "zrtl_simd", "zrtl_paint", "zrtl_fs", "zrtl_io"];
    for plugin_name in &plugins {
        let plugin_path = plugins_dir.join(format!("{}.zrtl", plugin_name));
        if plugin_path.exists() {
            runtime.load_plugin(&plugin_path).ok();
        }
    }

    // Register grammar (returns (), not Result)
    runtime.register_grammar("imagepipe", grammar);

    // Track loaded images
    let mut current_image: Option<String> = None;

    let stdin = io::stdin();
    loop {
        // Show prompt with current image
        let prompt = match &current_image {
            Some(img) => format!("[{}] imgpipe> ", img),
            None => "imgpipe> ".to_string(),
        };
        print!("{}", prompt);
        io::stdout().flush()?;

        // Read input
        let mut line = String::new();
        if stdin.lock().read_line(&mut line)? == 0 {
            break; // EOF
        }
        let line = line.trim();

        match line {
            "" => continue,
            "exit" | "quit" => {
                println!("Goodbye!");
                break;
            }
            "help" => {
                print_repl_help();
            }
            "images" => {
                println!("Current image: {:?}", current_image);
            }
            _ if line.starts_with("load ") => {
                // Parse: load "path" as name
                let rest = line.strip_prefix("load ").unwrap();
                let program = format!("{}\n", line);
                match compile_and_run(&mut runtime, &program) {
                    Ok(()) => {
                        // Extract variable name
                        if let Some(pos) = rest.find(" as ") {
                            let name = rest[pos + 4..].trim();
                            current_image = Some(name.to_string());
                            println!("Loaded image as '{}'", name);
                        }
                    }
                    Err(e) => eprintln!("Error: {}", e),
                }
            }
            _ if line.starts_with("save ") => {
                let program = format!("{}\n", line);
                match compile_and_run(&mut runtime, &program) {
                    Ok(()) => println!("Image saved"),
                    Err(e) => eprintln!("Error: {}", e),
                }
            }
            _ if line.starts_with("pipeline ") => {
                // Multi-line pipeline input
                println!("Enter pipeline operations (end with '}}'):");
                let mut program = format!("{}\n", line);
                loop {
                    print!("... ");
                    io::stdout().flush()?;
                    let mut next_line = String::new();
                    stdin.lock().read_line(&mut next_line)?;
                    program.push_str(&next_line);
                    if next_line.contains('}') {
                        break;
                    }
                }
                match compile_and_run(&mut runtime, &program) {
                    Ok(()) => println!("Pipeline applied"),
                    Err(e) => eprintln!("Error: {}", e),
                }
            }
            _ => {
                // Try to execute as a single operation on current image
                if let Some(img_name) = &current_image {
                    let program = format!(
                        "pipeline {} {{\n    {}\n}}\n",
                        img_name, line
                    );
                    match compile_and_run(&mut runtime, &program) {
                        Ok(()) => println!("Operation applied"),
                        Err(e) => eprintln!("Error: {}", e),
                    }
                } else {
                    eprintln!("No image loaded. Use 'load \"path\" as name' first.");
                }
            }
        }
    }

    Ok(())
}

fn compile_and_run(
    runtime: &mut zyntax_embed::ZyntaxRuntime,
    source: &str,
) -> Result<()> {
    let functions = runtime.load_module("imagepipe", source)
        .context("Compilation failed")?;

    // Try to find an entry point to run
    if functions.contains(&"main".to_string()) {
        runtime.call::<()>("main", &[])
            .context("Execution failed")?;
    } else if !functions.is_empty() {
        // For REPL, the grammar should generate inline execution
        // The module loading itself executes the pipeline operations
    }

    Ok(())
}

fn print_repl_help() {
    println!("ImagePipe REPL Commands:");
    println!();
    println!("  load \"path\" as name  - Load an image file");
    println!("  save name as \"path\"  - Save an image to file");
    println!("  pipeline name {{ ... }} - Apply operations to an image");
    println!("  images               - Show loaded images");
    println!("  help                 - Show this help");
    println!("  exit                 - Exit the REPL");
    println!();
    println!("Pipeline Operations:");
    println!();
    println!("  resize WxH           - Resize to exact dimensions");
    println!("  resize fit WxH       - Resize to fit within bounds");
    println!("  crop X,Y to X,Y      - Crop region");
    println!("  rotate 90|180|270    - Rotate image");
    println!("  flip horizontal      - Flip horizontally");
    println!("  flip vertical        - Flip vertically");
    println!("  blur SIGMA           - Gaussian blur");
    println!("  brightness +/-N      - Adjust brightness");
    println!("  contrast N           - Adjust contrast");
    println!("  grayscale            - Convert to grayscale");
    println!("  invert               - Invert colors");
    println!();
    println!("Example:");
    println!("  load \"photo.jpg\" as img");
    println!("  pipeline img {{");
    println!("      resize fit 800x600");
    println!("      brightness +10");
    println!("  }}");
    println!("  save img as \"output.png\"");
}
