# Tutorial: Building an Image Processing Pipeline DSL

This tutorial walks you through building a complete image processing DSL from scratch. You'll learn how to design domain-specific syntax, write a ZynPEG grammar with semantic actions, leverage ZRTL plugins, and run pipeline programs from both CLI and embedded Rust applications.

> **Working Example**: The complete, compilable example project is available at [`examples/imagepipe/`](../examples/imagepipe/). Build and run it with:
>
> ```bash
> # Build the imagepipe CLI and required plugins
> cargo build -p imagepipe-example --release
> cd plugins && ./build_zrtl.sh && cd ..
>
> # Run a sample pipeline
> ./target/release/imagepipe run examples/imagepipe/samples/basic.imgpipe \
>     --plugins plugins/target/zrtl --verbose
> ```

By the end, you'll have a working DSL that looks like this:

```text
// photo_enhance.imgpipe

load "input.jpg" as photo

resize photo to 800x600
brighten photo by 20
contrast photo by 1.5
grayscale photo

save photo as "output.png"

print "Processing complete!"
```

## What We're Building

Our **ImagePipe** DSL will support:

- **Loading images** from files with format auto-detection
- **Transformation operations**: resize, crop, rotate, flip
- **Filter operations**: blur, brightness, contrast, grayscale, invert
- **Saving images** to various formats (PNG, JPEG, etc.)
- **Console output** for status messages
- **Comments** for documenting pipelines

## Architecture Overview

```text
┌─────────────────────────────────────────────────────────────────┐
│                    ImagePipe DSL Source                         │
│                   (.imgpipe files)                              │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                   imagepipe.zyn Grammar                         │
│         (PEG rules + semantic actions → TypedAST)               │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                    Zyntax Compiler                              │
│               (TypedAST → HIR → Native Code)                    │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                    ZRTL Plugins                                 │
│  ┌──────────────┐  ┌────────────┐                              │
│  │  zrtl_image  │  │  zrtl_io   │                              │
│  │  (load, save │  │  (console  │                              │
│  │   transforms)│  │   output)  │                              │
│  └──────────────┘  └────────────┘                              │
└─────────────────────────────────────────────────────────────────┘
```

**Plugins Used:**

| Plugin | Purpose |
|--------|---------|
| `zrtl_image` | Image loading, saving, resize, crop, rotate, flip, filters |
| `zrtl_io` | Console output for status messages |

## Step 1: Design the DSL Syntax

Before writing grammar rules, sketch out the syntax you want users to write. Good DSL design focuses on:

1. **Readability**: Domain experts should understand it immediately
2. **Minimal boilerplate**: Focus on operations, not syntax overhead
3. **Natural flow**: Operations read like English sentences

### Core Syntax Elements

```text
// Comments start with //

// Load an image and give it a name
load "path/to/image.jpg" as myimage

// Apply operations to the image
resize myimage to 800x600          // Exact dimensions
crop myimage from 0,0 to 400,300   // Crop region
rotate myimage 90                  // Rotate by degrees (90, 180, 270)
flip myimage horizontal            // or "vertical"
blur myimage by 2.5                // Gaussian blur with sigma
brighten myimage by 30             // Adjust brightness (-255 to +255)
contrast myimage by 1.5            // Contrast factor
grayscale myimage                  // Convert to grayscale
invert myimage                     // Invert colors

// Save the result
save myimage as "output.png"

// Print status messages
print "Processing complete!"
```

## Step 2: Write the Grammar

Now let's translate this syntax into a ZynPEG grammar file.

### Grammar Header

```zyn
// imagepipe.zyn - Image Processing Pipeline DSL

@language {
    name: "ImagePipe",
    version: "1.0",
    file_extensions: [".imgpipe", ".ip"],
    entry_point: "run_pipeline"
}

// Map DSL operations to ZRTL plugin symbols
@builtin {
    // Image I/O
    image_load: "$Image$load",
    image_save: "$Image$save",

    // Transformations
    image_resize: "$Image$resize",
    image_crop: "$Image$crop",
    image_rotate90: "$Image$rotate90",
    image_rotate180: "$Image$rotate180",
    image_rotate270: "$Image$rotate270",
    image_flip_h: "$Image$flip_horizontal",
    image_flip_v: "$Image$flip_vertical",

    // Filters
    image_blur: "$Image$blur",
    image_brighten: "$Image$brighten",
    image_contrast: "$Image$contrast",
    image_grayscale: "$Image$grayscale",
    image_invert: "$Image$invert",

    // Console output
    println: "$IO$println",
}
```

The `@language` block defines metadata, and `@builtin` maps DSL function names to ZRTL plugin symbols. When the DSL calls `image_load`, it gets translated to `$Image$load` which is the symbol exported by the `zrtl_image` plugin.

### Program Structure

```zyn
// Entry point: collect statements, then wrap in run_pipeline function
program = { SOI ~ statements ~ EOI }
  -> TypedProgram {
      "get_child": { "index": 0 }
  }

// Collect all statements into a program with a single function
statements = { statement* }
  -> TypedProgram {
      "get_all_children": true,
      "define": "program",
      "args": {
          "declarations": [
              { "define": "function", "args": {
                  "name": "run_pipeline",
                  "params": [],
                  "return_type": "void",
                  "body": { "define": "block", "args": {
                      "statements": "$result"
                  }}
              }}
          ]
      }
  }

// Statements in the DSL
statement = {
    load_stmt |
    save_stmt |
    resize_stmt |
    crop_stmt |
    rotate_stmt |
    flip_stmt |
    blur_stmt |
    brightness_stmt |
    contrast_stmt |
    grayscale_stmt |
    invert_stmt |
    print_stmt
}
  -> TypedStatement {
      "get_child": { "index": 0 }
  }
```

This wraps all statements in a `run_pipeline` function, which serves as the entry point.

### Load and Save Statements

```zyn
// load "image.jpg" as varname
// $1 = string_literal (path), $2 = identifier (variable name as text)
load_stmt = { "load" ~ string_literal ~ "as" ~ identifier }
  -> TypedStatement {
      "commands": [
          { "define": "let_stmt", "args": {
              "name": "$2",
              "type": { "define": "primitive_type", "args": { "name": "u64" } },
              "init": { "define": "call", "args": {
                  "callee": { "define": "variable", "args": { "name": "image_load" } },
                  "args": ["$1"]
              }},
              "is_const": false
          }}
      ]
  }

// save varname as "output.png"
// $1 = identifier (variable name as text), $2 = string_literal (path)
save_stmt = { "save" ~ identifier ~ "as" ~ string_literal }
  -> TypedStatement {
      "commands": [
          { "define": "expression_stmt", "args": {
              "expr": { "define": "call", "args": {
                  "callee": { "define": "variable", "args": { "name": "image_save" } },
                  "args": [
                      { "define": "variable", "args": { "name": "$1" } },
                      "$2"
                  ]
              }}
          }}
      ]
  }
```

Key points:
- `$1`, `$2` refer to child nodes by position (1-based)
- The `identifier` rule returns text directly (via `"get_text": true`)
- We use `"define": "let_stmt"` to create a variable declaration

### Transformation Operations

Each operation follows a pattern: `operation varname [parameters]`

```zyn
// resize varname to 800x600
// $1 = identifier (image), $2 = integer (width), $3 = integer (height)
resize_stmt = { "resize" ~ identifier ~ "to" ~ integer ~ "x" ~ integer }
  -> TypedStatement {
      "commands": [
          { "define": "assignment", "args": {
              "target": { "define": "variable", "args": { "name": "$1" } },
              "value": { "define": "call", "args": {
                  "callee": { "define": "variable", "args": { "name": "image_resize" } },
                  "args": [
                      { "define": "variable", "args": { "name": "$1" } },
                      "$2",
                      "$3"
                  ]
              }}
          }}
      ]
  }
```

This generates: `img = image_resize(img, 800, 600)`

The same pattern applies to other operations:

```zyn
// blur varname by 2.5
blur_stmt = { "blur" ~ identifier ~ "by" ~ number }
  -> TypedStatement {
      "commands": [
          { "define": "assignment", "args": {
              "target": { "define": "variable", "args": { "name": "$1" } },
              "value": { "define": "call", "args": {
                  "callee": { "define": "variable", "args": { "name": "image_blur" } },
                  "args": [
                      { "define": "variable", "args": { "name": "$1" } },
                      "$2"
                  ]
              }}
          }}
      ]
  }

// grayscale varname (no parameters)
grayscale_stmt = { "grayscale" ~ identifier }
  -> TypedStatement {
      "commands": [
          { "define": "assignment", "args": {
              "target": { "define": "variable", "args": { "name": "$1" } },
              "value": { "define": "call", "args": {
                  "callee": { "define": "variable", "args": { "name": "image_grayscale" } },
                  "args": [{ "define": "variable", "args": { "name": "$1" } }]
              }}
          }}
      ]
  }

// print "message"
print_stmt = { "print" ~ string_literal }
  -> TypedStatement {
      "commands": [
          { "define": "expression_stmt", "args": {
              "expr": { "define": "call", "args": {
                  "callee": { "define": "variable", "args": { "name": "println" } },
                  "args": ["$1"]
              }}
          }}
      ]
  }
```

### Terminal Rules

```zyn
// String literal: "hello world"
string_literal = @{ "\"" ~ string_inner* ~ "\"" }
  -> TypedExpression {
      "get_text": true,
      "define": "string_literal",
      "args": { "value": "$result" }
  }

string_inner = { !("\"" | "\\") ~ ANY | "\\" ~ ANY }

// Integer: 123
integer = @{ ASCII_DIGIT+ }
  -> TypedExpression {
      "get_text": true,
      "parse_int": true,
      "define": "int_literal",
      "args": { "value": "$result" }
  }

// Signed integer: +30 or -20 or 15
signed_integer = @{ ("+" | "-")? ~ ASCII_DIGIT+ }
  -> TypedExpression {
      "get_text": true,
      "parse_int": true,
      "define": "int_literal",
      "args": { "value": "$result" }
  }

// Floating point: 1.5 or 0.5
number = @{ "-"? ~ ASCII_DIGIT+ ~ ("." ~ ASCII_DIGIT+)? }
  -> TypedExpression {
      "get_text": true,
      "parse_float": true,
      "define": "float_literal",
      "args": { "value": "$result" }
  }

// Identifier: my_image
identifier = @{ ASCII_ALPHA ~ (ASCII_ALPHANUMERIC | "_")* }
  -> String {
      "get_text": true
  }

// Whitespace and comments (ignored)
WHITESPACE = _{ " " | "\t" | "\n" | "\r" }
COMMENT = _{ "//" ~ (!"\n" ~ ANY)* }
```

## Step 3: Build the CLI Application

Create a Rust application that uses the `zyntax_embed` SDK to run ImagePipe programs.

### Cargo.toml

```toml
[package]
name = "imagepipe-example"
version = "0.1.0"
edition = "2021"

[[bin]]
name = "imagepipe"
path = "src/main.rs"

[dependencies]
zyntax_embed = { path = "../../crates/zyntax_embed" }
clap = { version = "4.4", features = ["derive"] }
anyhow = "1.0"
serde_json = "1.0"
```

### Main Application

```rust
use anyhow::{Context, Result};
use clap::{Parser, Subcommand};
use std::path::PathBuf;

#[derive(Parser)]
#[command(name = "imagepipe")]
#[command(about = "Run image processing pipelines")]
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
    /// Parse and show the AST
    Parse {
        /// Path to the .imgpipe file
        file: PathBuf,

        /// Output format: json or tree
        #[arg(long, default_value = "tree")]
        format: String,
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
    }
}

fn run_pipeline(file: &PathBuf, plugins_dir: &PathBuf, verbose: bool) -> Result<()> {
    use zyntax_embed::{ZyntaxRuntime, LanguageGrammar};

    // Load the grammar (embedded at compile time)
    let grammar_source = include_str!("../imagepipe.zyn");
    let grammar = LanguageGrammar::compile_zyn(grammar_source)
        .context("Failed to compile ImagePipe grammar")?;

    // Create runtime
    let mut runtime = ZyntaxRuntime::new()?;

    // Load ZRTL plugins
    let plugins = ["zrtl_image", "zrtl_io"];
    for plugin_name in &plugins {
        let path = plugins_dir.join(format!("{}.zrtl", plugin_name));
        if path.exists() {
            if verbose {
                println!("Loading plugin: {}", plugin_name);
            }
            runtime.load_plugin(&path)?;
        }
    }

    // Register grammar
    runtime.register_grammar("imagepipe", grammar);

    // Read and compile source
    let source = std::fs::read_to_string(file)?;
    let functions = runtime.load_module("imagepipe", &source)?;

    if verbose {
        println!("Compiled functions: {:?}", functions);
        println!("Running pipeline...\n");
    }

    // Execute entry point
    if functions.contains(&"run_pipeline".to_string()) {
        runtime.call::<()>("run_pipeline", &[])?;
    }

    Ok(())
}
```

## Step 4: Sample Programs

### basic.imgpipe

```text
// Basic image enhancement pipeline

load "input.jpg" as photo

// Resize for web
resize photo to 1200x800

// Enhance the image
brighten photo by 15
contrast photo by 1

// Light blur for noise reduction
blur photo by 0

// Save the result
save photo as "output.png"

print "Basic enhancement complete!"
```

### thumbnail.imgpipe

```text
// Generate thumbnails

load "original.png" as img
resize img to 64x64
grayscale img
save img as "icon.png"

load "original.png" as thumb
resize thumb to 150x150
save thumb as "thumbnail.png"

print "Thumbnails generated!"
```

### transform.imgpipe

```text
// Geometric transformations

load "landscape.jpg" as img
rotate img 90
save img as "rotated.png"

load "landscape.jpg" as img2
flip img2 horizontal
save img2 as "flipped.png"

load "landscape.jpg" as img3
crop img3 from 100,100 to 500,400
save img3 as "cropped.png"

print "Transformations complete!"
```

## Running the Example

```bash
# Build everything (from repository root)
cargo build -p imagepipe-example --release

# Build the ZRTL plugins
cd plugins && ./build_zrtl.sh && cd ..

# Parse a program (shows AST - no plugins required)
./target/release/imagepipe parse examples/imagepipe/samples/basic.imgpipe

# Run a pipeline (requires plugins and an actual image file)
./target/release/imagepipe run examples/imagepipe/samples/basic.imgpipe \
    --plugins plugins/target/zrtl --verbose
```

### Using Your Own Images

Create a simple test file:

```text
// my_test.imgpipe
load "/path/to/your/image.jpg" as img
resize img to 400x300
brighten img by 10
save img as "/path/to/output.png"
print "Done!"
```

Then run:

```bash
./target/release/imagepipe run my_test.imgpipe --plugins plugins/target/zrtl
```

## Extending the DSL

### Adding a New Operation

To add a `sharpen` operation:

1. **Add the builtin mapping** in `@builtin`:

```zyn
image_sharpen: "$Image$sharpen",
```

2. **Add the grammar rule**:

```zyn
// sharpen varname by 1.5
sharpen_stmt = { "sharpen" ~ identifier ~ "by" ~ number }
  -> TypedStatement {
      "commands": [
          { "define": "assignment", "args": {
              "target": { "define": "variable", "args": { "name": "$1" } },
              "value": { "define": "call", "args": {
                  "callee": { "define": "variable", "args": { "name": "image_sharpen" } },
                  "args": [
                      { "define": "variable", "args": { "name": "$1" } },
                      "$2"
                  ]
              }}
          }}
      ]
  }
```

3. **Add to the statement alternatives**:

```zyn
statement = {
    load_stmt |
    save_stmt |
    resize_stmt |
    sharpen_stmt |  // Added
    ...
}
```

4. **Implement in the plugin** (`zrtl_image`):

```rust
#[no_mangle]
pub extern "C" fn image_sharpen(handle: u64, sigma: f32) -> u64 {
    // Implementation using image crate
}
```

## Key Concepts Learned

1. **DSL Design**: Syntax should be readable and domain-focused
2. **Semantic Actions**: JSON command blocks transform parse trees into TypedAST
3. **Builtin Mapping**: `@builtin` connects DSL names to plugin symbols
4. **Statement Pattern**: `operation varname [params]` generates assignments
5. **Plugin Loading**: Plugins are loaded at runtime and symbols are registered with the JIT
6. **Embedding**: `zyntax_embed` provides a simple API for running DSL programs

## Troubleshooting

### "can't resolve symbol $Image$load"

This means the plugin wasn't loaded. Make sure:
1. The plugins are built: `cd plugins && ./build_zrtl.sh`
2. The `--plugins` path points to the correct directory
3. The plugin file exists (e.g., `plugins/target/zrtl/zrtl_image.zrtl`)

### Image not loading

Check that:
1. The image path is correct (use absolute paths for testing)
2. The image format is supported (JPEG, PNG, GIF, BMP, TIFF, WebP)

### Exit code 132 (SIGILL)

This is a known issue with the function terminator. The pipeline still executes correctly - you can ignore this for now.

## Next Steps

- See [Chapter 15: Building DSLs](./15-building-dsls.md) for more DSL patterns
- See [Chapter 14: Runtime Plugins](./14-runtime-plugins.md) for creating ZRTL plugins
- See [Chapter 12: Embedding SDK](./12-embedding-sdk.md) for embedding in applications
