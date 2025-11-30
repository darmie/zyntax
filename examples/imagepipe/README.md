# ImagePipe DSL Example

A complete example demonstrating how to build a Domain-Specific Language (DSL) for image processing using Zyntax.

## What is ImagePipe?

ImagePipe is a declarative language for image processing pipelines. Instead of writing procedural code, you describe *what* transformations to apply:

```
load "photo.jpg" as img

resize img to 1920x1080
brighten img by 15
contrast img by 1
blur img by 0

save img as "enhanced.png"
```

## Project Structure

```
imagepipe/
├── Cargo.toml          # Rust project configuration
├── imagepipe.zyn       # ZynPEG grammar file
├── src/
│   └── main.rs         # CLI runner application
├── samples/
│   ├── basic.imgpipe       # Basic enhancement
│   ├── thumbnail.imgpipe   # Thumbnail generation
│   ├── social_media.imgpipe # Social media exports
│   ├── vintage.imgpipe     # Vintage photo effect
│   └── transform.imgpipe   # Geometric transformations
└── README.md
```

## Building

```bash
# From the zyntax root directory
cargo build -p imagepipe-example --release

# Or from this directory
cargo build --release
```

## Usage

### Run a Pipeline

```bash
./target/release/imagepipe run samples/basic.imgpipe --verbose
```

### Parse and Display AST

```bash
# Tree view (default)
./target/release/imagepipe parse samples/basic.imgpipe

# JSON output
./target/release/imagepipe parse samples/basic.imgpipe --format json
```

### Interactive REPL

```bash
./target/release/imagepipe repl
```

In the REPL:
```
imgpipe> load "photo.jpg" as img
Loaded image as 'img'
[img] imgpipe> resize img to 800x600
Operation applied
[img] imgpipe> brighten img by 20
Operation applied
[img] imgpipe> save img as "result.png"
Image saved
[img] imgpipe> exit
```

## DSL Reference

### Loading Images

```
load "path/to/image.jpg" as variablename
```

Supported formats: PNG, JPEG, GIF, WebP, BMP, ICO, TIFF

### Saving Images

```
save variablename as "output.png"
```

The format is auto-detected from the file extension.

### Available Operations

| Operation | Syntax | Description |
|-----------|--------|-------------|
| Resize | `resize VAR to WxH` | Resize to exact dimensions |
| Crop | `crop VAR from X1,Y1 to X2,Y2` | Crop region |
| Rotate | `rotate VAR 90\|180\|270` | Rotate clockwise |
| Flip | `flip VAR horizontal\|vertical` | Flip the image |
| Blur | `blur VAR by SIGMA` | Gaussian blur |
| Brightness | `brighten VAR by +/-N` | Adjust brightness (-255 to +255) |
| Contrast | `contrast VAR by N` | Adjust contrast (factor) |
| Grayscale | `grayscale VAR` | Convert to grayscale |
| Invert | `invert VAR` | Invert colors |

### Comments

```
// This is a comment
load "image.jpg" as img  // Inline comment
```

## Required Plugins

ImagePipe uses these ZRTL plugins:

| Plugin | Purpose |
|--------|---------|
| `zrtl_image` | Image loading, saving, and transformations |
| `zrtl_io` | Console output |

Build plugins before running:

```bash
cd plugins
./build_zrtl.sh
```

## How It Works

1. **Grammar**: `imagepipe.zyn` defines the DSL syntax using ZynPEG
2. **Parsing**: Zyntax parses source code into TypedAST
3. **Compilation**: TypedAST is lowered to HIR and compiled to native code
4. **Execution**: Native code calls ZRTL plugin functions

## Extending ImagePipe

### Add a New Operation

1. Add the grammar rule in `imagepipe.zyn`:

```zyn
// sharpen varname by 1.5
// $1 = identifier, $2 = number (amount)
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

2. Add to the `@builtin` section:

```zyn
image_sharpen: "$Image$sharpen",
```

3. Update `statement` to include `sharpen_stmt`

### Add a Custom Plugin

See [Chapter 14: Runtime Plugins](../../book/14-runtime-plugins.md) for creating ZRTL plugins.

## Learn More

- [Chapter 16: Image Pipeline DSL Tutorial](../../book/16-image-pipeline-dsl.md)
- [Chapter 15: Building DSLs](../../book/15-building-dsls.md)
- [Chapter 14: Runtime Plugins](../../book/14-runtime-plugins.md)
- [Chapter 12: Embedding SDK](../../book/12-embedding-sdk.md)
