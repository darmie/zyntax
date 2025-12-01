# Classification Pipeline DSL

**Priority**: High
**Complexity**: High
**Estimated New Code**: ~3500 LOC

## Overview

A domain-specific language for running classification models (CNN, MLP) on CPU with SIMD acceleration. Targets edge deployment scenarios where GPU is unavailable or power-constrained.

## Use Cases

1. **Image Classification**
   - Product categorization
   - Content moderation
   - Medical imaging triage

2. **Text Classification**
   - Sentiment analysis
   - Spam detection
   - Intent classification

3. **Tabular Classification**
   - Fraud detection
   - Credit scoring
   - Churn prediction

4. **Edge Inference**
   - Mobile/embedded devices
   - Offline-capable applications
   - Privacy-preserving (on-device)

## Syntax Design

```
// classify.clf - Classification DSL

// Load model weights
load model "mobilenet_v3_small.safetensors" as classifier {
    input_shape: [1, 3, 224, 224],
    output_classes: 1000,
    labels: "imagenet_labels.txt"
}

// Preprocessing configuration
preprocess image_prep:
    resize to 224x224
    normalize mean=[0.485, 0.456, 0.406] std=[0.229, 0.224, 0.225]
    to_tensor channels_first

// Define the model architecture
model mobilenet_v3_small:
    // Initial conv
    conv2d 16 kernel=3 stride=2 padding=1 -> batch_norm -> hardswish

    // Inverted residual blocks (simplified)
    block expand=16 out=16 kernel=3 stride=1 se=true -> hardswish
    block expand=72 out=24 kernel=3 stride=2 se=false -> relu
    block expand=88 out=24 kernel=3 stride=1 se=false -> relu
    block expand=96 out=40 kernel=5 stride=2 se=true -> hardswish
    block expand=240 out=40 kernel=5 stride=1 se=true -> hardswish
    block expand=240 out=40 kernel=5 stride=1 se=true -> hardswish
    block expand=120 out=48 kernel=5 stride=1 se=true -> hardswish
    block expand=144 out=48 kernel=5 stride=1 se=true -> hardswish
    block expand=288 out=96 kernel=5 stride=2 se=true -> hardswish
    block expand=576 out=96 kernel=5 stride=1 se=true -> hardswish
    block expand=576 out=96 kernel=5 stride=1 se=true -> hardswish

    // Classifier head
    conv2d 576 kernel=1 -> batch_norm -> hardswish
    adaptive_avg_pool to 1x1
    flatten
    dense 1024 -> hardswish
    dropout 0.2
    dense 1000

    output: softmax

// Classification pipeline
pipeline classify_image:
    input: image path

    // Load and preprocess
    img = load_image input
    tensor = preprocess image_prep img

    // Run inference
    logits = forward classifier tensor

    // Get predictions
    probs = softmax logits
    top5 = topk probs k=5

    output: {
        predictions: top5,
        confidence: top5[0].score
    }

// Batch processing
pipeline classify_batch:
    input: image_dir path

    images = glob input "*.jpg"
    results = []

    for img in images:
        result = run classify_image with img
        append results result

    output: results

// Run single image
run classify_image with "cat.jpg"

// Run batch
run classify_batch with "./test_images/"
```

## Grammar Specification

```
// classify.zyn

@name = "classify"
@version = "1.0"
@file_extensions = [".clf", ".classify"]

@builtins {
    // Tensor operations
    tensor_alloc: "$Tensor$alloc"
    tensor_free: "$Tensor$free"
    tensor_load: "$Tensor$load_safetensors"
    tensor_copy: "$Tensor$copy"
    tensor_reshape: "$Tensor$reshape"

    // Convolution (from zrtl_simd)
    conv2d: "$SIMD$conv2d_f32"
    conv2d_depthwise: "$SIMD$conv2d_depthwise_f32"

    // Pooling
    max_pool2d: "$SIMD$max_pool2d_f32"
    avg_pool2d: "$SIMD$avg_pool2d_f32"
    adaptive_avg_pool: "$SIMD$adaptive_avg_pool2d_f32"

    // Activations
    relu: "$SIMD$relu_f32"
    relu6: "$SIMD$relu6_f32"
    hardswish: "$SIMD$hardswish_f32"
    sigmoid: "$SIMD$sigmoid_f32"
    softmax: "$SIMD$softmax_f32"

    // Normalization
    batch_norm: "$SIMD$batch_norm_f32"
    layer_norm: "$SIMD$layer_norm_f32"

    // Linear
    gemm: "$SIMD$gemm_f32"
    add_bias: "$SIMD$add_f32"

    // Utilities
    argmax: "$SIMD$argmax_f32"
    topk: "$VecSearch$topk"
    dropout: "$SIMD$dropout_f32"

    // Image loading (from zrtl_image)
    image_load: "$Image$load"
    image_resize: "$Image$resize"
    image_normalize: "$Image$normalize"
    image_to_tensor: "$Image$to_tensor"

    // IO
    print: "$IO$println"
    glob_files: "$IO$glob"
}

// Grammar rules
program = statement*

statement = load_stmt
          | preprocess_def
          | model_def
          | pipeline_def
          | run_stmt
          | config_stmt

// Load model weights
load_stmt = "load" "model" STRING "as" IDENT load_config?

load_config = "{" (config_pair ("," config_pair)*)? "}"

// Preprocessing definition
preprocess_def = "preprocess" IDENT ":" NEWLINE INDENT preprocess_steps DEDENT

preprocess_steps = preprocess_step+

preprocess_step = "resize" "to" INTEGER "x" INTEGER
                | "normalize" "mean" "=" array "std" "=" array
                | "to_tensor" tensor_format?
                | "crop" "center" INTEGER "x" INTEGER
                | "flip" "horizontal" FLOAT?

tensor_format = "channels_first" | "channels_last"

// Model architecture definition
model_def = "model" IDENT ":" NEWLINE INDENT layer_chain DEDENT

layer_chain = layer_stmt+

layer_stmt = layer_spec ("->" layer_spec)* NEWLINE

layer_spec = conv_layer
           | pool_layer
           | norm_layer
           | activation
           | dense_layer
           | block_layer
           | utility_layer

conv_layer = "conv2d" INTEGER conv_params?

conv_params = (conv_param)*

conv_param = "kernel" "=" INTEGER
           | "stride" "=" INTEGER
           | "padding" "=" INTEGER
           | "groups" "=" INTEGER
           | "dilation" "=" INTEGER

pool_layer = "max_pool" INTEGER "x" INTEGER pool_params?
           | "avg_pool" INTEGER "x" INTEGER pool_params?
           | "adaptive_avg_pool" "to" INTEGER "x" INTEGER
           | "global_avg_pool"

pool_params = ("stride" "=" INTEGER)?

norm_layer = "batch_norm"
           | "layer_norm"
           | "group_norm" INTEGER

activation = "relu" | "relu6" | "hardswish" | "sigmoid" | "tanh" | "gelu" | "softmax"

dense_layer = "dense" INTEGER

block_layer = "block" block_params

block_params = (block_param)+

block_param = "expand" "=" INTEGER
            | "out" "=" INTEGER
            | "kernel" "=" INTEGER
            | "stride" "=" INTEGER
            | "se" "=" BOOL

utility_layer = "flatten"
              | "dropout" FLOAT
              | "reshape" shape_spec

shape_spec = "[" INTEGER ("," INTEGER)* "]"

// Pipeline definition
pipeline_def = "pipeline" IDENT ":" NEWLINE INDENT pipeline_body DEDENT

pipeline_body = input_decl pipeline_stmt* output_decl

input_decl = "input" ":" type_hint IDENT?

output_decl = "output" ":" expr

pipeline_stmt = assign_stmt
              | for_stmt
              | if_stmt
              | expr_stmt

assign_stmt = IDENT "=" expr

for_stmt = "for" IDENT "in" expr ":" NEWLINE INDENT pipeline_stmt+ DEDENT

if_stmt = "if" expr ":" NEWLINE INDENT pipeline_stmt+ DEDENT ("else" ":" NEWLINE INDENT pipeline_stmt+ DEDENT)?

expr_stmt = expr

// Expressions
expr = primary (binary_op primary)*

primary = literal
        | IDENT
        | call_expr
        | index_expr
        | object_literal
        | array_literal
        | "(" expr ")"

call_expr = IDENT "(" (expr ("," expr)*)? ")"
          | "run" IDENT "with" expr
          | "forward" IDENT expr
          | "preprocess" IDENT expr
          | "load_image" expr
          | "softmax" expr
          | "topk" expr "k" "=" INTEGER
          | "append" IDENT expr
          | "glob" expr STRING

index_expr = IDENT "[" expr "]"

object_literal = "{" (IDENT ":" expr ("," IDENT ":" expr)*)? "}"

array_literal = "[" (expr ("," expr)*)? "]"

binary_op = "+" | "-" | "*" | "/" | "==" | "!=" | "<" | ">" | "<=" | ">=" | "and" | "or"

// Run statement
run_stmt = "run" IDENT "with" expr

// Configuration
config_stmt = "config" "{" config_pair* "}"

config_pair = IDENT ":" literal

// Type hints
type_hint = "image" | "tensor" | "text" | "path" | "float" | "int" | "list"

// Literals
literal = STRING | INTEGER | FLOAT | BOOL | array

array = "[" (literal ("," literal)*)? "]"

// Tokens
IDENT = /[a-zA-Z_][a-zA-Z0-9_]*/
STRING = /"[^"]*"/
INTEGER = /-?[0-9]+/
FLOAT = /-?[0-9]+\.[0-9]+/
BOOL = "true" | "false"
```

## Required New Operations

### Additions to `zrtl_simd`

```rust
// New activation functions

/// ReLU6: min(max(0, x), 6)
#[no_mangle]
pub extern "C" fn relu6_f32(data: *mut f32, len: u64);

/// HardSwish: x * relu6(x + 3) / 6
#[no_mangle]
pub extern "C" fn hardswish_f32(data: *mut f32, len: u64);

/// GELU: x * 0.5 * (1 + tanh(sqrt(2/pi) * (x + 0.044715 * x^3)))
#[no_mangle]
pub extern "C" fn gelu_f32(data: *mut f32, len: u64);

/// Adaptive average pooling to target size
#[no_mangle]
pub extern "C" fn adaptive_avg_pool2d_f32(
    input: *const f32,
    output: *mut f32,
    batch: u64,
    channels: u64,
    in_h: u64,
    in_w: u64,
    out_h: u64,
    out_w: u64
);

/// Depthwise separable convolution
#[no_mangle]
pub extern "C" fn conv2d_depthwise_f32(
    input: *const f32,
    kernel: *const f32,
    output: *mut f32,
    batch: u64,
    channels: u64,
    height: u64,
    width: u64,
    kernel_h: u64,
    kernel_w: u64,
    stride: u64,
    padding: u64
);

/// Add bias to tensor (broadcasting over batch and spatial dims)
#[no_mangle]
pub extern "C" fn add_bias_f32(
    data: *mut f32,
    bias: *const f32,
    batch: u64,
    channels: u64,
    spatial_size: u64
);

/// Squeeze-and-Excitation block
#[no_mangle]
pub extern "C" fn se_block_f32(
    data: *mut f32,
    weights_fc1: *const f32,
    weights_fc2: *const f32,
    batch: u64,
    channels: u64,
    spatial_size: u64,
    reduction: u64
);
```

### Plugin: `zrtl_model` (Model Loading)

```rust
/// Model handle containing weights and metadata
pub struct ModelHandle {
    weights: HashMap<String, TensorHandle>,
    input_shape: Vec<usize>,
    output_shape: Vec<usize>,
    config: ModelConfig,
}

/// Load model from safetensors
#[no_mangle]
pub extern "C" fn model_load_safetensors(
    path: *const u8,
    path_len: u32
) -> *mut ModelHandle;

/// Get weight tensor by name
#[no_mangle]
pub extern "C" fn model_get_weight(
    model: *const ModelHandle,
    name: *const u8,
    name_len: u32
) -> *const TensorHandle;

/// Get model input shape
#[no_mangle]
pub extern "C" fn model_input_shape(
    model: *const ModelHandle,
    dim: u32
) -> u64;

/// Free model
#[no_mangle]
pub extern "C" fn model_free(model: *mut ModelHandle);
```

### Extended `zrtl_image` Plugin

```rust
/// Normalize image tensor with mean and std
#[no_mangle]
pub extern "C" fn image_normalize(
    data: *mut f32,
    height: u64,
    width: u64,
    channels: u64,
    mean: *const f32,    // [3] for RGB
    std: *const f32      // [3] for RGB
);

/// Convert HWC u8 image to CHW f32 tensor
#[no_mangle]
pub extern "C" fn image_to_tensor_chw(
    input: *const u8,     // HWC u8
    output: *mut f32,     // CHW f32
    height: u64,
    width: u64,
    channels: u64
);

/// Center crop image
#[no_mangle]
pub extern "C" fn image_center_crop(
    input: *const u8,
    output: *mut u8,
    in_h: u64,
    in_w: u64,
    out_h: u64,
    out_w: u64,
    channels: u64
);
```

## Dependencies

### Rust Crates

```toml
[dependencies]
# Core
zrtl = { path = "../sdk/zrtl" }
wide = "0.7"

# Model loading
safetensors = "0.4"
memmap2 = "0.9"

# Image processing
image = "0.25"

# Serialization
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"

[dev-dependencies]
criterion = "0.5"
```

## File Structure

```
examples/classify/
├── Cargo.toml
├── classify.zyn                    # Grammar definition
├── src/
│   ├── main.rs                     # CLI entry point
│   ├── model_executor.rs           # Model execution logic
│   └── preprocessing.rs            # Image preprocessing
├── models/
│   ├── mobilenet_v3_small.safetensors
│   ├── resnet18.safetensors
│   └── efficientnet_b0.safetensors
├── samples/
│   ├── image_classify.clf          # Image classification
│   ├── batch_process.clf           # Batch processing
│   └── custom_model.clf            # Custom architecture
├── labels/
│   ├── imagenet_labels.txt
│   └── cifar10_labels.txt
└── test_images/
    ├── cat.jpg
    ├── dog.jpg
    └── car.jpg

plugins/zrtl_model/
├── Cargo.toml
├── src/
│   ├── lib.rs
│   ├── safetensors.rs
│   ├── onnx.rs                     # Optional ONNX support
│   └── weight_layout.rs
```

## Implementation Plan

### Phase 1: Model Loading (Week 1)
- [ ] Implement `zrtl_model` plugin
  - [ ] Safetensors parser
  - [ ] Weight tensor management
  - [ ] Model metadata handling
- [ ] Unit tests for various model formats

### Phase 2: New SIMD Operations (Week 1-2)
- [ ] Add to `zrtl_simd`:
  - [ ] `relu6_f32`
  - [ ] `hardswish_f32`
  - [ ] `gelu_f32`
  - [ ] `adaptive_avg_pool2d_f32`
  - [ ] `conv2d_depthwise_f32`
  - [ ] `add_bias_f32`
- [ ] Benchmark against scalar implementations

### Phase 3: Image Preprocessing (Week 2)
- [ ] Extend `zrtl_image`:
  - [ ] `image_normalize`
  - [ ] `image_to_tensor_chw`
  - [ ] `image_center_crop`
- [ ] Test with standard preprocessing pipelines

### Phase 4: Grammar and Compilation (Week 2-3)
- [ ] Write `classify.zyn` grammar
- [ ] Implement semantic actions:
  - [ ] Model definition → weight graph
  - [ ] Pipeline → execution sequence
  - [ ] Preprocessing → transform chain

### Phase 5: Execution Engine (Week 3-4)
- [ ] Build model executor
  - [ ] Weight binding
  - [ ] Forward pass orchestration
  - [ ] Memory management (workspace allocation)
- [ ] Optimize for common patterns

### Phase 6: CLI and Testing (Week 4)
- [ ] Build CLI tool
- [ ] Create example models
- [ ] Benchmark against PyTorch CPU
- [ ] Documentation

## Performance Targets

| Model | Input Size | Target Latency | PyTorch CPU |
|-------|------------|----------------|-------------|
| MobileNetV3-Small | 224x224 | 15ms | 25ms |
| ResNet-18 | 224x224 | 40ms | 60ms |
| EfficientNet-B0 | 224x224 | 50ms | 80ms |

| Operation | Target | Notes |
|-----------|--------|-------|
| Conv2d 3x3 (64 channels, 56x56) | 2ms | SIMD optimized |
| BatchNorm | 0.1ms | Fused with conv when possible |
| Softmax (1000 classes) | 0.05ms | Already implemented |

## Memory Management

### Workspace Allocation

```
Model Loading:
  - Weights: ~5-20MB (depending on model)
  - Memory-mapped for large models

Inference:
  - Input buffer: batch_size * C * H * W * 4 bytes
  - Intermediate buffers: 2x largest activation size
  - Output buffer: batch_size * num_classes * 4 bytes

Example (MobileNetV3-Small, batch=1):
  - Weights: 5.4MB
  - Input: 224*224*3*4 = 600KB
  - Workspace: ~2MB (reused)
  - Output: 4KB
  - Total: ~8MB
```

## Example Outputs

### Single Image Classification

```bash
$ classify run samples/image_classify.clf --image cat.jpg

Loading model: mobilenet_v3_small.safetensors (5.4MB)
Preprocessing: resize 224x224, normalize ImageNet

Predictions:
  1. [0.847] tabby cat
  2. [0.089] tiger cat
  3. [0.031] Egyptian cat
  4. [0.012] Persian cat
  5. [0.008] Siamese cat

Inference time: 14.2ms
Preprocessing: 3.1ms
Total: 17.3ms
```

### Batch Processing

```bash
$ classify run samples/batch_process.clf --dir ./test_images/

Processing 100 images...
████████████████████████████████████████ 100/100

Results saved to: results.json

Summary:
  - Total time: 1.82s
  - Average per image: 18.2ms
  - Throughput: 55 images/sec
```

## Testing Strategy

### Unit Tests
- Individual layer correctness
- Preprocessing transforms
- Model loading edge cases

### Integration Tests
- End-to-end classification
- Batch processing
- Memory leak detection

### Accuracy Tests
- Compare outputs with PyTorch reference
- Numerical precision (fp32 tolerance)

### Benchmarks
- Layer-by-layer timing
- Memory usage profiling
- Comparison with ONNX Runtime, TFLite

## Future Enhancements

1. **Quantization Support**
   - INT8 weights and activations
   - Dynamic quantization

2. **Model Optimization**
   - Layer fusion (Conv+BN+ReLU)
   - Constant folding
   - Dead code elimination

3. **More Architectures**
   - Vision Transformers
   - ConvNeXt
   - Detection heads (YOLO)

4. **Training Mode**
   - Gradient computation
   - Weight updates
   - Fine-tuning support

5. **Multi-threading**
   - Parallel batch processing
   - Async preprocessing
