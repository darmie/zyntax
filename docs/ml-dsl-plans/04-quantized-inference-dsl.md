# Quantized Inference DSL

**Priority**: Medium
**Complexity**: High
**Estimated New Code**: ~4000 LOC

## Overview

A domain-specific language for running quantized (INT8/INT4) neural network inference. Provides significant memory and compute savings for edge deployment while maintaining acceptable accuracy.

## Use Cases

1. **Edge Deployment**
   - Mobile devices with limited memory
   - Embedded systems (ARM Cortex-M)
   - Battery-powered IoT

2. **High-Throughput Inference**
   - Batch processing on CPU
   - Cost-sensitive cloud inference
   - Multi-model serving

3. **Real-Time Applications**
   - Video analytics
   - Speech recognition
   - Robotics control

4. **Memory-Constrained Environments**
   - Microcontrollers
   - FPGA soft processors
   - Shared hosting

## Syntax Design

```
// quant.qinf - Quantized Inference DSL

// Load quantized model
load quantized_model "yolov8n_int8.qmodel" as detector {
    input: tensor[1, 3, 640, 640] float32,
    output: tensor[1, 84, 8400] float32,
    quantization: {
        scheme: symmetric,
        bits: 8,
        granularity: per_channel
    }
}

// Calibration configuration (for post-training quantization)
calibration calibrate_detector {
    model: "yolov8n.safetensors",
    dataset: "calibration_images/",
    samples: 100,
    method: entropy,  // or minmax, percentile
    output: "yolov8n_int8.qmodel"
}

// Define quantization-aware preprocessing
preprocess quant_prep:
    input: image

    // Standard preprocessing
    resize to 640x640 letterbox
    normalize range=[0, 1]

    // Quantization happens automatically based on model input spec

    output: tensor[1, 3, 640, 640]

// Inference pipeline with mixed precision
pipeline detect_objects:
    input: image path

    // Load and preprocess
    img = load_image input
    tensor = preprocess quant_prep img

    // Quantize input (fp32 -> int8)
    q_input = quantize tensor scale=0.00392 zero_point=0

    // Run quantized inference
    // Internally: int8 weights, int8 activations, int32 accumulator
    q_output = forward detector q_input

    // Dequantize output (int8 -> fp32 for post-processing)
    output = dequantize q_output

    // Post-processing (in fp32)
    boxes = decode_boxes output anchors=detector.anchors
    detections = nms boxes threshold=0.5 iou=0.45

    output: detections

// Mixed-precision pipeline (some layers in fp32)
pipeline mixed_precision_classify:
    input: image path

    img = load_image input
    tensor = preprocess standard_prep img

    // First few layers in fp32 (sensitive to quantization)
    x = conv2d_fp32 tensor weights=model.conv1
    x = batch_norm_fp32 x params=model.bn1
    x = relu x

    // Quantize for middle layers
    q_x = quantize x scale=0.125 zero_point=64

    // Quantized backbone (int8)
    q_x = conv2d_int8 q_x weights=model.conv2_q
    q_x = relu_int8 q_x
    q_x = max_pool_int8 q_x kernel=2

    // Continue quantized...
    q_x = conv2d_int8 q_x weights=model.conv3_q
    q_x = relu_int8 q_x

    // Dequantize for classifier head
    x = dequantize q_x

    // FP32 classifier (for accuracy)
    x = flatten x
    x = dense_fp32 x weights=model.fc1
    x = relu x
    logits = dense_fp32 x weights=model.fc2

    probs = softmax logits
    output: argmax probs

// Batch inference with dynamic batching
pipeline batch_detect:
    input: images list[path]
    config: {
        max_batch: 8,
        dynamic_batch: true,
        timeout_ms: 100
    }

    results = []
    batch = []

    for img_path in images:
        img = load_image img_path
        tensor = preprocess quant_prep img
        append batch tensor

        if len(batch) >= config.max_batch:
            batch_tensor = stack batch
            q_batch = quantize batch_tensor
            q_output = forward detector q_batch
            output = dequantize q_output

            for i in range(len(batch)):
                detections = decode_boxes output[i]
                append results detections

            clear batch

    // Process remaining
    if len(batch) > 0:
        // ... same as above

    output: results

// Run inference
run detect_objects with "test.jpg"

// Run calibration (offline)
run calibration calibrate_detector
```

## Grammar Specification

```
// quant.zyn

@name = "quant"
@version = "1.0"
@file_extensions = [".qinf", ".quant"]

@builtins {
    // Quantization operations (new: zrtl_quant)
    quantize_f32_i8: "$Quant$quantize_symmetric_i8"
    quantize_f32_u8: "$Quant$quantize_asymmetric_u8"
    dequantize_i8_f32: "$Quant$dequantize_i8"
    dequantize_u8_f32: "$Quant$dequantize_u8"

    // Quantized compute (new: zrtl_quant)
    gemm_i8: "$Quant$gemm_i8_i32"
    conv2d_i8: "$Quant$conv2d_i8"
    relu_i8: "$Quant$relu_i8"
    max_pool_i8: "$Quant$max_pool2d_i8"
    add_i8: "$Quant$add_i8"

    // FP32 operations (from zrtl_simd)
    gemm_f32: "$SIMD$gemm_f32"
    conv2d_f32: "$SIMD$conv2d_f32"
    relu_f32: "$SIMD$relu_f32"
    softmax_f32: "$SIMD$softmax_f32"
    batch_norm_f32: "$SIMD$batch_norm_f32"

    // Scale computation
    compute_scale: "$Quant$compute_scale"
    compute_zero_point: "$Quant$compute_zero_point"

    // Model loading
    model_load_quantized: "$Model$load_quantized"
    model_get_scale: "$Model$get_layer_scale"
    model_get_zero_point: "$Model$get_layer_zero_point"

    // Calibration
    calibrate_minmax: "$Calibrate$minmax"
    calibrate_entropy: "$Calibrate$entropy"
    calibrate_percentile: "$Calibrate$percentile"

    // Utilities
    argmax: "$SIMD$argmax_f32"
    topk: "$VecSearch$topk"
    nms: "$Detection$nms"
    decode_boxes: "$Detection$decode_yolo"

    // Image
    image_load: "$Image$load"
    image_resize: "$Image$resize"
    image_normalize: "$Image$normalize"

    // IO
    print: "$IO$println"
}

// Grammar rules
program = statement*

statement = load_stmt
          | calibration_def
          | preprocess_def
          | pipeline_def
          | run_stmt
          | config_stmt

// Load quantized model
load_stmt = "load" "quantized_model" STRING "as" IDENT load_config?

load_config = "{" load_option* "}"

load_option = "input" ":" tensor_type
            | "output" ":" tensor_type
            | "quantization" ":" quant_config

tensor_type = "tensor" shape dtype?

shape = "[" INTEGER ("," INTEGER)* "]"

dtype = "float32" | "float16" | "int8" | "uint8" | "int32"

quant_config = "{" quant_option* "}"

quant_option = "scheme" ":" quant_scheme
             | "bits" ":" INTEGER
             | "granularity" ":" granularity

quant_scheme = "symmetric" | "asymmetric"

granularity = "per_tensor" | "per_channel" | "per_group"

// Calibration definition
calibration_def = "calibration" IDENT "{" calibration_option* "}"

calibration_option = "model" ":" STRING
                   | "dataset" ":" STRING
                   | "samples" ":" INTEGER
                   | "method" ":" calibration_method
                   | "output" ":" STRING

calibration_method = "minmax" | "entropy" | "percentile" | "mse"

// Preprocessing
preprocess_def = "preprocess" IDENT ":" NEWLINE INDENT
                 "input" ":" type_hint
                 preprocess_step*
                 "output" ":" tensor_type
                 DEDENT

preprocess_step = "resize" "to" INTEGER "x" INTEGER resize_mode?
                | "normalize" normalize_params
                | "crop" crop_spec
                | "pad" pad_spec

resize_mode = "letterbox" | "stretch" | "crop"

normalize_params = "range" "=" array
                 | "mean" "=" array "std" "=" array

// Pipeline
pipeline_def = "pipeline" IDENT ":" NEWLINE INDENT
               pipeline_input
               pipeline_config?
               pipeline_stmt*
               output_decl
               DEDENT

pipeline_input = "input" ":" type_hint IDENT?

pipeline_config = "config" ":" "{" config_pair* "}"

pipeline_stmt = assign_stmt
              | for_stmt
              | if_stmt
              | clear_stmt

assign_stmt = IDENT "=" expr

for_stmt = "for" IDENT "in" expr ":" NEWLINE INDENT pipeline_stmt* DEDENT

if_stmt = "if" condition ":" NEWLINE INDENT pipeline_stmt* DEDENT

clear_stmt = "clear" IDENT

output_decl = "output" ":" expr

// Expressions
expr = quant_expr
     | compute_expr
     | call_expr
     | IDENT
     | literal

quant_expr = "quantize" expr quant_params?
           | "dequantize" expr

quant_params = "scale" "=" FLOAT "zero_point" "=" INTEGER

compute_expr = op_name expr compute_args?

op_name = "conv2d_fp32" | "conv2d_int8"
        | "dense_fp32" | "dense_int8"
        | "batch_norm_fp32"
        | "relu" | "relu_int8"
        | "max_pool_int8" | "avg_pool_int8"
        | "softmax" | "sigmoid"
        | "flatten" | "reshape"
        | "add_int8" | "mul_int8"

compute_args = (compute_arg)*

compute_arg = "weights" "=" IDENT "." IDENT
            | "params" "=" IDENT "." IDENT
            | "kernel" "=" INTEGER
            | "stride" "=" INTEGER
            | "padding" "=" INTEGER

call_expr = IDENT "(" (expr ("," expr)*)? ")"
          | "forward" IDENT expr
          | "preprocess" IDENT expr
          | "load_image" expr
          | "decode_boxes" expr decode_args?
          | "nms" expr nms_args
          | "stack" expr
          | "append" IDENT expr
          | "len" "(" expr ")"
          | "range" "(" expr ")"

decode_args = "anchors" "=" expr

nms_args = "threshold" "=" FLOAT "iou" "=" FLOAT

// Run statement
run_stmt = "run" run_target ("with" expr)?

run_target = IDENT
           | "calibration" IDENT

// Types and literals
type_hint = "image" | "tensor" | "path" | "list" "[" type_hint "]"

literal = STRING | INTEGER | FLOAT | BOOL | array

array = "[" (literal ("," literal)*)? "]"

config_pair = IDENT ":" literal

// Tokens
IDENT = /[a-zA-Z_][a-zA-Z0-9_]*/
STRING = /"[^"]*"/
INTEGER = /-?[0-9]+/
FLOAT = /-?[0-9]+\.[0-9]+/
BOOL = "true" | "false"
```

## Required New Operations

### Plugin: `zrtl_quant`

```rust
//! Quantization and quantized compute operations

// ============================================================================
// Quantization/Dequantization
// ============================================================================

/// Symmetric quantization: q = round(x / scale)
/// scale = max(abs(x)) / 127
#[no_mangle]
pub extern "C" fn quantize_symmetric_i8(
    input: *const f32,
    output: *mut i8,
    len: u64,
    scale: f32
);

/// Asymmetric quantization: q = round(x / scale) + zero_point
/// scale = (max - min) / 255
/// zero_point = round(-min / scale)
#[no_mangle]
pub extern "C" fn quantize_asymmetric_u8(
    input: *const f32,
    output: *mut u8,
    len: u64,
    scale: f32,
    zero_point: u8
);

/// Dequantize INT8 to FP32: x = (q - zero_point) * scale
#[no_mangle]
pub extern "C" fn dequantize_i8(
    input: *const i8,
    output: *mut f32,
    len: u64,
    scale: f32,
    zero_point: i8
);

/// Dequantize UINT8 to FP32
#[no_mangle]
pub extern "C" fn dequantize_u8(
    input: *const u8,
    output: *mut f32,
    len: u64,
    scale: f32,
    zero_point: u8
);

/// Compute optimal scale for symmetric quantization
#[no_mangle]
pub extern "C" fn compute_scale_symmetric(
    data: *const f32,
    len: u64
) -> f32;

/// Compute scale and zero_point for asymmetric quantization
#[no_mangle]
pub extern "C" fn compute_scale_asymmetric(
    data: *const f32,
    len: u64,
    out_scale: *mut f32,
    out_zero_point: *mut u8
);

// ============================================================================
// Quantized Matrix Operations
// ============================================================================

/// INT8 GEMM with INT32 accumulator
/// C = A @ B where A is [M, K] int8, B is [K, N] int8, C is [M, N] int32
/// Then requantize: out = round(C * (scale_a * scale_b / scale_out)) + zero_out
#[no_mangle]
pub extern "C" fn gemm_i8_i32(
    a: *const i8,
    b: *const i8,
    c: *mut i32,
    m: u64,
    n: u64,
    k: u64,
    // For requantization to int8 output (optional, pass null for int32 output)
    out_i8: *mut i8,
    scale_a: f32,
    scale_b: f32,
    scale_out: f32,
    zero_out: i8
);

/// INT8 GEMM with FP32 accumulator (for mixed precision)
#[no_mangle]
pub extern "C" fn gemm_i8_f32(
    a: *const i8,
    b: *const i8,
    c: *mut f32,
    m: u64,
    n: u64,
    k: u64,
    scale_a: f32,
    scale_b: f32
);

// ============================================================================
// Quantized Convolution
// ============================================================================

/// INT8 2D Convolution
#[no_mangle]
pub extern "C" fn conv2d_i8(
    input: *const i8,
    kernel: *const i8,
    bias: *const i32,        // Bias is in int32 (pre-scaled)
    output: *mut i8,
    // Input params
    batch: u64,
    in_channels: u64,
    in_height: u64,
    in_width: u64,
    // Kernel params
    out_channels: u64,
    kernel_h: u64,
    kernel_w: u64,
    stride: u64,
    padding: u64,
    // Quantization params
    input_scale: f32,
    input_zero: i8,
    kernel_scale: *const f32,  // Per-channel scales [out_channels]
    output_scale: f32,
    output_zero: i8
);

/// INT8 Depthwise Convolution
#[no_mangle]
pub extern "C" fn conv2d_depthwise_i8(
    input: *const i8,
    kernel: *const i8,
    output: *mut i8,
    batch: u64,
    channels: u64,
    height: u64,
    width: u64,
    kernel_h: u64,
    kernel_w: u64,
    stride: u64,
    padding: u64,
    // Quantization params
    input_scale: f32,
    kernel_scale: *const f32,
    output_scale: f32,
    input_zero: i8,
    output_zero: i8
);

// ============================================================================
// Quantized Activations and Pooling
// ============================================================================

/// INT8 ReLU: out = max(input, zero_point)
#[no_mangle]
pub extern "C" fn relu_i8(
    data: *mut i8,
    len: u64,
    zero_point: i8
);

/// INT8 ReLU6 (for MobileNet)
#[no_mangle]
pub extern "C" fn relu6_i8(
    data: *mut i8,
    len: u64,
    zero_point: i8,
    six_quantized: i8  // Quantized value of 6.0
);

/// INT8 Max Pooling
#[no_mangle]
pub extern "C" fn max_pool2d_i8(
    input: *const i8,
    output: *mut i8,
    batch: u64,
    channels: u64,
    height: u64,
    width: u64,
    pool_h: u64,
    pool_w: u64,
    stride: u64
);

/// INT8 Average Pooling (requires rescaling)
#[no_mangle]
pub extern "C" fn avg_pool2d_i8(
    input: *const i8,
    output: *mut i8,
    batch: u64,
    channels: u64,
    height: u64,
    width: u64,
    pool_h: u64,
    pool_w: u64,
    stride: u64,
    input_scale: f32,
    output_scale: f32,
    input_zero: i8,
    output_zero: i8
);

/// INT8 Element-wise Add (with rescaling)
#[no_mangle]
pub extern "C" fn add_i8(
    a: *const i8,
    b: *const i8,
    output: *mut i8,
    len: u64,
    scale_a: f32,
    scale_b: f32,
    scale_out: f32,
    zero_a: i8,
    zero_b: i8,
    zero_out: i8
);

// ============================================================================
// Calibration
// ============================================================================

/// Collect min/max statistics for calibration
#[no_mangle]
pub extern "C" fn calibrate_minmax(
    data: *const f32,
    len: u64,
    running_min: *mut f32,
    running_max: *mut f32
);

/// Entropy calibration (KL divergence minimization)
#[no_mangle]
pub extern "C" fn calibrate_entropy(
    histogram: *const u64,
    num_bins: u64,
    target_bins: u64  // 128 for int8
) -> f32;  // Returns optimal threshold

/// Percentile calibration
#[no_mangle]
pub extern "C" fn calibrate_percentile(
    data: *const f32,
    len: u64,
    percentile: f32  // e.g., 99.99
) -> f32;  // Returns threshold
```

### Plugin: `zrtl_qmodel` (Quantized Model Format)

```rust
//! Quantized model file format and loading

/// Quantized model file format (.qmodel)
/// Header:
///   magic: [u8; 4] = "QMOD"
///   version: u32
///   num_layers: u32
///   metadata_offset: u64
///
/// For each layer:
///   name_len: u32
///   name: [u8; name_len]
///   dtype: u8 (0=f32, 1=i8, 2=u8, 3=i32)
///   rank: u32
///   shape: [u64; rank]
///   scale: f32 (0.0 if f32 layer)
///   zero_point: i32
///   data_offset: u64
///   data_size: u64
///
/// Metadata (JSON):
///   input_shapes, output_shapes, layer_order, etc.

/// Load quantized model
#[no_mangle]
pub extern "C" fn qmodel_load(
    path: *const u8,
    path_len: u32
) -> *mut QModelHandle;

/// Get layer data pointer
#[no_mangle]
pub extern "C" fn qmodel_get_layer(
    model: *const QModelHandle,
    name: *const u8,
    name_len: u32
) -> *const u8;  // Returns raw data pointer

/// Get layer quantization parameters
#[no_mangle]
pub extern "C" fn qmodel_get_quant_params(
    model: *const QModelHandle,
    name: *const u8,
    name_len: u32,
    scale: *mut f32,
    zero_point: *mut i32
);

/// Get layer shape
#[no_mangle]
pub extern "C" fn qmodel_get_shape(
    model: *const QModelHandle,
    name: *const u8,
    name_len: u32,
    shape: *mut u64,
    max_dims: u32
) -> u32;  // Returns actual rank

/// Free model
#[no_mangle]
pub extern "C" fn qmodel_free(model: *mut QModelHandle);

/// Create quantized model from FP32 model + calibration data
#[no_mangle]
pub extern "C" fn qmodel_quantize(
    fp32_model: *const u8,
    fp32_path_len: u32,
    calibration_data: *const f32,
    calibration_samples: u64,
    output_path: *const u8,
    output_path_len: u32,
    method: u32  // 0=minmax, 1=entropy, 2=percentile
) -> i32;  // 0 on success
```

## Dependencies

### Rust Crates

```toml
[dependencies]
# Core
zrtl = { path = "../sdk/zrtl" }
wide = "0.7"

# Model formats
safetensors = "0.4"
memmap2 = "0.9"

# Image processing
image = "0.25"

# Serialization
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
bincode = "1.3"

# Optional: SIMD intrinsics for specific platforms
[target.'cfg(target_arch = "x86_64")'.dependencies]
# For VNNI instructions (INT8 acceleration)

[target.'cfg(target_arch = "aarch64")'.dependencies]
# For ARM dot product instructions

[dev-dependencies]
criterion = "0.5"
approx = "0.5"
```

## File Structure

```
examples/quant/
├── Cargo.toml
├── quant.zyn                       # Grammar definition
├── src/
│   ├── main.rs                     # CLI entry point
│   ├── calibrator.rs               # Calibration logic
│   ├── executor.rs                 # Quantized execution
│   └── model_converter.rs          # FP32 -> INT8 conversion
├── models/
│   ├── mobilenet_v3_int8.qmodel    # Pre-quantized model
│   └── yolov8n_int8.qmodel
├── samples/
│   ├── classify_int8.qinf          # Classification example
│   ├── detect_int8.qinf            # Detection example
│   └── calibrate.qinf              # Calibration example
├── calibration_data/
│   └── sample_images/              # Images for calibration
└── test_data/
    ├── test_image.jpg
    └── reference_outputs.json      # For accuracy testing

plugins/zrtl_quant/
├── Cargo.toml
├── src/
│   ├── lib.rs
│   ├── quantize.rs                 # Quantization functions
│   ├── dequantize.rs               # Dequantization
│   ├── gemm_i8.rs                  # INT8 GEMM
│   ├── conv_i8.rs                  # INT8 convolution
│   ├── activations_i8.rs           # INT8 activations
│   ├── pooling_i8.rs               # INT8 pooling
│   └── calibration.rs              # Calibration algorithms
├── benches/
│   ├── gemm_bench.rs
│   └── conv_bench.rs
└── tests/
    ├── accuracy_tests.rs
    └── numerical_tests.rs

plugins/zrtl_qmodel/
├── Cargo.toml
├── src/
│   ├── lib.rs
│   ├── format.rs                   # .qmodel file format
│   ├── loader.rs                   # Model loading
│   └── converter.rs                # FP32 to INT8 conversion
```

## Implementation Plan

### Phase 1: Core Quantization (Week 1-2)
- [ ] Implement basic quantization/dequantization
  - [ ] Symmetric INT8 quantization
  - [ ] Asymmetric UINT8 quantization
  - [ ] Scale/zero-point computation
- [ ] SIMD-optimized implementations
- [ ] Unit tests for numerical accuracy

### Phase 2: Quantized Compute (Week 2-3)
- [ ] Implement INT8 GEMM
  - [ ] Reference implementation
  - [ ] SIMD optimization (SSE4.1, AVX2, NEON)
  - [ ] Benchmark vs FP32
- [ ] Implement INT8 Convolution
  - [ ] Im2col + GEMM approach
  - [ ] Direct convolution for small kernels
- [ ] INT8 activations and pooling

### Phase 3: Model Format (Week 3)
- [ ] Design .qmodel file format
- [ ] Implement loader
- [ ] Implement FP32 -> INT8 converter
- [ ] Per-channel quantization support

### Phase 4: Calibration (Week 3-4)
- [ ] MinMax calibration
- [ ] Entropy calibration (KL divergence)
- [ ] Percentile calibration
- [ ] Calibration data collection pipeline

### Phase 5: Grammar and Runtime (Week 4)
- [ ] Write grammar
- [ ] Implement semantic actions
- [ ] Build execution engine
- [ ] Mixed-precision support

### Phase 6: Testing and Optimization (Week 5)
- [ ] Accuracy testing vs FP32 reference
- [ ] Performance benchmarks
- [ ] Example models (MobileNet, YOLO)
- [ ] Documentation

## Performance Targets

### Memory Reduction

| Model | FP32 Size | INT8 Size | Reduction |
|-------|-----------|-----------|-----------|
| MobileNetV3-Small | 10.2 MB | 2.6 MB | 4x |
| ResNet-18 | 44.7 MB | 11.2 MB | 4x |
| YOLOv8n | 12.0 MB | 3.0 MB | 4x |

### Compute Speedup (vs FP32)

| Operation | Expected Speedup | Notes |
|-----------|------------------|-------|
| GEMM (large) | 2-4x | Depends on SIMD support |
| Conv2d | 2-3x | Memory bandwidth limited |
| Overall inference | 1.5-2x | Typical end-to-end |

### Accuracy

| Model | FP32 Accuracy | INT8 Accuracy | Drop |
|-------|---------------|---------------|------|
| MobileNetV3 (ImageNet) | 67.4% | 66.8% | <1% |
| YOLOv8n (COCO) | 37.3 mAP | 36.5 mAP | <1% |

## Example Outputs

### Calibration

```bash
$ quant run calibration calibrate_detector

Loading FP32 model: yolov8n.safetensors (12.0 MB)
Collecting calibration data from: calibration_images/

Processing calibration samples...
████████████████████████████████████████ 100/100

Computing quantization parameters:
  Layer conv1: scale=0.0234, zero_point=0 (range: [-2.98, 2.87])
  Layer conv2: scale=0.0156, zero_point=0 (range: [-1.99, 1.95])
  ...
  Layer output: scale=0.0078, zero_point=64 (range: [-0.5, 1.5])

Using entropy calibration for sensitive layers:
  Layer backbone.conv3: adjusted scale 0.0312 -> 0.0289

Saving quantized model: yolov8n_int8.qmodel (3.0 MB)
Compression ratio: 4.0x
Estimated accuracy: -0.8% mAP
```

### Inference

```bash
$ quant run detect_objects --input test.jpg

Loading quantized model: yolov8n_int8.qmodel
  - Weights: INT8 symmetric per-channel
  - Activations: INT8 symmetric per-tensor

Preprocessing: 640x640 letterbox

Running inference...
  - Quantize input: 0.2ms
  - Forward pass: 12.3ms
  - Dequantize output: 0.1ms
  - Post-processing: 1.8ms

Detections:
  [0.89] person at (123, 45, 456, 678)
  [0.82] car at (234, 123, 567, 345)
  [0.76] dog at (345, 234, 456, 456)

Total: 14.4ms (69 FPS)
Memory: 8.2 MB peak
```

## Testing Strategy

### Numerical Accuracy
- Compare quantized outputs to FP32 reference
- Test edge cases (near zero, saturation)
- Per-layer error analysis

### Model Accuracy
- Run on validation sets
- Compare mAP/accuracy with FP32
- Test multiple calibration methods

### Performance
- Benchmark vs FP32 on same hardware
- Memory usage profiling
- Throughput at various batch sizes

### Compatibility
- Test multiple model architectures
- Verify calibration across datasets

## Future Enhancements

1. **INT4 Quantization**
   - 2x additional compression
   - Group quantization for accuracy

2. **Dynamic Quantization**
   - Quantize activations on-the-fly
   - Better for variable input distributions

3. **Quantization-Aware Training (QAT)**
   - Training with fake quantization
   - Better accuracy for aggressive quantization

4. **Mixed-Precision Optimization**
   - Automatic sensitivity analysis
   - Per-layer precision selection

5. **Hardware-Specific Kernels**
   - Intel VNNI support
   - ARM dot product instructions
   - Apple AMX
