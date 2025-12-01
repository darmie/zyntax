# Audio Processing DSL

**Priority**: Low
**Complexity**: High
**Estimated New Code**: ~4500 LOC

## Overview

A domain-specific language for audio signal processing and speech ML inference. Supports feature extraction (spectrograms, MFCCs), audio classification, and speech recognition on CPU.

## Use Cases

1. **Speech Recognition**
   - Voice commands
   - Transcription
   - Voice search

2. **Audio Classification**
   - Sound event detection
   - Music genre classification
   - Environmental sound recognition

3. **Speaker Identification**
   - Voice authentication
   - Speaker diarization
   - Voice fingerprinting

4. **Audio Enhancement**
   - Noise reduction
   - Echo cancellation
   - Audio restoration

## Syntax Design

```
// audio.audl - Audio Processing DSL

// Load audio processing model
load audio_model "whisper_tiny.safetensors" as transcriber {
    architecture: whisper,
    sample_rate: 16000,
    n_mels: 80,
    n_fft: 400,
    hop_length: 160,
    max_audio_length: 30,  // seconds
    vocab: "whisper_vocab.json"
}

load audio_model "yamnet.safetensors" as classifier {
    architecture: yamnet,
    sample_rate: 16000,
    n_mels: 64,
    classes: "yamnet_classes.txt"
}

// Audio preprocessing configuration
preprocess whisper_prep:
    input: audio

    // Resample if needed
    resample to 16000

    // Pad or trim to 30 seconds
    pad_or_trim to 30 seconds

    // Compute log-mel spectrogram
    stft n_fft=400 hop_length=160 window=hann
    mel_filterbank n_mels=80 fmin=0 fmax=8000
    log_mel = log(mel + 1e-6)

    // Normalize
    normalize mean=-4.0 std=4.0

    output: tensor[1, 80, 3000]  // [batch, n_mels, time_frames]

preprocess yamnet_prep:
    input: audio

    resample to 16000

    // Extract patches (0.96s windows with 0.48s hop)
    patches = extract_patches window=0.96 hop=0.48

    for patch in patches:
        stft n_fft=400 hop_length=160
        mel_filterbank n_mels=64 fmin=125 fmax=7500
        log_mel = log(mel + 0.001)

    output: tensor[num_patches, 96, 64]

// Whisper encoder architecture
architecture whisper_encoder:
    // Initial convolution
    layer conv1: conv1d 80 -> 512 kernel=3 padding=1 activation=gelu
    layer conv2: conv1d 512 -> 512 kernel=3 stride=2 padding=1 activation=gelu

    // Sinusoidal position embedding
    layer pos_embed: sinusoidal_position 1500 512

    // Transformer blocks
    repeat 4:  // tiny has 4 encoder layers
        layer attention:
            q_proj: linear 512 -> 512
            k_proj: linear 512 -> 512
            v_proj: linear 512 -> 512
            out_proj: linear 512 -> 512
            num_heads: 8

        layer attention_norm: layer_norm 512

        layer ffn:
            fc1: linear 512 -> 2048
            fc2: linear 2048 -> 512
            activation: gelu

        layer ffn_norm: layer_norm 512

    layer final_norm: layer_norm 512

// Whisper decoder architecture
architecture whisper_decoder:
    layer token_embed: embedding 51865 -> 512
    layer pos_embed: learned_position 448 512

    repeat 4:  // tiny has 4 decoder layers
        // Self-attention (causal)
        layer self_attention:
            q_proj: linear 512 -> 512
            k_proj: linear 512 -> 512
            v_proj: linear 512 -> 512
            out_proj: linear 512 -> 512
            num_heads: 8
            causal: true

        layer self_attn_norm: layer_norm 512

        // Cross-attention to encoder output
        layer cross_attention:
            q_proj: linear 512 -> 512
            k_proj: linear 512 -> 512
            v_proj: linear 512 -> 512
            out_proj: linear 512 -> 512
            num_heads: 8

        layer cross_attn_norm: layer_norm 512

        layer ffn:
            fc1: linear 512 -> 2048
            fc2: linear 2048 -> 512
            activation: gelu

        layer ffn_norm: layer_norm 512

    layer final_norm: layer_norm 512
    layer lm_head: linear 512 -> 51865

// Speech recognition pipeline
pipeline transcribe:
    input: audio_file path
    config: {
        language: "en",
        task: "transcribe",
        beam_size: 5,
        temperature: 0.0
    }

    // Load audio
    audio = load_audio audio_file

    // Preprocess
    mel = preprocess whisper_prep audio

    // Encode audio
    encoder_output = forward transcriber.encoder mel

    // Decode with beam search
    tokens = []
    decoder_input = [transcriber.sot_token, transcriber.lang_token["en"]]

    kv_cache = create_kv_cache transcriber.decoder.num_layers 512

    while len(tokens) < 448:
        // Decoder forward
        decoder_output = forward_decoder transcriber.decoder decoder_input encoder_output kv_cache

        // Get logits for last position
        logits = decoder_output[-1]

        // Suppress special tokens if needed
        suppress_tokens logits [transcriber.sot_token, transcriber.translate_token]

        // Sample or beam search
        if config.temperature == 0.0:
            next_token = argmax logits
        else:
            probs = softmax(logits / config.temperature)
            next_token = sample probs

        if next_token == transcriber.eot_token:
            break

        append tokens next_token
        decoder_input = [next_token]

    // Decode tokens to text
    text = decode_tokens transcriber.vocab tokens

    output: {
        text: text,
        language: config.language
    }

// Audio classification pipeline
pipeline classify_audio:
    input: audio_file path
    config: {
        top_k: 5
    }

    audio = load_audio audio_file

    // Preprocess into patches
    patches = preprocess yamnet_prep audio

    // Classify each patch
    all_scores = []

    for patch in patches:
        logits = forward classifier patch
        scores = sigmoid logits
        append all_scores scores

    // Average scores across patches
    avg_scores = mean all_scores axis=0

    // Get top-k classes
    top_indices = topk avg_scores k=config.top_k
    top_classes = lookup classifier.classes top_indices

    output: {
        predictions: zip(top_classes, avg_scores[top_indices]),
        num_patches: len(patches)
    }

// Voice activity detection
pipeline detect_speech:
    input: audio_stream
    config: {
        frame_length: 0.03,  // 30ms frames
        threshold: 0.5,
        min_speech_duration: 0.3
    }

    frames = extract_frames audio_stream length=config.frame_length

    speech_probs = []
    for frame in frames:
        features = compute_mfcc frame n_mfcc=13
        prob = forward vad_model features
        append speech_probs prob

    // Smooth predictions
    smoothed = median_filter speech_probs window=5

    // Find speech segments
    segments = find_segments smoothed threshold=config.threshold min_duration=config.min_speech_duration

    output: segments

// Real-time audio processing
pipeline realtime_transcribe:
    input: microphone
    config: {
        chunk_duration: 5,  // seconds
        overlap: 1
    }

    // Continuous processing loop
    stream audio_chunks from microphone chunk_size=config.chunk_duration overlap=config.overlap:

        // Check for speech
        if has_speech audio_chunks:
            mel = preprocess whisper_prep audio_chunks
            encoder_out = forward transcriber.encoder mel

            // Decode
            text = greedy_decode transcriber.decoder encoder_out

            emit transcription text timestamp=now()

// Run transcription
run transcribe with "recording.wav"

// Run classification
run classify_audio with "sound.wav"
```

## Grammar Specification

```
// audio.zyn

@name = "audio"
@version = "1.0"
@file_extensions = [".audl", ".audio"]

@builtins {
    // Audio I/O
    audio_load: "$Audio$load"
    audio_save: "$Audio$save"
    audio_resample: "$Audio$resample"

    // Signal processing (new: zrtl_dsp)
    stft: "$DSP$stft"
    istft: "$DSP$istft"
    mel_filterbank: "$DSP$mel_filterbank"
    mfcc: "$DSP$mfcc"
    fft: "$DSP$fft"
    ifft: "$DSP$ifft"

    // Window functions
    hann_window: "$DSP$hann"
    hamming_window: "$DSP$hamming"

    // Audio utilities
    pad_or_trim: "$Audio$pad_or_trim"
    extract_patches: "$Audio$extract_patches"
    extract_frames: "$Audio$extract_frames"

    // ML operations (reuse existing)
    conv1d: "$Conv$conv1d_f32"
    linear: "$SIMD$gemm_f32"
    layer_norm: "$SIMD$layer_norm_f32"
    softmax: "$SIMD$softmax_f32"
    sigmoid: "$SIMD$sigmoid_f32"
    gelu: "$SIMD$gelu_f32"
    argmax: "$SIMD$argmax_f32"

    // Attention (reuse from transformer)
    multi_head_attention: "$Attention$multi_head"
    cross_attention: "$Attention$cross"
    causal_attention: "$Attention$causal"

    // Utilities
    topk: "$VecSearch$topk"
    mean: "$Stats$mean_f32"
    median_filter: "$DSP$median_filter"
    print: "$IO$println"
}

// Grammar rules
program = (load_stmt | preprocess_def | architecture_def | pipeline_def | run_stmt)*

// Load audio model
load_stmt = "load" "audio_model" STRING "as" IDENT model_config

model_config = "{" model_option* "}"

model_option = "architecture" ":" IDENT
             | "sample_rate" ":" INTEGER
             | "n_mels" ":" INTEGER
             | "n_fft" ":" INTEGER
             | "hop_length" ":" INTEGER
             | "max_audio_length" ":" INTEGER
             | "vocab" ":" STRING
             | "classes" ":" STRING

// Audio preprocessing
preprocess_def = "preprocess" IDENT ":" NEWLINE INDENT
                 "input" ":" "audio"
                 preprocess_step*
                 "output" ":" tensor_type
                 DEDENT

preprocess_step = "resample" "to" INTEGER
                | "pad_or_trim" "to" INTEGER ("seconds" | "samples")
                | "stft" stft_params
                | "mel_filterbank" mel_params
                | "extract_patches" patch_params
                | "normalize" normalize_params
                | IDENT "=" expr
                | "for" IDENT "in" IDENT ":" NEWLINE INDENT preprocess_step* DEDENT

stft_params = ("n_fft" "=" INTEGER)? ("hop_length" "=" INTEGER)? ("window" "=" window_type)?

window_type = "hann" | "hamming" | "blackman"

mel_params = ("n_mels" "=" INTEGER)? ("fmin" "=" INTEGER)? ("fmax" "=" INTEGER)?

patch_params = "window" "=" FLOAT "hop" "=" FLOAT

normalize_params = ("mean" "=" FLOAT)? ("std" "=" FLOAT)?
                 | "range" "=" "[" FLOAT "," FLOAT "]"

// Architecture (similar to transformer)
architecture_def = "architecture" IDENT ":" NEWLINE INDENT layer_def* DEDENT

layer_def = "layer" IDENT ":" layer_spec
          | "repeat" INTEGER ":" NEWLINE INDENT layer_def* DEDENT

layer_spec = conv1d_spec
           | linear_spec
           | attention_spec
           | norm_spec
           | embedding_spec
           | position_spec

conv1d_spec = "conv1d" INTEGER "->" INTEGER conv1d_opts?

conv1d_opts = ("kernel" "=" INTEGER)? ("stride" "=" INTEGER)?
              ("padding" "=" INTEGER)? ("activation" "=" activation)?

// Pipeline
pipeline_def = "pipeline" IDENT ":" NEWLINE INDENT
               pipeline_input
               pipeline_config?
               pipeline_stmt*
               output_decl
               DEDENT

pipeline_input = "input" ":" input_type IDENT?

input_type = "audio_file" | "audio_stream" | "microphone" | "path"

pipeline_config = "config" ":" "{" config_pair* "}"

pipeline_stmt = assign_stmt
              | for_stmt
              | while_stmt
              | if_stmt
              | emit_stmt
              | stream_stmt

assign_stmt = IDENT "=" expr

for_stmt = "for" IDENT "in" expr ":" NEWLINE INDENT pipeline_stmt* DEDENT

while_stmt = "while" condition ":" NEWLINE INDENT pipeline_stmt* DEDENT

if_stmt = "if" condition ":" NEWLINE INDENT pipeline_stmt* DEDENT

emit_stmt = "emit" IDENT expr ("timestamp" "=" expr)?

stream_stmt = "stream" IDENT "from" expr stream_params ":" NEWLINE INDENT pipeline_stmt* DEDENT

stream_params = ("chunk_size" "=" FLOAT)? ("overlap" "=" FLOAT)?

output_decl = "output" ":" expr

// Expressions
expr = binary_expr
     | call_expr
     | index_expr
     | member_expr
     | object_literal
     | array_literal
     | IDENT
     | literal

binary_expr = expr binary_op expr

call_expr = func_name expr (IDENT "=" expr)*
          | func_name "(" (expr ("," expr)*)? ")"
          | "preprocess" IDENT expr
          | "forward" member_expr expr
          | "forward_decoder" member_expr expr expr expr
          | "load_audio" expr
          | "decode_tokens" member_expr expr
          | "compute_mfcc" expr mfcc_params?
          | "greedy_decode" member_expr expr
          | "has_speech" expr
          | "find_segments" expr segment_params

mfcc_params = "n_mfcc" "=" INTEGER

segment_params = "threshold" "=" FLOAT "min_duration" "=" FLOAT

func_name = IDENT

object_literal = "{" (IDENT ":" expr ("," IDENT ":" expr)*)? "}"

array_literal = "[" (expr ("," expr)*)? "]"

// Types
tensor_type = "tensor" "[" INTEGER ("," INTEGER)* "]"

// Run
run_stmt = "run" IDENT "with" expr

// Tokens
IDENT = /[a-zA-Z_][a-zA-Z0-9_]*/
STRING = /"[^"]*"/
INTEGER = /-?[0-9]+/
FLOAT = /-?[0-9]+\.[0-9]+/
BOOL = "true" | "false"
```

## Required New Operations

### Plugin: `zrtl_dsp`

```rust
//! Digital Signal Processing operations

/// Short-Time Fourier Transform
/// Input: [samples]
/// Output: [num_frames, n_fft/2 + 1] complex magnitudes
#[no_mangle]
pub extern "C" fn stft(
    input: *const f32,
    output_real: *mut f32,
    output_imag: *mut f32,
    num_samples: u64,
    n_fft: u64,
    hop_length: u64,
    window: *const f32  // Pre-computed window [n_fft]
) -> u64;  // Returns num_frames

/// Inverse STFT
#[no_mangle]
pub extern "C" fn istft(
    input_real: *const f32,
    input_imag: *const f32,
    output: *mut f32,
    num_frames: u64,
    n_fft: u64,
    hop_length: u64,
    window: *const f32
) -> u64;  // Returns num_samples

/// Create mel filterbank matrix
/// Returns: [n_mels, n_fft/2 + 1]
#[no_mangle]
pub extern "C" fn create_mel_filterbank(
    output: *mut f32,
    n_mels: u64,
    n_fft: u64,
    sample_rate: u64,
    fmin: f32,
    fmax: f32
);

/// Apply mel filterbank to spectrogram
/// Input: [num_frames, n_fft/2 + 1]
/// Output: [num_frames, n_mels]
#[no_mangle]
pub extern "C" fn apply_mel_filterbank(
    spectrogram: *const f32,
    filterbank: *const f32,
    output: *mut f32,
    num_frames: u64,
    freq_bins: u64,
    n_mels: u64
);

/// Compute MFCC features
#[no_mangle]
pub extern "C" fn mfcc(
    input: *const f32,
    output: *mut f32,
    num_samples: u64,
    sample_rate: u64,
    n_mfcc: u64,
    n_fft: u64,
    hop_length: u64,
    n_mels: u64
) -> u64;  // Returns num_frames

/// FFT (real to complex)
#[no_mangle]
pub extern "C" fn fft_real(
    input: *const f32,
    output_real: *mut f32,
    output_imag: *mut f32,
    n: u64
);

/// IFFT (complex to real)
#[no_mangle]
pub extern "C" fn ifft_real(
    input_real: *const f32,
    input_imag: *const f32,
    output: *mut f32,
    n: u64
);

/// Create Hann window
#[no_mangle]
pub extern "C" fn hann_window(output: *mut f32, n: u64);

/// Create Hamming window
#[no_mangle]
pub extern "C" fn hamming_window(output: *mut f32, n: u64);

/// Median filter for smoothing
#[no_mangle]
pub extern "C" fn median_filter(
    input: *const f32,
    output: *mut f32,
    len: u64,
    window_size: u64
);

/// Compute magnitude from complex
#[no_mangle]
pub extern "C" fn complex_magnitude(
    real: *const f32,
    imag: *const f32,
    output: *mut f32,
    len: u64
);

/// Compute power spectrum (magnitude squared)
#[no_mangle]
pub extern "C" fn power_spectrum(
    real: *const f32,
    imag: *const f32,
    output: *mut f32,
    len: u64
);
```

### Plugin: `zrtl_audio`

```rust
//! Audio loading and manipulation

/// Audio handle with metadata
pub struct AudioHandle {
    samples: Vec<f32>,
    sample_rate: u32,
    channels: u32,
}

/// Load audio from file
/// Automatically converts to mono float32
#[no_mangle]
pub extern "C" fn audio_load(
    path: *const u8,
    path_len: u32
) -> *mut AudioHandle;

/// Save audio to file
#[no_mangle]
pub extern "C" fn audio_save(
    handle: *const AudioHandle,
    path: *const u8,
    path_len: u32,
    format: u32  // 0=wav, 1=mp3, 2=flac
) -> i32;

/// Get sample rate
#[no_mangle]
pub extern "C" fn audio_sample_rate(handle: *const AudioHandle) -> u32;

/// Get number of samples
#[no_mangle]
pub extern "C" fn audio_num_samples(handle: *const AudioHandle) -> u64;

/// Get samples pointer
#[no_mangle]
pub extern "C" fn audio_samples(handle: *const AudioHandle) -> *const f32;

/// Resample audio to target sample rate
#[no_mangle]
pub extern "C" fn audio_resample(
    input: *const f32,
    output: *mut f32,
    num_samples: u64,
    src_rate: u32,
    dst_rate: u32
) -> u64;  // Returns number of output samples

/// Pad or trim audio to target length
#[no_mangle]
pub extern "C" fn audio_pad_or_trim(
    input: *const f32,
    output: *mut f32,
    num_samples: u64,
    target_samples: u64
);

/// Extract overlapping patches/frames
#[no_mangle]
pub extern "C" fn audio_extract_patches(
    input: *const f32,
    output: *mut f32,
    num_samples: u64,
    patch_samples: u64,
    hop_samples: u64
) -> u64;  // Returns number of patches

/// Free audio handle
#[no_mangle]
pub extern "C" fn audio_free(handle: *mut AudioHandle);
```

### Plugin: `zrtl_conv1d`

```rust
//! 1D Convolution for audio models

/// 1D Convolution
/// Input: [batch, in_channels, length]
/// Kernel: [out_channels, in_channels, kernel_size]
/// Output: [batch, out_channels, out_length]
#[no_mangle]
pub extern "C" fn conv1d_f32(
    input: *const f32,
    kernel: *const f32,
    bias: *const f32,  // [out_channels] or null
    output: *mut f32,
    batch: u64,
    in_channels: u64,
    out_channels: u64,
    length: u64,
    kernel_size: u64,
    stride: u64,
    padding: u64
);

/// Depthwise 1D Convolution
#[no_mangle]
pub extern "C" fn conv1d_depthwise_f32(
    input: *const f32,
    kernel: *const f32,
    output: *mut f32,
    batch: u64,
    channels: u64,
    length: u64,
    kernel_size: u64,
    stride: u64,
    padding: u64
);

/// 1D Transposed Convolution
#[no_mangle]
pub extern "C" fn conv1d_transpose_f32(
    input: *const f32,
    kernel: *const f32,
    bias: *const f32,
    output: *mut f32,
    batch: u64,
    in_channels: u64,
    out_channels: u64,
    length: u64,
    kernel_size: u64,
    stride: u64,
    padding: u64
);
```

## Dependencies

### Rust Crates

```toml
[dependencies]
# Core
zrtl = { path = "../sdk/zrtl" }
wide = "0.7"

# Audio I/O
symphonia = { version = "0.5", features = ["mp3", "flac", "wav", "ogg"] }
hound = "3.5"  # WAV reading/writing

# DSP
rustfft = "6.2"
realfft = "3.3"

# Resampling
rubato = "0.15"

# Model loading
safetensors = "0.4"
memmap2 = "0.9"

# Tokenization (for Whisper)
tokenizers = "0.19"

# Serialization
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"

[dev-dependencies]
criterion = "0.5"
approx = "0.5"
```

## File Structure

```
examples/audio/
├── Cargo.toml
├── audio.zyn                       # Grammar definition
├── src/
│   ├── main.rs                     # CLI entry point
│   ├── model.rs                    # Model loading
│   ├── executor.rs                 # Inference executor
│   ├── preprocessing.rs            # Audio preprocessing
│   └── streaming.rs                # Real-time processing
├── models/
│   ├── whisper_tiny/
│   │   ├── encoder.safetensors
│   │   ├── decoder.safetensors
│   │   └── vocab.json
│   ├── yamnet/
│   │   ├── model.safetensors
│   │   └── classes.txt
│   └── silero_vad/
│       └── model.safetensors
├── samples/
│   ├── transcribe.audl             # Speech recognition
│   ├── classify.audl               # Audio classification
│   └── vad.audl                    # Voice activity detection
└── test_audio/
    ├── speech_sample.wav
    └── sound_effects.wav

plugins/zrtl_dsp/
├── Cargo.toml
├── src/
│   ├── lib.rs
│   ├── stft.rs
│   ├── mel.rs
│   ├── mfcc.rs
│   ├── fft.rs
│   └── window.rs
├── benches/
│   └── dsp_bench.rs
└── tests/
    └── dsp_tests.rs

plugins/zrtl_audio/
├── Cargo.toml
├── src/
│   ├── lib.rs
│   ├── load.rs
│   ├── resample.rs
│   └── manipulation.rs

plugins/zrtl_conv1d/
├── Cargo.toml
├── src/
│   ├── lib.rs
│   └── conv1d.rs
```

## Implementation Plan

### Phase 1: Audio I/O (Week 1)
- [ ] Implement `zrtl_audio` plugin
  - [ ] WAV/MP3/FLAC loading
  - [ ] Resampling
  - [ ] Pad/trim operations
- [ ] Unit tests

### Phase 2: DSP Operations (Week 1-2)
- [ ] Implement `zrtl_dsp` plugin
  - [ ] FFT/IFFT wrapper
  - [ ] STFT/ISTFT
  - [ ] Mel filterbank
  - [ ] Window functions
- [ ] MFCC computation
- [ ] Benchmarks

### Phase 3: 1D Convolution (Week 2)
- [ ] Implement `zrtl_conv1d` plugin
  - [ ] Standard conv1d
  - [ ] Depthwise conv1d
  - [ ] Transposed conv1d
- [ ] SIMD optimization

### Phase 4: Grammar and Preprocessing (Week 3)
- [ ] Write grammar
- [ ] Implement preprocessing pipeline
- [ ] Audio feature extraction

### Phase 5: Model Integration (Week 3-4)
- [ ] Whisper encoder
- [ ] Whisper decoder with cross-attention
- [ ] YAMNet classifier
- [ ] VAD model

### Phase 6: Streaming Support (Week 4-5)
- [ ] Chunked processing
- [ ] Real-time microphone input
- [ ] Overlapping windows

### Phase 7: Testing and Examples (Week 5)
- [ ] End-to-end tests
- [ ] Example pipelines
- [ ] Documentation

## Performance Targets

### Whisper Tiny Transcription

| Metric | Target | Notes |
|--------|--------|-------|
| 30s audio preprocessing | 50ms | STFT + mel filterbank |
| Encoder forward | 150ms | 4 layers |
| Decode per token | 20ms | With KV cache |
| Total for 30s audio | ~2s | Full transcription |

### Feature Extraction

| Operation | Target | Input |
|-----------|--------|-------|
| STFT (30s audio) | 30ms | 16kHz, n_fft=400 |
| Mel filterbank | 5ms | 80 mels |
| MFCC (30s audio) | 40ms | 13 coefficients |
| Resample 44.1k -> 16k | 20ms | 30s audio |

### YAMNet Classification

| Metric | Target |
|--------|--------|
| Preprocessing | 15ms |
| Forward (per patch) | 8ms |
| Total (10 patches) | 95ms |

## Example Outputs

### Speech Transcription

```bash
$ audio run transcribe --input speech_sample.wav

Loading model: whisper_tiny
  - Encoder: 4 layers, 512 hidden
  - Decoder: 4 layers, 51865 vocab
  - Sample rate: 16000Hz

Loading audio: speech_sample.wav
  - Duration: 12.3s
  - Sample rate: 44100Hz -> resampling to 16000Hz

Preprocessing:
  - Resample: 8.2ms
  - STFT: 15.3ms
  - Mel filterbank: 4.1ms
  - Total: 27.6ms

Encoding:
  - Forward pass: 89.2ms

Decoding:
  - Tokens generated: 47
  - Time: 892ms (52.7 tokens/sec)

Transcription:
"Hello, this is a test of the speech recognition system.
It should be able to transcribe this audio accurately."

Total time: 1.01s
Real-time factor: 0.082x (12.2x faster than real-time)
```

### Audio Classification

```bash
$ audio run classify_audio --input doorbell.wav

Loading model: yamnet
  - Classes: 521
  - Sample rate: 16000Hz

Loading audio: doorbell.wav
  - Duration: 2.1s

Preprocessing into patches:
  - Patches extracted: 4 (0.96s windows)
  - Preprocessing time: 12.3ms

Classification:
  - Forward pass: 32.1ms (4 patches)

Predictions (top 5):
  1. [0.87] Doorbell
  2. [0.12] Ding-dong
  3. [0.08] Bell
  4. [0.03] Chime
  5. [0.02] Alarm

Total time: 44.4ms
```

### Voice Activity Detection

```bash
$ audio run detect_speech --input meeting_recording.wav

Loading VAD model: silero_vad

Processing audio: meeting_recording.wav
  - Duration: 300s (5 minutes)
  - Frame size: 30ms

Analyzing speech activity...
████████████████████████████████████████ 10000/10000 frames

Speech segments detected: 47

Segments:
  [0:03.2 - 0:15.8] Speaker activity
  [0:18.1 - 0:45.3] Speaker activity
  [0:47.0 - 1:12.4] Speaker activity
  ...

Summary:
  - Total speech: 4:12 (84%)
  - Total silence: 0:48 (16%)
  - Processing time: 1.2s
  - Real-time factor: 0.004x
```

## Testing Strategy

### Unit Tests
- FFT correctness (compare with numpy)
- Mel filterbank accuracy
- STFT/ISTFT round-trip
- Resampling quality

### Integration Tests
- End-to-end transcription
- Classification accuracy
- Streaming processing

### Accuracy Tests
- WER (Word Error Rate) for transcription
- Classification accuracy vs reference
- Compare spectrograms with librosa

### Performance Tests
- Latency benchmarks
- Throughput measurement
- Memory usage

## Future Enhancements

1. **Streaming Recognition**
   - Online/incremental decoding
   - Voice activity triggered

2. **Speaker Diarization**
   - Speaker embedding extraction
   - Clustering for speaker ID

3. **Noise Reduction**
   - Spectral subtraction
   - Neural denoising

4. **More Models**
   - Wav2Vec 2.0
   - HuBERT
   - Conformer

5. **GPU Acceleration**
   - FFT on GPU
   - Batched inference
