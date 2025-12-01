# Anomaly Detection DSL

**Priority**: Medium
**Complexity**: Medium
**Estimated New Code**: ~2500 LOC

## Overview

A domain-specific language for real-time anomaly detection in streaming data. Optimized for IoT sensors, industrial monitoring, and time-series analysis on edge devices.

## Use Cases

1. **Industrial IoT**
   - Equipment failure prediction
   - Process anomaly detection
   - Quality control

2. **Infrastructure Monitoring**
   - Server health monitoring
   - Network traffic analysis
   - Security intrusion detection

3. **Financial Systems**
   - Fraud detection
   - Trading anomalies
   - Transaction monitoring

4. **Healthcare**
   - Patient vital monitoring
   - Medical device alerts
   - Clinical anomalies

## Syntax Design

```
// anomaly.detect - Anomaly Detection DSL

// Configuration
config {
    window_size: 100,
    alert_cooldown: 60s,
    log_level: "info"
}

// Define data sources
source sensor_data from mqtt://sensors/+ {
    format: json,
    fields: [timestamp, sensor_id, temperature, pressure, vibration],
    buffer_size: 1000
}

source file_data from file://logs/metrics.csv {
    format: csv,
    has_header: true,
    timestamp_column: "time"
}

// Define baseline/reference patterns
baseline normal_operation from "baseline_stats.json" {
    // Pre-computed statistics
    mean: [25.0, 101.3, 0.02],      // temp, pressure, vibration
    std: [2.0, 5.0, 0.005],
    min: [18.0, 95.0, 0.0],
    max: [35.0, 110.0, 0.05]
}

// Feature extraction pipeline
features sensor_features:
    input: window[100] of [temperature, pressure, vibration]

    // Statistical features
    mean_temp = mean(temperature)
    std_temp = std(temperature)
    min_temp = min(temperature)
    max_temp = max(temperature)

    // Derived features
    temp_range = max_temp - min_temp
    pressure_derivative = diff(pressure)
    vibration_energy = sum(vibration^2)

    // Rolling statistics
    temp_zscore = (temperature[-1] - mean_temp) / std_temp
    pressure_trend = linear_slope(pressure)

    output: [mean_temp, std_temp, temp_range, pressure_derivative,
             vibration_energy, temp_zscore, pressure_trend]

// Anomaly detection rules
detector multi_signal_detector:
    input: features from sensor_features

    // Statistical thresholds
    rule high_temp:
        condition: temp_zscore > 3.0
        severity: warning
        message: "Temperature {temp_zscore:.2f} std devs above normal"

    rule extreme_temp:
        condition: temp_zscore > 5.0 or temp_zscore < -5.0
        severity: critical
        message: "Extreme temperature deviation detected"

    // Trend-based rules
    rule pressure_spike:
        condition: abs(pressure_derivative) > 10.0
        severity: warning
        message: "Rapid pressure change: {pressure_derivative:.2f} units/sample"

    // Multi-variate rules
    rule equipment_failure:
        condition: vibration_energy > 0.1 and pressure_trend < -1.0
        severity: critical
        message: "Potential equipment failure - high vibration with pressure drop"

    // ML-based scoring (isolation forest style)
    rule isolation_anomaly:
        condition: isolation_score(features) > 0.7
        severity: warning
        message: "Statistical anomaly detected (score: {score:.2f})"

// Alert actions
action slack_alert:
    type: webhook
    url: "https://hooks.slack.com/services/xxx"
    template: |
        :warning: *Anomaly Alert*
        Severity: {severity}
        Sensor: {sensor_id}
        Message: {message}
        Time: {timestamp}

action log_alert:
    type: file
    path: "alerts.log"
    format: "{timestamp},{severity},{sensor_id},{message}"

action email_alert:
    type: email
    to: ["ops@company.com"]
    subject: "[{severity}] Anomaly: {message}"

// Main pipeline
pipeline monitor_sensors:
    input: stream from sensor_data

    // Apply feature extraction
    features = extract sensor_features from input

    // Run detector
    anomalies = detect multi_signal_detector on features

    // Route alerts by severity
    when anomalies.severity == "critical":
        trigger slack_alert
        trigger email_alert

    when anomalies.severity == "warning":
        trigger log_alert

    // Always log metrics
    emit metrics to influxdb://localhost:8086/anomaly_metrics

// Run continuous monitoring
run monitor_sensors continuous with checkpoint every 5min
```

## Grammar Specification

```
// anomaly.zyn

@name = "anomaly"
@version = "1.0"
@file_extensions = [".detect", ".anomaly"]

@builtins {
    // Statistical operations (from zrtl_simd)
    vec_sum: "$SIMD$sum_f32"
    vec_mean: "$Stats$mean_f32"
    vec_std: "$Stats$std_f32"
    vec_min: "$SIMD$min_f32"
    vec_max: "$SIMD$max_f32"
    vec_var: "$Stats$var_f32"

    // Time series operations (new: zrtl_timeseries)
    ts_diff: "$TimeSeries$diff"
    ts_rolling_mean: "$TimeSeries$rolling_mean"
    ts_rolling_std: "$TimeSeries$rolling_std"
    ts_linear_slope: "$TimeSeries$linear_slope"
    ts_ewma: "$TimeSeries$ewma"
    ts_autocorr: "$TimeSeries$autocorrelation"

    // Anomaly scoring (new: zrtl_anomaly)
    isolation_score: "$Anomaly$isolation_forest_score"
    mahalanobis_distance: "$Anomaly$mahalanobis"
    local_outlier_factor: "$Anomaly$lof_score"
    zscore: "$Anomaly$zscore"

    // Window operations
    window_create: "$Window$create"
    window_push: "$Window$push"
    window_get: "$Window$get_buffer"
    window_full: "$Window$is_full"

    // IO sources
    mqtt_connect: "$IO$mqtt_connect"
    mqtt_subscribe: "$IO$mqtt_subscribe"
    file_stream: "$IO$file_stream_csv"
    http_post: "$IO$http_post"

    // Utilities
    print: "$IO$println"
    json_encode: "$IO$json_encode"
    timestamp_now: "$Time$now"
}

// Grammar rules
program = config_block? (source_def | baseline_def | features_def | detector_def | action_def | pipeline_def | run_stmt)*

// Configuration
config_block = "config" "{" config_pair* "}"

config_pair = IDENT ":" literal

// Data source definition
source_def = "source" IDENT "from" source_uri source_config?

source_uri = IDENT "://" /[^\s{]+/

source_config = "{" source_option* "}"

source_option = "format" ":" ("json" | "csv" | "binary")
              | "fields" ":" "[" ident_list "]"
              | "buffer_size" ":" INTEGER
              | "has_header" ":" BOOL
              | "timestamp_column" ":" STRING

// Baseline definition
baseline_def = "baseline" IDENT "from" STRING baseline_config?

baseline_config = "{" baseline_stat* "}"

baseline_stat = "mean" ":" array
              | "std" ":" array
              | "min" ":" array
              | "max" ":" array
              | "covariance" ":" array

// Feature extraction
features_def = "features" IDENT ":" NEWLINE INDENT
               "input" ":" window_spec
               feature_stmt+
               "output" ":" "[" ident_list "]"
               DEDENT

window_spec = "window" "[" INTEGER "]" "of" "[" ident_list "]"

feature_stmt = IDENT "=" feature_expr

feature_expr = stat_call
             | binary_expr
             | index_expr
             | IDENT

stat_call = stat_func "(" IDENT ")"
          | stat_func "(" IDENT "," INTEGER ")"  // with parameter

stat_func = "mean" | "std" | "var" | "min" | "max" | "sum"
          | "diff" | "linear_slope" | "ewma" | "autocorr"

binary_expr = feature_expr binary_op feature_expr

binary_op = "+" | "-" | "*" | "/" | "^"

index_expr = IDENT "[" INTEGER "]"

// Detector definition
detector_def = "detector" IDENT ":" NEWLINE INDENT
               "input" ":" "features" "from" IDENT
               rule_def+
               DEDENT

rule_def = "rule" IDENT ":" NEWLINE INDENT
           "condition" ":" condition_expr
           "severity" ":" severity_level
           "message" ":" STRING
           DEDENT

condition_expr = comparison
               | condition_expr "and" condition_expr
               | condition_expr "or" condition_expr
               | "not" condition_expr
               | call_expr comparison
               | "(" condition_expr ")"

comparison = operand comparator operand

operand = IDENT | FLOAT | INTEGER | call_expr

comparator = ">" | "<" | ">=" | "<=" | "==" | "!="

call_expr = IDENT "(" (expr ("," expr)*)? ")"

severity_level = "info" | "warning" | "critical"

// Action definition
action_def = "action" IDENT ":" NEWLINE INDENT
             "type" ":" action_type
             action_option*
             DEDENT

action_type = "webhook" | "file" | "email" | "mqtt"

action_option = "url" ":" STRING
              | "path" ":" STRING
              | "template" ":" STRING
              | "format" ":" STRING
              | "to" ":" "[" string_list "]"
              | "subject" ":" STRING

// Pipeline definition
pipeline_def = "pipeline" IDENT ":" NEWLINE INDENT
               pipeline_input
               pipeline_stmt+
               DEDENT

pipeline_input = "input" ":" "stream" "from" IDENT

pipeline_stmt = assign_stmt
              | when_stmt
              | emit_stmt

assign_stmt = IDENT "=" pipeline_expr

pipeline_expr = "extract" IDENT "from" IDENT
              | "detect" IDENT "on" IDENT
              | call_expr
              | IDENT

when_stmt = "when" condition_expr ":" NEWLINE INDENT
            trigger_stmt+
            DEDENT

trigger_stmt = "trigger" IDENT

emit_stmt = "emit" IDENT "to" source_uri

// Run statement
run_stmt = "run" IDENT run_mode ("with" run_option)*

run_mode = "continuous" | "once" | "batch"

run_option = "checkpoint" "every" duration
           | "from" STRING
           | "to" STRING

duration = INTEGER ("s" | "m" | "h" | "d")

// Lists
ident_list = IDENT ("," IDENT)*
string_list = STRING ("," STRING)*

// Literals
literal = STRING | INTEGER | FLOAT | BOOL | array | duration

array = "[" (literal ("," literal)*)? "]"

// Tokens
IDENT = /[a-zA-Z_][a-zA-Z0-9_]*/
STRING = /"[^"]*"/ | /\|[^\n]*(\n\s+[^\n]*)*/  // multi-line with |
INTEGER = /-?[0-9]+/
FLOAT = /-?[0-9]+\.[0-9]+/
BOOL = "true" | "false"
```

## Required New Operations

### Plugin: `zrtl_timeseries`

```rust
//! Time series operations for streaming data

/// Compute first-order difference: out[i] = data[i] - data[i-1]
#[no_mangle]
pub extern "C" fn ts_diff(
    data: *const f32,
    output: *mut f32,
    len: u64
);

/// Rolling mean with window size
#[no_mangle]
pub extern "C" fn ts_rolling_mean(
    data: *const f32,
    output: *mut f32,
    len: u64,
    window: u64
);

/// Rolling standard deviation
#[no_mangle]
pub extern "C" fn ts_rolling_std(
    data: *const f32,
    output: *mut f32,
    len: u64,
    window: u64
);

/// Linear regression slope over window
#[no_mangle]
pub extern "C" fn ts_linear_slope(
    data: *const f32,
    len: u64
) -> f32;

/// Exponentially weighted moving average
#[no_mangle]
pub extern "C" fn ts_ewma(
    data: *const f32,
    output: *mut f32,
    len: u64,
    alpha: f32
);

/// Autocorrelation at given lag
#[no_mangle]
pub extern "C" fn ts_autocorrelation(
    data: *const f32,
    len: u64,
    lag: u64
) -> f32;

/// Detect change points using CUSUM
#[no_mangle]
pub extern "C" fn ts_cusum(
    data: *const f32,
    len: u64,
    threshold: f32,
    drift: f32
) -> i64;  // Returns index of change point or -1

/// Seasonal decomposition (simple moving average method)
#[no_mangle]
pub extern "C" fn ts_decompose(
    data: *const f32,
    trend: *mut f32,
    seasonal: *mut f32,
    residual: *mut f32,
    len: u64,
    period: u64
);
```

### Plugin: `zrtl_anomaly`

```rust
//! Anomaly detection algorithms

/// Isolation Forest score for a single point
/// Higher score = more anomalous (0.0 to 1.0)
#[no_mangle]
pub extern "C" fn isolation_forest_score(
    point: *const f32,
    reference_data: *const f32,
    n_samples: u64,
    n_features: u64,
    n_trees: u64
) -> f32;

/// Mahalanobis distance from distribution
#[no_mangle]
pub extern "C" fn mahalanobis_distance(
    point: *const f32,
    mean: *const f32,
    inv_cov: *const f32,  // Inverse covariance matrix (n_features x n_features)
    n_features: u64
) -> f32;

/// Local Outlier Factor score
#[no_mangle]
pub extern "C" fn lof_score(
    point: *const f32,
    reference_data: *const f32,
    n_samples: u64,
    n_features: u64,
    k_neighbors: u64
) -> f32;

/// Z-score for univariate data
#[no_mangle]
pub extern "C" fn zscore(
    value: f32,
    mean: f32,
    std: f32
) -> f32;

/// Multivariate z-score
#[no_mangle]
pub extern "C" fn zscore_multivariate(
    point: *const f32,
    mean: *const f32,
    std: *const f32,
    output: *mut f32,
    n_features: u64
);

/// One-class SVM decision function (pre-trained)
#[no_mangle]
pub extern "C" fn ocsvm_score(
    point: *const f32,
    support_vectors: *const f32,
    alphas: *const f32,
    n_sv: u64,
    n_features: u64,
    gamma: f32,
    rho: f32
) -> f32;
```

### Plugin: `zrtl_stats`

```rust
//! Statistical operations

/// Compute mean
#[no_mangle]
pub extern "C" fn mean_f32(data: *const f32, len: u64) -> f32;

/// Compute variance
#[no_mangle]
pub extern "C" fn var_f32(data: *const f32, len: u64) -> f32;

/// Compute standard deviation
#[no_mangle]
pub extern "C" fn std_f32(data: *const f32, len: u64) -> f32;

/// Compute covariance matrix
#[no_mangle]
pub extern "C" fn covariance_matrix(
    data: *const f32,      // [n_samples, n_features] row-major
    output: *mut f32,      // [n_features, n_features]
    n_samples: u64,
    n_features: u64
);

/// Compute percentile
#[no_mangle]
pub extern "C" fn percentile_f32(
    data: *const f32,
    len: u64,
    p: f32  // 0.0 to 100.0
) -> f32;

/// Compute median
#[no_mangle]
pub extern "C" fn median_f32(data: *const f32, len: u64) -> f32;

/// Compute interquartile range
#[no_mangle]
pub extern "C" fn iqr_f32(data: *const f32, len: u64) -> f32;

/// Online/streaming mean update
#[no_mangle]
pub extern "C" fn streaming_mean_update(
    state: *mut StreamingMeanState,
    value: f32
);

/// Online/streaming variance update (Welford's algorithm)
#[no_mangle]
pub extern "C" fn streaming_var_update(
    state: *mut StreamingVarState,
    value: f32
);
```

### Plugin: `zrtl_window`

```rust
//! Sliding window buffer management

/// Window handle
pub struct WindowHandle {
    buffer: Vec<f32>,
    capacity: usize,
    n_features: usize,
    write_pos: usize,
    count: usize,
}

/// Create a new sliding window
#[no_mangle]
pub extern "C" fn window_create(
    capacity: u64,
    n_features: u64
) -> *mut WindowHandle;

/// Push a sample into the window
#[no_mangle]
pub extern "C" fn window_push(
    handle: *mut WindowHandle,
    sample: *const f32  // [n_features]
);

/// Get the current buffer (returns pointer to internal data)
#[no_mangle]
pub extern "C" fn window_get_buffer(
    handle: *const WindowHandle
) -> *const f32;

/// Get the number of samples in window
#[no_mangle]
pub extern "C" fn window_count(
    handle: *const WindowHandle
) -> u64;

/// Check if window is full
#[no_mangle]
pub extern "C" fn window_is_full(
    handle: *const WindowHandle
) -> bool;

/// Clear the window
#[no_mangle]
pub extern "C" fn window_clear(handle: *mut WindowHandle);

/// Free the window
#[no_mangle]
pub extern "C" fn window_free(handle: *mut WindowHandle);
```

## Dependencies

### Rust Crates

```toml
[dependencies]
# Core
zrtl = { path = "../sdk/zrtl" }
wide = "0.7"

# Async runtime for streaming
tokio = { version = "1.0", features = ["rt-multi-thread", "net", "time"] }

# Data sources
rumqttc = "0.24"              # MQTT client
csv = "1.3"                   # CSV parsing
serde_json = "1.0"            # JSON parsing

# HTTP for webhooks
reqwest = { version = "0.12", features = ["json"] }

# Time handling
chrono = "0.4"

[dev-dependencies]
criterion = "0.5"
tokio-test = "0.4"
```

## File Structure

```
examples/anomaly/
├── Cargo.toml
├── anomaly.zyn                     # Grammar definition
├── src/
│   ├── main.rs                     # CLI entry point
│   ├── runtime.rs                  # Streaming runtime
│   ├── sources.rs                  # Data source implementations
│   └── actions.rs                  # Alert action implementations
├── baselines/
│   ├── normal_operation.json       # Example baseline stats
│   └── model_weights.bin           # Pre-trained anomaly model
├── samples/
│   ├── sensor_monitor.detect       # IoT monitoring
│   ├── server_health.detect        # Server monitoring
│   └── fraud_detection.detect      # Transaction monitoring
└── test_data/
    ├── normal_data.csv
    └── anomaly_data.csv

plugins/zrtl_timeseries/
├── Cargo.toml
├── src/
│   ├── lib.rs
│   ├── diff.rs
│   ├── rolling.rs
│   ├── decompose.rs
│   └── changepoint.rs
└── benches/
    └── timeseries_bench.rs

plugins/zrtl_anomaly/
├── Cargo.toml
├── src/
│   ├── lib.rs
│   ├── isolation_forest.rs
│   ├── mahalanobis.rs
│   ├── lof.rs
│   └── ocsvm.rs
└── tests/
    └── anomaly_tests.rs

plugins/zrtl_stats/
├── Cargo.toml
├── src/
│   ├── lib.rs
│   ├── descriptive.rs
│   ├── streaming.rs
│   └── covariance.rs

plugins/zrtl_window/
├── Cargo.toml
├── src/
│   ├── lib.rs
│   └── circular_buffer.rs
```

## Implementation Plan

### Phase 1: Core Statistics (Week 1)
- [ ] Implement `zrtl_stats` plugin
  - [ ] Basic statistics (mean, var, std)
  - [ ] Percentiles and median
  - [ ] Streaming statistics
- [ ] Implement `zrtl_window` plugin
  - [ ] Circular buffer
  - [ ] Multi-feature windows

### Phase 2: Time Series Operations (Week 1-2)
- [ ] Implement `zrtl_timeseries` plugin
  - [ ] Diff and rolling operations
  - [ ] Linear slope computation
  - [ ] EWMA
  - [ ] Autocorrelation
- [ ] SIMD optimization for rolling operations

### Phase 3: Anomaly Algorithms (Week 2-3)
- [ ] Implement `zrtl_anomaly` plugin
  - [ ] Z-score (univariate and multivariate)
  - [ ] Mahalanobis distance
  - [ ] Isolation Forest scoring
  - [ ] Local Outlier Factor
- [ ] Unit tests with known anomalies

### Phase 4: Grammar and Compilation (Week 3)
- [ ] Write `anomaly.zyn` grammar
- [ ] Implement semantic actions
  - [ ] Feature extraction compilation
  - [ ] Rule compilation
  - [ ] Pipeline orchestration

### Phase 5: Streaming Runtime (Week 3-4)
- [ ] Implement data sources
  - [ ] File streaming (CSV)
  - [ ] MQTT subscription
- [ ] Implement actions
  - [ ] Webhook POST
  - [ ] File logging
- [ ] Event loop and checkpointing

### Phase 6: CLI and Testing (Week 4)
- [ ] Build CLI tool
- [ ] Create example detectors
- [ ] Integration testing
- [ ] Documentation

## Performance Targets

| Operation | Target Latency | Notes |
|-----------|----------------|-------|
| Feature extraction (100 samples, 5 features) | <1ms | SIMD optimized |
| Isolation Forest score (1000 ref samples) | <5ms | Per sample |
| Mahalanobis distance | <0.1ms | Per sample |
| Z-score (5 features) | <0.01ms | Per sample |
| Full pipeline (1 sample) | <10ms | End-to-end |

| Throughput | Target |
|------------|--------|
| Samples processed | >10,000/sec |
| Concurrent streams | >100 |
| Alert latency | <100ms |

## Example Outputs

### Normal Operation

```
$ anomaly run samples/sensor_monitor.detect --source mqtt://localhost/sensors

[2024-01-15 10:23:45] INFO  Connected to MQTT broker
[2024-01-15 10:23:45] INFO  Subscribed to sensors/+
[2024-01-15 10:23:45] INFO  Loading baseline: normal_operation.json
[2024-01-15 10:23:45] INFO  Starting continuous monitoring...

[10:23:46] sensor_01: temp=24.3°C pressure=101.2kPa vibration=0.018 ✓
[10:23:47] sensor_01: temp=24.5°C pressure=101.1kPa vibration=0.019 ✓
[10:23:48] sensor_01: temp=24.4°C pressure=101.3kPa vibration=0.017 ✓
...
```

### Anomaly Detected

```
[10:45:12] sensor_01: temp=24.2°C pressure=101.0kPa vibration=0.021 ✓
[10:45:13] sensor_01: temp=35.8°C pressure=98.2kPa vibration=0.089 ⚠
           WARNING: Temperature 4.2 std devs above normal
[10:45:14] sensor_01: temp=42.1°C pressure=95.1kPa vibration=0.156 🚨
           CRITICAL: Potential equipment failure - high vibration with pressure drop
           → Triggered: slack_alert, email_alert
[10:45:15] sensor_01: temp=48.3°C pressure=91.8kPa vibration=0.234 🚨
           CRITICAL: Extreme temperature deviation detected
           → Triggered: slack_alert, email_alert (cooldown active)
```

## Testing Strategy

### Unit Tests
- Statistical function correctness
- Window buffer edge cases
- Anomaly scoring accuracy

### Integration Tests
- End-to-end pipeline with mock data
- Source connection handling
- Action delivery verification

### Accuracy Tests
- False positive rate measurement
- Detection latency measurement
- Compare with scikit-learn reference

### Stress Tests
- High-frequency data streams
- Many concurrent sensors
- Long-running stability

## Future Enhancements

1. **More Anomaly Algorithms**
   - DBSCAN clustering
   - Autoencoder-based detection
   - Prophet-style forecasting

2. **Advanced Time Series**
   - Fourier transform for periodicity
   - Wavelet decomposition
   - Dynamic time warping

3. **Distributed Processing**
   - Multiple detector instances
   - Shared state via Redis
   - Load balancing

4. **Model Training**
   - Online learning for baselines
   - Adaptive thresholds
   - Feedback loop integration

5. **Visualization**
   - Real-time dashboards
   - Anomaly timeline
   - Feature importance
