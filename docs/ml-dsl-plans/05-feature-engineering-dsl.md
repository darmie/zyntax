# Feature Engineering DSL

**Priority**: Medium
**Complexity**: Medium
**Estimated New Code**: ~2000 LOC

## Overview

A domain-specific language for declarative feature computation in ML pipelines. Enables real-time feature extraction, transformation, and materialization for production ML systems.

## Use Cases

1. **Real-Time ML Features**
   - Recommendation systems
   - Fraud detection
   - Personalization

2. **Feature Stores**
   - Offline feature computation
   - Feature versioning
   - Point-in-time correctness

3. **Data Preprocessing**
   - ETL pipelines
   - Data validation
   - Schema enforcement

4. **Online/Offline Feature Sync**
   - Training/serving consistency
   - Feature drift detection

## Syntax Design

```
// features.feat - Feature Engineering DSL

// Import raw data sources
source transactions from postgres://db/transactions {
    columns: [user_id, merchant_id, amount, timestamp, category],
    timestamp_column: timestamp
}

source user_profiles from redis://cache/users {
    key_pattern: "user:{user_id}",
    fields: [age, signup_date, country, tier]
}

source item_embeddings from file://models/item_embeddings.npy {
    shape: [100000, 128],
    index_column: item_id
}

// Define entity (the thing we're computing features for)
entity User {
    key: user_id,
    description: "End user of the platform"
}

entity Transaction {
    key: transaction_id,
    timestamp: timestamp,
    description: "A single transaction"
}

// Feature group: user behavioral features
feature_group user_behavior for User:
    source: transactions

    // Time-windowed aggregations
    feature transaction_count_1d:
        aggregate: count(*)
        window: 1 day
        description: "Number of transactions in last 24 hours"

    feature transaction_count_7d:
        aggregate: count(*)
        window: 7 days

    feature transaction_count_30d:
        aggregate: count(*)
        window: 30 days

    feature total_spend_1d:
        aggregate: sum(amount)
        window: 1 day

    feature total_spend_7d:
        aggregate: sum(amount)
        window: 7 days

    feature avg_transaction_amount_30d:
        aggregate: avg(amount)
        window: 30 days

    feature max_transaction_amount_7d:
        aggregate: max(amount)
        window: 7 days

    // Derived features
    feature spend_velocity:
        expression: total_spend_1d / (total_spend_7d / 7 + 0.01)
        description: "Ratio of daily spend to weekly average"

    feature is_high_spender:
        expression: total_spend_30d > 1000
        dtype: bool

// Feature group: transaction context
feature_group transaction_context for Transaction:
    source: transactions

    // Same-entity features (from user)
    feature user_avg_amount:
        aggregate: avg(amount)
        window: 30 days
        group_by: user_id

    feature amount_vs_user_avg:
        expression: amount / (user_avg_amount + 0.01)
        description: "Transaction amount relative to user's average"

    // Time-based features
    feature hour_of_day:
        expression: extract_hour(timestamp)
        dtype: int

    feature day_of_week:
        expression: extract_dayofweek(timestamp)
        dtype: int

    feature is_weekend:
        expression: day_of_week >= 5
        dtype: bool

    // Categorical encoding
    feature category_onehot:
        expression: onehot(category, vocab=["food", "travel", "shopping", "entertainment", "other"])
        dtype: vector[5]

    feature merchant_frequency:
        aggregate: count(*)
        window: 30 days
        group_by: merchant_id

// Feature group: embedding-based features
feature_group embedding_features for User:
    source: transactions, item_embeddings

    // User embedding from interaction history
    feature user_embedding:
        expression: mean(
            lookup(item_embeddings, item_id)
            for item_id in recent_items(user_id, limit=50)
        )
        dtype: vector[128]
        description: "User embedding from recent item interactions"

    # Similarity to category centroids
    feature category_affinity:
        expression: cosine_similarity(user_embedding, category_centroids)
        dtype: vector[10]

// Feature transformations
transform standardize:
    method: zscore
    params: {clip: 3.0}

transform log_transform:
    method: log1p

transform bucketize:
    method: quantile
    params: {n_buckets: 10}

// Apply transformations
transformed_features user_features_normalized:
    base: user_behavior
    apply:
        - standardize to [transaction_count_*, total_spend_*]
        - log_transform to [total_spend_*]
        - bucketize to [avg_transaction_amount_30d]

// Feature validation
validate user_behavior:
    rules:
        - transaction_count_* >= 0
        - total_spend_* >= 0
        - spend_velocity between 0 and 100
        - not null for [transaction_count_1d, total_spend_1d]

// Materialization targets
materialize user_behavior:
    to: redis://features/user/{user_id}
    ttl: 1 hour
    refresh: every 5 minutes

materialize transaction_context:
    to: postgres://features/transaction_features
    mode: append
    partition_by: date(timestamp)

// Export for training
export training_data:
    features: [user_behavior, transaction_context]
    labels: from postgres://db/labels
    format: parquet
    output: "s3://bucket/training_data/"
    time_range: last 90 days

// Serve features online
serve online_features:
    features: [user_behavior, embedding_features]
    endpoint: grpc://0.0.0.0:50051
    cache: redis://cache/features
    batch_size: 100
    timeout_ms: 50
```

## Grammar Specification

```
// features.zyn

@name = "features"
@version = "1.0"
@file_extensions = [".feat", ".features"]

@builtins {
    // Aggregations (from zrtl_stats)
    agg_count: "$Stats$count"
    agg_sum: "$Stats$sum_f32"
    agg_avg: "$Stats$mean_f32"
    agg_min: "$SIMD$min_f32"
    agg_max: "$SIMD$max_f32"
    agg_std: "$Stats$std_f32"
    agg_var: "$Stats$var_f32"
    agg_median: "$Stats$median_f32"
    agg_percentile: "$Stats$percentile_f32"

    // Vector operations
    vec_mean: "$Stats$mean_f32"
    vec_sum: "$SIMD$sum_f32"
    vec_dot: "$SIMD$dot_product_f32"
    vec_normalize: "$VecSearch$l2_normalize"
    cosine_sim: "$VecSearch$cosine_similarity"

    // Transformations
    zscore: "$Transform$zscore"
    minmax_scale: "$Transform$minmax"
    log1p: "$Transform$log1p"
    quantile_bucket: "$Transform$quantile_buckets"
    onehot_encode: "$Transform$onehot"

    // Time functions
    extract_hour: "$Time$extract_hour"
    extract_day: "$Time$extract_day"
    extract_dayofweek: "$Time$extract_dayofweek"
    extract_month: "$Time$extract_month"
    time_since: "$Time$seconds_since"

    // Lookup operations
    embedding_lookup: "$Tensor$lookup"

    // IO
    redis_get: "$IO$redis_get"
    redis_set: "$IO$redis_set"
    postgres_query: "$IO$postgres_query"
    print: "$IO$println"
}

// Grammar rules
program = (source_def | entity_def | feature_group_def | transform_def |
           transformed_features_def | validate_def | materialize_def |
           export_def | serve_def)*

// Data source definition
source_def = "source" IDENT "from" source_uri source_config

source_uri = /[a-z]+:\/\/[^\s{]+/

source_config = "{" source_option* "}"

source_option = "columns" ":" "[" ident_list "]"
              | "timestamp_column" ":" IDENT
              | "key_pattern" ":" STRING
              | "fields" ":" "[" ident_list "]"
              | "shape" ":" "[" int_list "]"
              | "index_column" ":" IDENT

// Entity definition
entity_def = "entity" IDENT "{" entity_option* "}"

entity_option = "key" ":" IDENT
              | "timestamp" ":" IDENT
              | "description" ":" STRING

// Feature group
feature_group_def = "feature_group" IDENT "for" IDENT ":" NEWLINE INDENT
                    "source" ":" ident_list
                    feature_def*
                    DEDENT

feature_def = "feature" IDENT ":" NEWLINE INDENT
              feature_option+
              DEDENT

feature_option = "aggregate" ":" aggregate_expr
               | "expression" ":" expression
               | "window" ":" duration
               | "group_by" ":" IDENT
               | "dtype" ":" dtype
               | "description" ":" STRING
               | "default" ":" literal

aggregate_expr = agg_func "(" (IDENT | "*") ")"

agg_func = "count" | "sum" | "avg" | "min" | "max" | "std" | "var"
         | "median" | "percentile"

// Feature expressions
expression = binary_expr
           | unary_expr
           | call_expr
           | IDENT
           | literal
           | "(" expression ")"

binary_expr = expression binary_op expression

binary_op = "+" | "-" | "*" | "/" | ">" | "<" | ">=" | "<=" | "==" | "!="
          | "and" | "or" | "between"

unary_expr = "not" expression
           | "-" expression

call_expr = func_name "(" (call_arg ("," call_arg)*)? ")"

func_name = IDENT

call_arg = expression
         | IDENT "=" expression

// Transform definition
transform_def = "transform" IDENT ":" NEWLINE INDENT
                "method" ":" transform_method
                ("params" ":" "{" config_pair* "}")?
                DEDENT

transform_method = "zscore" | "minmax" | "log1p" | "log" | "sqrt"
                 | "quantile" | "onehot" | "label_encode"

// Transformed features
transformed_features_def = "transformed_features" IDENT ":" NEWLINE INDENT
                           "base" ":" IDENT
                           "apply" ":" NEWLINE INDENT
                           transform_apply+
                           DEDENT DEDENT

transform_apply = "-" IDENT "to" "[" feature_pattern_list "]"

feature_pattern_list = feature_pattern ("," feature_pattern)*

feature_pattern = IDENT | /[a-zA-Z_][a-zA-Z0-9_]*\*/  // glob pattern

// Validation
validate_def = "validate" IDENT ":" NEWLINE INDENT
               "rules" ":" NEWLINE INDENT
               validation_rule+
               DEDENT DEDENT

validation_rule = "-" validation_expr

validation_expr = feature_pattern comparator literal
                | feature_pattern "between" literal "and" literal
                | "not" "null" "for" "[" ident_list "]"
                | feature_pattern "in" "[" literal_list "]"

comparator = ">=" | "<=" | ">" | "<" | "==" | "!="

// Materialization
materialize_def = "materialize" IDENT ":" NEWLINE INDENT
                  materialize_option+
                  DEDENT

materialize_option = "to" ":" source_uri
                   | "ttl" ":" duration
                   | "refresh" ":" "every" duration
                   | "mode" ":" ("overwrite" | "append" | "upsert")
                   | "partition_by" ":" expression

// Export
export_def = "export" IDENT ":" NEWLINE INDENT
             export_option+
             DEDENT

export_option = "features" ":" "[" ident_list "]"
              | "labels" ":" "from" source_uri
              | "format" ":" ("parquet" | "csv" | "tfrecord")
              | "output" ":" STRING
              | "time_range" ":" time_range

time_range = "last" INTEGER ("days" | "hours" | "minutes")
           | "from" STRING "to" STRING

// Serve
serve_def = "serve" IDENT ":" NEWLINE INDENT
            serve_option+
            DEDENT

serve_option = "features" ":" "[" ident_list "]"
             | "endpoint" ":" source_uri
             | "cache" ":" source_uri
             | "batch_size" ":" INTEGER
             | "timeout_ms" ":" INTEGER

// Types
dtype = "int" | "float" | "bool" | "string" | "vector" "[" INTEGER "]"
      | "timestamp" | "date"

duration = INTEGER ("second" "s"? | "minute" "s"? | "hour" "s"? | "day" "s"?)

// Lists
ident_list = IDENT ("," IDENT)*
int_list = INTEGER ("," INTEGER)*
literal_list = literal ("," literal)*

// Literals
literal = STRING | INTEGER | FLOAT | BOOL | "null"

config_pair = IDENT ":" literal

// Tokens
IDENT = /[a-zA-Z_][a-zA-Z0-9_]*/
STRING = /"[^"]*"/
INTEGER = /-?[0-9]+/
FLOAT = /-?[0-9]+\.[0-9]+/
BOOL = "true" | "false"
```

## Required New Operations

### Plugin: `zrtl_transform`

```rust
//! Feature transformation operations

/// Z-score normalization: (x - mean) / std
/// Optionally clip to range [-clip, clip]
#[no_mangle]
pub extern "C" fn zscore_transform(
    data: *mut f32,
    len: u64,
    mean: f32,
    std: f32,
    clip: f32  // 0.0 for no clipping
);

/// Min-max scaling: (x - min) / (max - min)
#[no_mangle]
pub extern "C" fn minmax_transform(
    data: *mut f32,
    len: u64,
    min: f32,
    max: f32
);

/// Log1p transform: log(1 + x)
#[no_mangle]
pub extern "C" fn log1p_transform(data: *mut f32, len: u64);

/// Quantile bucketing
/// Returns bucket indices (0 to n_buckets-1)
#[no_mangle]
pub extern "C" fn quantile_buckets(
    data: *const f32,
    output: *mut u32,
    len: u64,
    boundaries: *const f32,  // [n_buckets - 1] threshold values
    n_buckets: u32
);

/// One-hot encoding
/// vocab_size is the vocabulary size
/// Returns: [len * vocab_size] one-hot vectors
#[no_mangle]
pub extern "C" fn onehot_encode(
    indices: *const u32,
    output: *mut f32,
    len: u64,
    vocab_size: u32
);

/// Label encoding (string to int)
/// vocab is a null-terminated array of null-terminated strings
#[no_mangle]
pub extern "C" fn label_encode(
    input: *const *const u8,  // Array of string pointers
    output: *mut u32,
    len: u64,
    vocab: *const *const u8,
    vocab_size: u32
) -> i32;  // Returns -1 if unknown value found

/// Target encoding (category -> mean target value)
#[no_mangle]
pub extern "C" fn target_encode(
    categories: *const u32,
    output: *mut f32,
    len: u64,
    category_means: *const f32,  // Pre-computed means per category
    n_categories: u32,
    global_mean: f32  // Fallback for unknown categories
);
```

### Plugin: `zrtl_agg` (Extended Aggregations)

```rust
//! Extended aggregation operations for feature engineering

/// Count distinct values
#[no_mangle]
pub extern "C" fn count_distinct_u32(
    data: *const u32,
    len: u64
) -> u64;

/// Compute mode (most frequent value)
#[no_mangle]
pub extern "C" fn mode_u32(
    data: *const u32,
    len: u64
) -> u32;

/// Windowed aggregation with timestamps
/// Returns aggregated value for data points within [timestamp - window, timestamp]
#[no_mangle]
pub extern "C" fn windowed_sum(
    values: *const f32,
    timestamps: *const i64,
    len: u64,
    query_timestamp: i64,
    window_seconds: i64
) -> f32;

/// Group-by aggregation
/// groups: [len] group indices
/// values: [len] values to aggregate
/// output: [n_groups] aggregated values
#[no_mangle]
pub extern "C" fn groupby_sum(
    groups: *const u32,
    values: *const f32,
    output: *mut f32,
    len: u64,
    n_groups: u32
);

#[no_mangle]
pub extern "C" fn groupby_mean(
    groups: *const u32,
    values: *const f32,
    output: *mut f32,
    len: u64,
    n_groups: u32
);

#[no_mangle]
pub extern "C" fn groupby_count(
    groups: *const u32,
    output: *mut u64,
    len: u64,
    n_groups: u32
);

/// Rolling aggregation
#[no_mangle]
pub extern "C" fn rolling_sum(
    data: *const f32,
    output: *mut f32,
    len: u64,
    window: u64
);

#[no_mangle]
pub extern "C" fn rolling_mean(
    data: *const f32,
    output: *mut f32,
    len: u64,
    window: u64
);
```

### Plugin: `zrtl_time` (Time Operations)

```rust
//! Time/date extraction and manipulation

/// Extract hour from Unix timestamp (0-23)
#[no_mangle]
pub extern "C" fn extract_hour(timestamp: i64) -> u32;

/// Extract day of month (1-31)
#[no_mangle]
pub extern "C" fn extract_day(timestamp: i64) -> u32;

/// Extract day of week (0=Monday, 6=Sunday)
#[no_mangle]
pub extern "C" fn extract_dayofweek(timestamp: i64) -> u32;

/// Extract month (1-12)
#[no_mangle]
pub extern "C" fn extract_month(timestamp: i64) -> u32;

/// Extract year
#[no_mangle]
pub extern "C" fn extract_year(timestamp: i64) -> i32;

/// Seconds since reference timestamp
#[no_mangle]
pub extern "C" fn seconds_since(timestamp: i64, reference: i64) -> i64;

/// Days since reference timestamp
#[no_mangle]
pub extern "C" fn days_since(timestamp: i64, reference: i64) -> i32;

/// Check if timestamp is weekend
#[no_mangle]
pub extern "C" fn is_weekend(timestamp: i64) -> bool;

/// Truncate to day boundary
#[no_mangle]
pub extern "C" fn truncate_day(timestamp: i64) -> i64;

/// Truncate to hour boundary
#[no_mangle]
pub extern "C" fn truncate_hour(timestamp: i64) -> i64;

/// Batch extraction for efficiency
#[no_mangle]
pub extern "C" fn extract_hour_batch(
    timestamps: *const i64,
    output: *mut u32,
    len: u64
);
```

## Dependencies

### Rust Crates

```toml
[dependencies]
# Core
zrtl = { path = "../sdk/zrtl" }
wide = "0.7"

# Async runtime
tokio = { version = "1.0", features = ["rt-multi-thread", "net", "time", "sync"] }

# Data sources
redis = { version = "0.25", features = ["tokio-comp"] }
tokio-postgres = "0.7"
sqlx = { version = "0.7", features = ["postgres", "runtime-tokio"] }

# File formats
parquet = "52"
arrow = "52"
csv = "1.3"

# Serialization
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"

# Time
chrono = "0.4"

# HTTP/gRPC for serving
tonic = "0.11"
axum = "0.7"

[dev-dependencies]
criterion = "0.5"
tokio-test = "0.4"
```

## File Structure

```
examples/features/
├── Cargo.toml
├── features.zyn                    # Grammar definition
├── src/
│   ├── main.rs                     # CLI entry point
│   ├── compiler.rs                 # Feature definition compiler
│   ├── executor.rs                 # Feature computation engine
│   ├── sources/
│   │   ├── mod.rs
│   │   ├── postgres.rs
│   │   ├── redis.rs
│   │   └── file.rs
│   ├── sinks/
│   │   ├── mod.rs
│   │   ├── redis.rs
│   │   ├── postgres.rs
│   │   └── parquet.rs
│   └── server/
│       ├── mod.rs
│       └── grpc.rs
├── samples/
│   ├── user_features.feat          # User behavioral features
│   ├── transaction_features.feat   # Transaction features
│   └── recommendation.feat         # Recommendation features
└── test_data/
    ├── sample_transactions.csv
    └── sample_users.json

plugins/zrtl_transform/
├── Cargo.toml
├── src/
│   ├── lib.rs
│   ├── normalize.rs
│   ├── encode.rs
│   └── bucket.rs
└── tests/
    └── transform_tests.rs

plugins/zrtl_agg/
├── Cargo.toml
├── src/
│   ├── lib.rs
│   ├── window.rs
│   ├── groupby.rs
│   └── rolling.rs

plugins/zrtl_time/
├── Cargo.toml
├── src/
│   ├── lib.rs
│   └── extract.rs
```

## Implementation Plan

### Phase 1: Core Transformations (Week 1)
- [ ] Implement `zrtl_transform` plugin
  - [ ] Z-score normalization
  - [ ] Min-max scaling
  - [ ] Log transforms
  - [ ] Quantile bucketing
  - [ ] One-hot encoding
- [ ] Unit tests

### Phase 2: Aggregations (Week 1-2)
- [ ] Implement `zrtl_agg` plugin
  - [ ] Windowed aggregations
  - [ ] Group-by aggregations
  - [ ] Rolling aggregations
- [ ] SIMD optimization

### Phase 3: Time Operations (Week 2)
- [ ] Implement `zrtl_time` plugin
  - [ ] Time extraction functions
  - [ ] Duration computations
  - [ ] Batch operations

### Phase 4: Grammar and Compiler (Week 2-3)
- [ ] Write grammar
- [ ] Implement feature definition parser
- [ ] Build computation graph
- [ ] Expression compiler

### Phase 5: Data Sources/Sinks (Week 3)
- [ ] Redis integration
- [ ] PostgreSQL integration
- [ ] Parquet export
- [ ] Connection pooling

### Phase 6: Serving Layer (Week 4)
- [ ] gRPC feature server
- [ ] Caching layer
- [ ] Batch request handling
- [ ] Documentation

## Performance Targets

| Operation | Target Latency | Notes |
|-----------|----------------|-------|
| Single feature lookup | <5ms | From Redis cache |
| Feature batch (100 entities) | <50ms | With Redis |
| Windowed aggregation (1K rows) | <1ms | In-memory |
| Group-by aggregation (100K rows) | <10ms | SIMD optimized |
| Transform pipeline (1K features) | <0.5ms | |

| Throughput | Target |
|------------|--------|
| Feature requests/sec | >10,000 |
| Materialization rows/sec | >100,000 |

## Example Outputs

### Feature Computation

```bash
$ features compute user_behavior --entity user_123

Computing features for user_123...

Source: transactions (querying last 30 days)
  Found 47 transactions

Features:
  transaction_count_1d: 3
  transaction_count_7d: 12
  transaction_count_30d: 47
  total_spend_1d: 125.50
  total_spend_7d: 489.23
  total_spend_30d: 1847.92
  avg_transaction_amount_30d: 39.32
  max_transaction_amount_7d: 156.00
  spend_velocity: 1.79
  is_high_spender: true

Computation time: 12.3ms
Cache: written to redis://features/user/user_123 (TTL: 1h)
```

### Batch Materialization

```bash
$ features materialize user_behavior --time-range "last 24 hours"

Materializing user_behavior features...

Source: transactions
  Time range: 2024-01-14 00:00:00 to 2024-01-15 00:00:00
  Active users: 15,432

Progress:
████████████████████████████████████████ 15432/15432

Summary:
  - Features computed: 15,432 users x 10 features
  - Total time: 45.2s
  - Throughput: 341 users/sec
  - Written to: redis://features/user/*
  - Validation: 0 errors, 23 warnings (null values)
```

### Online Serving

```bash
$ features serve online_features --port 50051

Starting feature server on grpc://0.0.0.0:50051

Configuration:
  - Features: user_behavior, embedding_features
  - Cache: redis://cache/features
  - Batch size: 100
  - Timeout: 50ms

Server ready. Listening for requests...

[10:23:45] Request: GetFeatures(user_ids=[u1, u2, u3])
           Response: 3 feature vectors in 4.2ms (cache hit: 2/3)
[10:23:46] Request: GetFeatures(user_ids=[u4, u5, ...u100])
           Response: 100 feature vectors in 23.1ms (cache hit: 87/100)
```

## Testing Strategy

### Unit Tests
- Transform correctness
- Aggregation accuracy
- Time extraction edge cases

### Integration Tests
- End-to-end feature computation
- Source/sink connectivity
- Cache consistency

### Performance Tests
- Latency benchmarks
- Throughput under load
- Memory usage

### Data Quality Tests
- Validation rule enforcement
- Null handling
- Type coercion

## Future Enhancements

1. **Point-in-Time Correctness**
   - Time travel queries
   - Historical feature values
   - Training data generation

2. **Feature Drift Detection**
   - Statistical monitoring
   - Distribution comparison
   - Alerting

3. **Feature Lineage**
   - Dependency tracking
   - Impact analysis
   - Data catalog integration

4. **Advanced Transformations**
   - PCA/dimensionality reduction
   - Embedding generation
   - AutoML feature selection

5. **Distributed Computation**
   - Spark integration
   - Partitioned processing
   - Incremental updates
