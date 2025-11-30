# zrtl_time

Time and duration operations for Zyntax-based languages.

## Overview

Provides current time access, sleep functions, precise timing with instants, and time formatting.

## Exported Symbols

### Current Time

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Time$now_secs` | `() -> i64` | Unix timestamp in seconds |
| `$Time$now_millis` | `() -> i64` | Unix timestamp in milliseconds |
| `$Time$now_micros` | `() -> i64` | Unix timestamp in microseconds |
| `$Time$monotonic_nanos` | `() -> u64` | Monotonic time in nanoseconds (for benchmarking) |

### Sleep

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Time$sleep_secs` | `(u64) -> ()` | Sleep for N seconds |
| `$Time$sleep_millis` | `(u64) -> ()` | Sleep for N milliseconds |
| `$Time$sleep_micros` | `(u64) -> ()` | Sleep for N microseconds |
| `$Time$sleep_nanos` | `(u64) -> ()` | Sleep for N nanoseconds |

### Instant (Precise Timing)

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Time$instant_now` | `() -> u64` | Create instant handle for timing |
| `$Time$instant_elapsed_nanos` | `(u64) -> u64` | Nanoseconds since instant |
| `$Time$instant_elapsed_micros` | `(u64) -> u64` | Microseconds since instant |
| `$Time$instant_elapsed_millis` | `(u64) -> u64` | Milliseconds since instant |
| `$Time$instant_elapsed_secs_f64` | `(u64) -> f64` | Seconds (as float) since instant |
| `$Time$instant_free` | `(u64) -> ()` | Free instant handle |

### Formatting

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Time$format_iso8601` | `(i64) -> StringPtr` | Format Unix timestamp as ISO 8601 |
| `$Time$format_duration_nanos` | `(u64) -> StringPtr` | Format duration human-readable |

## Usage Example

```zig
// Get current Unix timestamp
const now = $Time$now_secs();

// Format as ISO 8601
const formatted = $Time$format_iso8601(now);
// "2024-01-15T12:30:45Z"

// Sleep for a bit
$Time$sleep_millis(100);

// Precise timing for benchmarks
const start = $Time$instant_now();

// ... do work ...

const elapsed = $Time$instant_elapsed_millis(start);
$Time$instant_free(start);

// Format elapsed time
const duration_str = $Time$format_duration_nanos(elapsed * 1_000_000);
// "123.456ms"
```

## Time Types

### Unix Timestamps
- Seconds/milliseconds/microseconds since January 1, 1970 UTC
- Suitable for absolute time representation
- Can be negative for dates before 1970

### Monotonic Time
- Only meaningful for measuring elapsed time
- Guaranteed to never go backwards
- Not related to wall clock time

### Instants
- Opaque handles for precise timing
- Use `instant_now` to capture a point in time
- Use `instant_elapsed_*` to measure duration
- Remember to `instant_free` when done

## Duration Formatting

`format_duration_nanos` returns human-readable strings:
- `"123ns"` for nanoseconds
- `"1.234µs"` for microseconds
- `"45.678ms"` for milliseconds
- `"1.234s"` for seconds

## Dependencies

- `zrtl` - Core ZRTL SDK
