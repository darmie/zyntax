# zrtl_math

Mathematical functions for Zyntax-based languages.

## Overview

Provides trigonometry, exponentials, logarithms, rounding, min/max/clamp, random number generation, and mathematical constants.

## Exported Symbols

### Trigonometry

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Math$sin` | `(f64) -> f64` | Sine of angle in radians |
| `$Math$cos` | `(f64) -> f64` | Cosine of angle in radians |
| `$Math$tan` | `(f64) -> f64` | Tangent of angle in radians |
| `$Math$asin` | `(f64) -> f64` | Arcsine, returns radians |
| `$Math$acos` | `(f64) -> f64` | Arccosine, returns radians |
| `$Math$atan` | `(f64) -> f64` | Arctangent, returns radians |
| `$Math$atan2` | `(f64, f64) -> f64` | Two-argument arctangent (y, x) |
| `$Math$sinh` | `(f64) -> f64` | Hyperbolic sine |
| `$Math$cosh` | `(f64) -> f64` | Hyperbolic cosine |
| `$Math$tanh` | `(f64) -> f64` | Hyperbolic tangent |

### Exponentials & Logarithms

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Math$exp` | `(f64) -> f64` | e^x |
| `$Math$exp2` | `(f64) -> f64` | 2^x |
| `$Math$log` | `(f64) -> f64` | Natural logarithm (base e) |
| `$Math$log2` | `(f64) -> f64` | Base-2 logarithm |
| `$Math$log10` | `(f64) -> f64` | Base-10 logarithm |
| `$Math$pow` | `(f64, f64) -> f64` | x^y (power) |
| `$Math$powi` | `(f64, i32) -> f64` | Integer power (more efficient) |
| `$Math$sqrt` | `(f64) -> f64` | Square root |
| `$Math$cbrt` | `(f64) -> f64` | Cube root |
| `$Math$hypot` | `(f64, f64) -> f64` | Hypotenuse without overflow |

### Rounding

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Math$floor` | `(f64) -> f64` | Round down |
| `$Math$ceil` | `(f64) -> f64` | Round up |
| `$Math$round` | `(f64) -> f64` | Round to nearest (ties away from zero) |
| `$Math$trunc` | `(f64) -> f64` | Truncate toward zero |
| `$Math$fract` | `(f64) -> f64` | Fractional part |
| `$Math$abs` | `(f64) -> f64` | Absolute value (float) |
| `$Math$abs_i64` | `(i64) -> i64` | Absolute value (integer) |
| `$Math$signum` | `(f64) -> f64` | Sign: -1, 0, or 1 |
| `$Math$copysign` | `(f64, f64) -> f64` | Copy sign from y to x |

### Min/Max/Clamp

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Math$min` | `(f64, f64) -> f64` | Minimum of two floats |
| `$Math$max` | `(f64, f64) -> f64` | Maximum of two floats |
| `$Math$clamp` | `(f64, f64, f64) -> f64` | Clamp to [min, max] |
| `$Math$min_i64` | `(i64, i64) -> i64` | Minimum of two integers |
| `$Math$max_i64` | `(i64, i64) -> i64` | Maximum of two integers |
| `$Math$clamp_i64` | `(i64, i64, i64) -> i64` | Clamp integer to [min, max] |

### Random Numbers

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Math$seed` | `(u64) -> ()` | Seed the random generator |
| `$Math$random` | `() -> f64` | Random float in [0, 1) |
| `$Math$random_range` | `(f64, f64) -> f64` | Random float in [min, max) |
| `$Math$random_int` | `(i64, i64) -> i64` | Random integer in [min, max] (inclusive) |
| `$Math$random_bool` | `() -> i32` | Random boolean (0 or 1) |

### Constants

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Math$pi` | `() -> f64` | π (3.14159...) |
| `$Math$e` | `() -> f64` | Euler's number (2.71828...) |
| `$Math$tau` | `() -> f64` | τ = 2π |
| `$Math$sqrt2` | `() -> f64` | √2 |
| `$Math$ln2` | `() -> f64` | ln(2) |
| `$Math$ln10` | `() -> f64` | ln(10) |

### Special Values

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Math$is_nan` | `(f64) -> i32` | Check if NaN |
| `$Math$is_infinite` | `(f64) -> i32` | Check if infinite |
| `$Math$is_finite` | `(f64) -> i32` | Check if finite |
| `$Math$infinity` | `() -> f64` | Positive infinity |
| `$Math$neg_infinity` | `() -> f64` | Negative infinity |
| `$Math$nan` | `() -> f64` | NaN value |

### Utility

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Math$fmod` | `(f64, f64) -> f64` | Floating-point remainder |
| `$Math$lerp` | `(f64, f64, f64) -> f64` | Linear interpolation: a + t*(b-a) |
| `$Math$to_radians` | `(f64) -> f64` | Convert degrees to radians |
| `$Math$to_degrees` | `(f64) -> f64` | Convert radians to degrees |

## Usage Example

```zig
// Trigonometry
const angle = $Math$pi() / 4.0;  // 45 degrees
const sin_val = $Math$sin(angle);
const cos_val = $Math$cos(angle);

// Powers and roots
const squared = $Math$pow(5.0, 2.0);  // 25.0
const root = $Math$sqrt(16.0);        // 4.0

// Rounding
const floor_val = $Math$floor(3.7);   // 3.0
const ceil_val = $Math$ceil(3.2);     // 4.0

// Clamp to range
const clamped = $Math$clamp(15.0, 0.0, 10.0);  // 10.0

// Random numbers
$Math$seed(12345);
const r = $Math$random();               // [0, 1)
const dice = $Math$random_int(1, 6);    // 1-6 inclusive

// Linear interpolation
const midpoint = $Math$lerp(0.0, 100.0, 0.5);  // 50.0

// Angle conversion
const rad = $Math$to_radians(180.0);  // π
const deg = $Math$to_degrees($Math$pi());  // 180.0
```

## Random Number Generator

The RNG uses xorshift64* algorithm seeded from OS entropy (`getrandom`). For reproducible results, call `$Math$seed()` with a fixed value.

## Dependencies

- `zrtl` - Core ZRTL SDK
- `getrandom` - Cryptographically secure initial seeding
