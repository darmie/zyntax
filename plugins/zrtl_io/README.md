# zrtl_io

Standard I/O operations for Zyntax-based languages.

## Overview

Provides console input/output and value formatting using the ZRTL string format `[i32 length][utf8_bytes...]`.

## Exported Symbols

### String Output

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$IO$print` | `(StringPtr) -> ()` | Print string to stdout (no newline) |
| `$IO$println` | `(StringPtr) -> ()` | Print string with newline to stdout |
| `$IO$eprint` | `(StringPtr) -> ()` | Print string to stderr (no newline) |
| `$IO$eprintln` | `(StringPtr) -> ()` | Print string with newline to stderr |

### Primitive Output

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$IO$print_i64` | `(i64) -> ()` | Print integer to stdout |
| `$IO$println_i64` | `(i64) -> ()` | Print integer with newline |
| `$IO$print_u64` | `(u64) -> ()` | Print unsigned integer |
| `$IO$println_u64` | `(u64) -> ()` | Print unsigned integer with newline |
| `$IO$print_f64` | `(f64) -> ()` | Print float to stdout |
| `$IO$println_f64` | `(f64) -> ()` | Print float with newline |
| `$IO$print_bool` | `(bool) -> ()` | Print boolean to stdout |
| `$IO$println_bool` | `(bool) -> ()` | Print boolean with newline |
| `$IO$print_char` | `(u32) -> ()` | Print Unicode codepoint |
| `$IO$println_char` | `(u32) -> ()` | Print Unicode codepoint with newline |

### Flush

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$IO$flush` | `() -> i32` | Flush stdout (0=success, -1=error) |
| `$IO$flush_stderr` | `() -> i32` | Flush stderr (0=success, -1=error) |

### Input

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$IO$read_line` | `() -> StringPtr` | Read line from stdin (caller frees) |
| `$IO$input` | `(StringPtr) -> StringPtr` | Print prompt, read line (caller frees) |

### Formatting

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$IO$format_i64` | `(i64) -> StringPtr` | Format integer as string |
| `$IO$format_u64` | `(u64) -> StringPtr` | Format unsigned integer as string |
| `$IO$format_f64` | `(f64) -> StringPtr` | Format float as string |
| `$IO$format_f64_precision` | `(f64, u32) -> StringPtr` | Format float with decimal precision |
| `$IO$format_bool` | `(bool) -> StringPtr` | Format boolean ("true"/"false") |
| `$IO$format_hex` | `(i64) -> StringPtr` | Format as lowercase hex |
| `$IO$format_hex_upper` | `(i64) -> StringPtr` | Format as uppercase hex |
| `$IO$format_binary` | `(i64) -> StringPtr` | Format as binary |
| `$IO$format_octal` | `(i64) -> StringPtr` | Format as octal |
| `$IO$format_char` | `(u32) -> StringPtr` | Format Unicode codepoint as string |

### Memory Management

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$IO$string_free` | `(StringPtr) -> ()` | Free a string allocated by this module |

## Usage Example

```zig
// Print with newline
const msg = "Hello, World!";
$IO$println(msg);

// Read input with prompt
const name = $IO$input("Enter your name: ");
$IO$print("Hello, ");
$IO$println(name);
$IO$string_free(name);

// Format numbers
const formatted = $IO$format_f64_precision(3.14159, 2);  // "3.14"
$IO$println(formatted);
$IO$string_free(formatted);
```

## Memory Management

All formatting and input functions return newly allocated strings. The caller is responsible for freeing these strings using `$IO$string_free` or the SDK's `string_free` function.

## Dependencies

- `zrtl` - Core ZRTL SDK
