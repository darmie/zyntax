# zrtl_env

Environment variables, command-line arguments, and system information for Zyntax-based languages.

## Overview

Provides access to environment variables, command-line arguments, process utilities, and system information.

## Exported Symbols

### Environment Variables

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Env$get` | `(StringPtr) -> StringPtr` | Get environment variable (null if not found) |
| `$Env$set` | `(StringPtr, StringPtr) -> i32` | Set environment variable (0=success) |
| `$Env$remove` | `(StringPtr) -> ()` | Remove environment variable |
| `$Env$has` | `(StringPtr) -> i32` | Check if variable exists (1=yes, 0=no) |

### Command-line Arguments

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Env$args_count` | `() -> i32` | Get number of command-line arguments |
| `$Env$arg` | `(i32) -> StringPtr` | Get argument at index (null if out of bounds) |
| `$Env$exe_path` | `() -> StringPtr` | Get path to current executable |

### Process

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Env$exit` | `(i32) -> !` | Exit process with status code (never returns) |
| `$Env$pid` | `() -> u32` | Get current process ID |

### Directories

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Env$home_dir` | `() -> StringPtr` | Get user's home directory |
| `$Env$temp_dir` | `() -> StringPtr` | Get system temp directory |
| `$Env$current_dir` | `() -> StringPtr` | Get current working directory |

### System Info

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Env$username` | `() -> StringPtr` | Get current username |
| `$Env$os` | `() -> StringPtr` | Get OS name (macos, linux, windows, etc.) |
| `$Env$arch` | `() -> StringPtr` | Get CPU architecture (x86_64, aarch64, etc.) |

## Usage Example

```zig
// Get environment variable
const path = $Env$get("PATH");
if (path != null) {
    $IO$println(path);
    string_free(path);
}

// Set environment variable
$Env$set("MY_VAR", "my_value");

// Command-line arguments
const argc = $Env$args_count();
for (var i = 0; i < argc; i += 1) {
    const arg = $Env$arg(i);
    $IO$println(arg);
    string_free(arg);
}

// System info
const os = $Env$os();      // "macos", "linux", "windows"
const arch = $Env$arch();  // "x86_64", "aarch64"

// Get home directory
const home = $Env$home_dir();
if (home != null) {
    $IO$println(home);
    string_free(home);
}
```

## Platform Notes

- `$Env$home_dir`: Uses `HOME` on Unix, `USERPROFILE` on Windows
- `$Env$username`: Uses `USER` on Unix, `USERNAME` on Windows
- `$Env$os` returns: "macos", "linux", "windows", "freebsd", "android", "ios", or "unknown"
- `$Env$arch` returns: "x86_64", "aarch64", "x86", "arm", "wasm32", or "unknown"

## Memory Management

All functions returning `StringPtr` allocate new memory. Caller must free using SDK's `string_free`.

## Dependencies

- `zrtl` - Core ZRTL SDK
