# zrtl_process

Subprocess execution and management for Zyntax-based languages.

## Overview

Provides subprocess spawning, output capture, stdin/stdout handling, and process management. Supports both quick execution and handle-based process control.

## Exported Symbols

### Quick Execution

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Process$run` | `(StringPtr, ArrayPtr) -> i32` | Run command, return exit code |
| `$Process$run_capture` | `(StringPtr, ArrayPtr) -> StringPtr` | Run and capture stdout |
| `$Process$shell` | `(StringPtr) -> i32` | Run shell command, return exit code |
| `$Process$shell_capture` | `(StringPtr) -> StringPtr` | Run shell command, capture stdout |

### Process Spawning

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Process$spawn` | `(StringPtr, ArrayPtr) -> u64` | Spawn process (0 on error) |
| `$Process$wait` | `(u64) -> i32` | Wait for process, get exit code |
| `$Process$is_running` | `(u64) -> i32` | Check if still running |
| `$Process$kill` | `(u64) -> i32` | Kill process (1=success) |
| `$Process$id` | `(u64) -> u32` | Get process ID |

### I/O

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Process$read_stdout` | `(u64) -> StringPtr` | Read stdout from spawned process |
| `$Process$read_stderr` | `(u64) -> StringPtr` | Read stderr from spawned process |
| `$Process$write_stdin` | `(u64, StringPtr) -> i32` | Write to stdin (returns bytes written) |
| `$Process$close_stdin` | `(u64) -> ()` | Close stdin (signals EOF) |

### Utility

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Process$current_pid` | `() -> u32` | Get current process ID |
| `$Process$command_exists` | `(StringPtr) -> i32` | Check if command exists in PATH |

## Usage Example

```zig
// Quick shell command
const exit_code = $Process$shell("echo hello");

// Capture output
const output = $Process$shell_capture("ls -la");
$IO$println(output);
string_free(output);

// Run with arguments
const args = array_new<StringPtr>(2);
array_push(args, "-l");
array_push(args, "-a");
const code = $Process$run("ls", args);

// Capture with arguments
const result = $Process$run_capture("echo", args);

// Check if command exists
if ($Process$command_exists("git") == 1) {
    // git is installed
}

// Spawn and control a process
const handle = $Process$spawn("cat", null);  // cat waits for input
if (handle != 0) {
    // Write to stdin
    $Process$write_stdin(handle, "Hello\n");
    $Process$close_stdin(handle);  // Signal EOF

    // Wait and read output
    const exit_code = $Process$wait(handle);
    const stdout = $Process$read_stdout(handle);

    $IO$println(stdout);
    string_free(stdout);
}

// Long-running process
const server = $Process$spawn("python", ["-m", "http.server"]);
if (server != 0) {
    // Check if still running
    if ($Process$is_running(server) == 1) {
        // Do something...
    }

    // Kill when done
    $Process$kill(server);
    $Process$wait(server);  // Clean up
}

// Get current process ID
const my_pid = $Process$current_pid();
```

## Shell Commands

The `$Process$shell` and `$Process$shell_capture` functions use:
- `/bin/sh -c` on Unix (macOS, Linux)
- `cmd /C` on Windows

## Return Values

- `$Process$run` / `$Process$shell`: Exit code, or -1 on error
- `$Process$spawn`: Process handle, or 0 on error
- `$Process$wait`: Exit code, or -1 on error
- `$Process$kill`: 1 on success, 0 on failure
- `$Process$write_stdin`: Bytes written, or -1 on error

## Process Handle Lifecycle

1. `$Process$spawn` creates a handle
2. Optionally write to stdin with `$Process$write_stdin`
3. Close stdin with `$Process$close_stdin` if needed
4. Wait for completion with `$Process$wait`
5. Read output with `$Process$read_stdout` / `$Process$read_stderr`

Note: `$Process$wait` consumes the handle - don't use it after waiting.

## Memory Management

Functions returning `StringPtr` allocate memory. Caller must free with `string_free`.

## Dependencies

- `zrtl` - Core ZRTL SDK
