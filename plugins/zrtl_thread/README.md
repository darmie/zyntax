# zrtl_thread

Threading and synchronization primitives for Zyntax-based languages.

## Overview

Provides thread spawning, atomic operations, and mutex support. Supports both function pointers and ZRTL closures for thread entry points.

## Exported Symbols

### Thread Operations

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Thread$spawn` | `(ThreadFn, i64) -> u64` | Spawn thread with function pointer and argument |
| `$Thread$spawn_closure` | `(*ZrtlClosure, i64) -> u64` | Spawn thread with closure (consumes closure) |
| `$Thread$spawn_closure_noarg` | `(*ZrtlClosure) -> u64` | Spawn thread with closure, no argument |
| `$Thread$spawn_closure_cloned` | `(*ZrtlClosure, i64) -> u64` | Spawn thread with cloned closure (reusable) |
| `$Thread$spawn_boxed` | `(*DynamicBox) -> u64` | Spawn thread with boxed closure |
| `$Thread$join` | `(u64) -> i64` | Wait for thread and get return value |
| `$Thread$current_id` | `() -> u64` | Get current thread ID |
| `$Thread$yield_now` | `() -> ()` | Yield execution to other threads |
| `$Thread$park` | `() -> ()` | Park current thread until unparked |
| `$Thread$unpark` | `(u64) -> i32` | Unpark a thread by handle |
| `$Thread$available_parallelism` | `() -> u32` | Get number of CPU cores |

### Atomic Operations (i64)

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Atomic$new` | `(i64) -> u64` | Create new atomic with initial value |
| `$Atomic$load` | `(u64) -> i64` | Load value atomically (SeqCst) |
| `$Atomic$store` | `(u64, i64) -> ()` | Store value atomically (SeqCst) |
| `$Atomic$add` | `(u64, i64) -> i64` | Fetch-add, returns previous value |
| `$Atomic$sub` | `(u64, i64) -> i64` | Fetch-sub, returns previous value |
| `$Atomic$swap` | `(u64, i64) -> i64` | Atomic swap, returns previous value |
| `$Atomic$compare_exchange` | `(u64, i64, i64) -> i32` | CAS: if current==expected, set to new (1=success) |
| `$Atomic$free` | `(u64) -> ()` | Free an atomic |

### Mutex Operations

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Mutex$new` | `() -> u64` | Create a new mutex |
| `$Mutex$lock` | `(u64) -> i32` | Lock mutex (0=success, -1=error) |
| `$Mutex$try_lock` | `(u64) -> i32` | Try lock (1=acquired, 0=would block, -1=error) |
| `$Mutex$unlock` | `(u64) -> i32` | Unlock mutex |
| `$Mutex$free` | `(u64) -> ()` | Free a mutex |

## Usage Example

```zig
// Spawn a thread with function pointer
extern fn worker(arg: i64) i64 {
    return arg * 2;
}

const handle = $Thread$spawn(worker, 21);
const result = $Thread$join(handle);  // result = 42

// Using atomics for thread-safe counter
const counter = $Atomic$new(0);

// In multiple threads:
$Atomic$add(counter, 1);

// Read final value
const count = $Atomic$load(counter);
$Atomic$free(counter);

// Using mutex for critical sections
const mutex = $Mutex$new();

$Mutex$lock(mutex);
// ... critical section ...
$Mutex$unlock(mutex);

$Mutex$free(mutex);

// Get CPU cores for work distribution
const cores = $Thread$available_parallelism();
```

## Thread Types

### Function Pointer
```c
typedef i64 (*ThreadFn)(i64);
```
Basic function pointer that takes an i64 argument and returns i64.

### Closure
Use `ZrtlClosure` for closures with captured state. The `spawn_closure` variants consume the closure, while `spawn_closure_cloned` clones it so the original can be reused.

## Memory Ordering

All atomic operations use `SeqCst` (sequential consistency) ordering for maximum safety. This provides strong guarantees but may have some performance overhead compared to weaker orderings.

## Dependencies

- `zrtl` - Core ZRTL SDK (ZrtlClosure, DynamicBox)
