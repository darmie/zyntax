# zrtl_thread

Threading, synchronization, and message passing for Zyntax-based languages.

## Overview

Provides thread spawning, atomic operations, mutex support, and MPSC (multi-producer, single-consumer) channels for inter-thread communication. Supports both function pointers and ZRTL closures for thread entry points.

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

### Channel Operations (MPSC)

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Channel$new` | `() -> ChannelPair` | Create unbounded channel |
| `$Channel$bounded` | `(u64) -> ChannelPair` | Create bounded channel with capacity |
| `$Channel$send` | `(u64, i64) -> i32` | Send value (0=ok, -1=disconnected) |
| `$Channel$send_ptr` | `(u64, *u8) -> i32` | Send pointer as i64 |
| `$Channel$recv` | `(u64) -> i64` | Receive (blocking) |
| `$Channel$try_recv` | `(u64) -> i64` | Try receive (non-blocking) |
| `$Channel$try_recv_status` | `(u64) -> TryRecvResult` | Try receive with status |
| `$Channel$recv_timeout` | `(u64, u64) -> i64` | Receive with timeout (ms) |
| `$Channel$recv_timeout_status` | `(u64, u64) -> RecvTimeoutResult` | Receive with timeout and status |
| `$Channel$clone_sender` | `(u64) -> u64` | Clone sender for multiple producers |
| `$Channel$close_sender` | `(u64) -> ()` | Close a sender |
| `$Channel$close_receiver` | `(u64) -> ()` | Close the receiver |

#### Channel Types

```c
struct ChannelPair {
    u64 sender;    // Sender handle
    u64 receiver;  // Receiver handle
}

struct TryRecvResult {
    i64 value;     // Received value (valid if status == 0)
    i32 status;    // 0 = success, 1 = empty, -1 = disconnected
}

struct RecvTimeoutResult {
    i64 value;     // Received value (valid if status == 0)
    i32 status;    // 0 = success, 1 = timeout, -1 = disconnected
}
```

## Usage Examples

### Basic Threading

```zig
// Spawn a thread with function pointer
extern fn worker(arg: i64) i64 {
    return arg * 2;
}

const handle = $Thread$spawn(worker, 21);
const result = $Thread$join(handle);  // result = 42

// Get CPU cores for work distribution
const cores = $Thread$available_parallelism();
```

### Atomics

```zig
// Using atomics for thread-safe counter
const counter = $Atomic$new(0);

// In multiple threads:
$Atomic$add(counter, 1);

// Read final value
const count = $Atomic$load(counter);
$Atomic$free(counter);
```

### Mutex

```zig
// Using mutex for critical sections
const mutex = $Mutex$new();

$Mutex$lock(mutex);
// ... critical section ...
$Mutex$unlock(mutex);

$Mutex$free(mutex);
```

### Channels (Message Passing)

```zig
// Create a channel
const pair = $Channel$new();
const sender = pair.sender;
const receiver = pair.receiver;

// Producer thread
extern fn producer(tx: i64) i64 {
    for (var i = 0; i < 10; i += 1) {
        $Channel$send(tx, i);
    }
    $Channel$close_sender(tx);
    return 10;
}

// Spawn producer
const prod = $Thread$spawn(producer, sender);

// Receive messages in main thread
while (true) {
    const result = $Channel$try_recv_status(receiver);
    if (result.status == 0) {
        // Got value: result.value
        process(result.value);
    } else if (result.status == -1) {
        // Channel disconnected, all done
        break;
    }
    // status == 1 means empty, keep trying
}

$Thread$join(prod);
$Channel$close_receiver(receiver);
```

### Multiple Producers

```zig
const pair = $Channel$new();
const receiver = pair.receiver;

// Clone sender for each producer
const sender1 = pair.sender;
const sender2 = $Channel$clone_sender(sender1);
const sender3 = $Channel$clone_sender(sender1);

// Spawn multiple producers
const t1 = $Thread$spawn(producer_fn, sender1);
const t2 = $Thread$spawn(producer_fn, sender2);
const t3 = $Thread$spawn(producer_fn, sender3);

// Receive from all producers
while (true) {
    const result = $Channel$recv_timeout_status(receiver, 100);
    if (result.status == 0) {
        // Process message
    } else if (result.status == -1) {
        break;  // All senders closed
    }
}

// Wait for all producers
$Thread$join(t1);
$Thread$join(t2);
$Thread$join(t3);
$Channel$close_receiver(receiver);
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

## Channel Semantics

- **MPSC**: Multiple senders, single receiver
- **Unbounded**: `$Channel$new` creates a channel with unlimited buffer
- **Bounded**: `$Channel$bounded(n)` blocks senders when n messages are queued
- **Disconnection**: Closing all senders signals EOF to receiver
- **Ordering**: Messages are received in FIFO order per sender

## Dependencies

- `zrtl` - Core ZRTL SDK (ZrtlClosure, DynamicBox)
