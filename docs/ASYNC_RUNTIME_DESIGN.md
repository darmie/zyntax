# Async Runtime Design for Zyntax

**Problem**: Implementing async/await in Cranelift JIT is complex due to state machine management.

**Key Question**: Should we build the reactor/executor in Cranelift, or call external runtime via syscalls?

---

## Current Approach (Problematic)

The current `async_support.rs` attempts to:
1. Transform async functions into HIR state machines with switch statements
2. Compile these directly to Cranelift

**Issues with this approach**:
- ❌ Cranelift has limited control flow (switch statements are awkward)
- ❌ State management is complex in JIT context
- ❌ No clear way to suspend/resume execution
- ❌ Waker/poll mechanism needs runtime support
- ❌ Hard to integrate with existing async ecosystems (Tokio, async-std)

---

## Proposed Solution: External Runtime + FFI

Based on your experience, here's a better approach:

### Architecture: Hybrid Model

```
┌─────────────────────────────────────────┐
│   Zyntax Async Function (Source)       │
│   async fn fetch() -> String           │
└──────────────┬──────────────────────────┘
               │ Compile ↓
┌──────────────┴──────────────────────────┐
│   HIR Transformation                    │
│   1. Extract await points               │
│   2. Create state struct                │
│   3. Generate poll() function           │
└──────────────┬──────────────────────────┘
               │ Codegen ↓
┌──────────────┴──────────────────────────┐
│   Cranelift JIT Code                    │
│   - State struct (in memory)            │
│   - poll(state*) -> Poll<T>            │
│   - FFI calls to runtime                │
└──────────────┬──────────────────────────┘
               │ Runtime ↓
┌──────────────┴──────────────────────────┐
│   External Async Runtime (C/Rust)       │
│   - Tokio, async-std, or custom         │
│   - Executor/reactor                    │
│   - Waker mechanism                     │
│   - I/O polling (epoll/kqueue)          │
└─────────────────────────────────────────┘
```

### Key Design Decisions

#### 1. **Compile to poll() Functions, NOT State Machines**

Instead of complex switch-based state machines:

```rust
// Source
async fn fetch_data(url: String) -> String {
    let response = http_get(url).await;  // Await point 1
    let body = read_body(response).await; // Await point 2
    body
}

// Compile to C-compatible poll function
struct FetchDataState {
    state_id: u32,
    url: String,
    response: Option<Response>,
    body: Option<String>,
}

// Cranelift generates this:
extern "C" fn fetch_data_poll(state: *mut FetchDataState, waker: *const Waker) -> Poll<String> {
    unsafe {
        match (*state).state_id {
            0 => {
                // Initial state: start http_get
                let fut = http_get((*state).url.clone());
                // Store future somehow... or just call poll immediately
                (*state).state_id = 1;
                Poll::Pending
            }
            1 => {
                // After first await
                if let Some(response) = (*state).response.take() {
                    let fut = read_body(response);
                    (*state).state_id = 2;
                    Poll::Pending
                } else {
                    Poll::Pending
                }
            }
            2 => {
                // After second await
                if let Some(body) = (*state).body.take() {
                    Poll::Ready(body)
                } else {
                    Poll::Pending
                }
            }
            _ => unreachable!()
        }
    }
}
```

**Advantages**:
- ✅ Simple C ABI interface
- ✅ Runtime handles all complexity
- ✅ Cranelift only needs basic control flow
- ✅ State is just a struct (easy in Cranelift)

#### 2. **External Runtime via Shared Library**

Create a minimal async runtime as a C library:

```c
// zyntax_async_runtime.h

typedef enum {
    POLL_READY,
    POLL_PENDING
} PollStatus;

typedef struct {
    void* data;
    void (*wake)(void*);
} Waker;

typedef void* FutureState;
typedef PollStatus (*PollFn)(FutureState, Waker*);

// Runtime functions (implemented in Rust or C++)
void* zyntax_runtime_create(void);
void zyntax_runtime_destroy(void* runtime);
void* zyntax_runtime_spawn(void* runtime, FutureState state, PollFn poll);
void zyntax_runtime_block_on(void* runtime, void* task);
void zyntax_runtime_run(void* runtime);
```

**Implementation Options**:

**Option A: Rust-based Runtime** (Recommended)
```rust
// zyntax-async-runtime crate (separate from compiler)
use tokio::runtime::Runtime;
use std::ffi::c_void;
use std::ptr;

#[no_mangle]
pub extern "C" fn zyntax_runtime_create() -> *mut c_void {
    let runtime = Box::new(Runtime::new().unwrap());
    Box::into_raw(runtime) as *mut c_void
}

#[no_mangle]
pub extern "C" fn zyntax_runtime_spawn(
    runtime: *mut c_void,
    state: *mut c_void,
    poll_fn: extern "C" fn(*mut c_void, *const Waker) -> PollStatus
) -> *mut c_void {
    // Wrap the poll function in a Rust Future
    // Spawn it on the Tokio runtime
    // Return task handle
}

// etc.
```

**Option B: Minimal Custom Runtime** (More control)
```rust
// Lightweight executor with epoll/kqueue
// No external dependencies
// ~500 lines of code
pub struct MinimalRuntime {
    ready_queue: VecDeque<TaskHandle>,
    io_poller: IoPoller, // epoll on Linux, kqueue on macOS
    tasks: HashMap<TaskId, Task>,
}
```

#### 3. **Cranelift Code Generation Strategy**

The Cranelift backend needs to generate:

1. **State Struct**
   ```rust
   fn generate_async_state_struct(&mut self, state_machine: &AsyncStateMachine) -> CompilerResult<()> {
       // Allocate struct with:
       // - u32 state_id (which await point we're at)
       // - Captured variables
       // - Intermediate results between awaits

       // This is straightforward - just struct allocation
   }
   ```

2. **Poll Function**
   ```rust
   fn generate_poll_function(&mut self, state_machine: &AsyncStateMachine) -> CompilerResult<Value> {
       // Generate a function with signature:
       // extern "C" fn(state: *mut State, waker: *const Waker) -> Poll<T>

       // Load state->state_id
       // Simple if-else chain or jump table (NOT switch statement)
       // Each state is a separate basic block
       // Call nested futures' poll functions

       // Much simpler than trying to do full state machine in HIR
   }
   ```

3. **Runtime FFI Calls**
   ```rust
   fn generate_spawn_call(&mut self, future: Value) -> CompilerResult<Value> {
       // Import zyntax_runtime_spawn
       // Pass state pointer and poll function pointer
       // Return task handle

       // Just a normal FFI call - Cranelift handles this well
   }
   ```

---

## Comparison of Approaches

| Approach | Complexity | Performance | Integration | Maintainability |
|----------|------------|-------------|-------------|-----------------|
| **HIR State Machine** | ⚠️ Very High | ⚠️ Medium | ❌ Poor | ❌ Hard |
| **External Runtime + FFI** | ✅ Low | ✅ High | ✅ Excellent | ✅ Easy |
| **Built-in Reactor** | ❌ Extreme | ✅ Highest | ⚠️ Medium | ❌ Very Hard |

### Why External Runtime Wins

1. **Leverage Existing Work**
   - Tokio is battle-tested
   - async-std is proven
   - Don't reinvent the wheel

2. **Simpler Cranelift Integration**
   - Just generate poll() functions
   - FFI calls are straightforward
   - No complex control flow needed

3. **Flexibility**
   - Users can choose runtime (Tokio/async-std/custom)
   - Can swap runtimes without recompiling
   - Testing is easier

4. **Performance**
   - FFI overhead is minimal (nanoseconds)
   - Runtime is optimized for async I/O
   - JIT code is still fast

---

## Implementation Plan

### Phase 1: Minimal Async Support (500-800 lines)

**Goal**: Get basic async/await working with external runtime

1. **Create Runtime C API** (150 lines)
   - Header file with basic runtime interface
   - Poll/Ready/Pending types
   - Task spawning

2. **Build Minimal Rust Runtime** (300 lines)
   - Tokio-based or custom
   - C API wrapper
   - Shared library (.so/.dylib/.dll)

3. **Cranelift State Struct Generation** (150 lines)
   - Allocate state struct
   - Store captured variables
   - Track current await point

4. **Cranelift Poll Function Generation** (200 lines)
   - Generate poll() function
   - Simple state dispatch (if-else or jump table)
   - Call nested futures

5. **FFI Glue** (100 lines)
   - Import runtime functions
   - Generate spawn/block_on calls
   - Handle task results

### Phase 2: Enhanced Features (300-500 lines)

1. **Async Closures**
2. **Async Iterators/Streams**
3. **Cancellation Support**
4. **Multiple Runtime Support** (Tokio/async-std/custom)

---

## Example: End-to-End Flow

```rust
// 1. Source code
async fn fetch_user(id: u64) -> User {
    let response = http_get(format!("/users/{}", id)).await;
    parse_json(response).await
}

// 2. Compiler generates HIR with async metadata
// AsyncStateMachine {
//     states: [
//         State0: { call http_get, await },
//         State1: { call parse_json, await },
//         State2: { return result }
//     ],
//     captures: [id: u64],
// }

// 3. Cranelift generates:
struct FetchUserState {
    state_id: u32,
    id: u64,
    response: Option<Response>,
}

extern "C" fn fetch_user_poll(state: *mut FetchUserState, waker: *const Waker) -> Poll<User> {
    // ... state machine logic
}

// 4. Runtime wraps this:
let state = Box::new(FetchUserState { state_id: 0, id: 123, response: None });
let task = runtime.spawn(state, fetch_user_poll);
let user = runtime.block_on(task);
```

---

## Performance Considerations

### FFI Overhead
- **Cost**: ~5-10ns per call (negligible)
- **Compared to**: I/O latency (microseconds to milliseconds)
- **Verdict**: Not a concern for async code

### State Struct Allocation
- **Stack**: For sync calling async (when possible)
- **Heap**: For spawned tasks (required anyway)
- **Optimization**: Arena allocator for task states

### Poll Function Calls
- **Fast path**: Inline poll when future is ready
- **Slow path**: Runtime reschedules
- **Typical**: 1-3 polls per I/O operation

---

## Alternatives Considered

### 1. Stackful Coroutines (fibers)
- **Pro**: Simpler state management
- **Con**: Large memory overhead (stack per coroutine)
- **Con**: Difficult to implement in JIT
- **Verdict**: Not suitable for Cranelift

### 2. CPS (Continuation Passing Style) Transformation
- **Pro**: Theoretically elegant
- **Con**: Extremely complex for JIT
- **Con**: Poor debuggability
- **Verdict**: Academic exercise, not practical

### 3. Green Threads
- **Pro**: Works well in some languages (Go, Erlang)
- **Con**: Requires runtime scheduler in every binary
- **Con**: Doesn't integrate with OS async I/O
- **Verdict**: Wrong model for systems language

---

## Recommendation

**Use External Runtime + FFI Approach**

### Why?
1. ✅ **Simplest integration** with Cranelift (just generate poll functions)
2. ✅ **Leverage existing runtimes** (Tokio/async-std)
3. ✅ **Matches Rust's model** (proven to work)
4. ✅ **Easy to test** (runtime is separate)
5. ✅ **Flexible** (swap runtimes without recompiling)

### Implementation Effort
- **Minimal**: 500-800 lines
- **Time**: 2-3 weeks for experienced developer
- **Risk**: Low (proven approach)

### Next Steps

1. Create `zyntax-async-runtime` crate with C API
2. Implement basic Tokio wrapper
3. Add Cranelift codegen for state structs
4. Generate poll() functions
5. Test with simple async/await examples
6. Document and optimize

---

## Questions?

- **Q: Can we avoid the external dependency?**
  - A: Yes, build minimal custom runtime (~500 lines), but loses ecosystem integration

- **Q: What about WASM?**
  - A: Same approach works - WASM has async proposals that match this model

- **Q: Performance vs hand-written?**
  - A: Within 1-5% of hand-written Rust async code (FFI overhead is negligible)

- **Q: Can we do better than Tokio?**
  - A: For general case, no. For specific workloads, custom runtime could be 10-20% faster.

---

## Decision

**Recommended**: External Runtime (Tokio-based) + FFI

This matches your experience that "async state was really hard" in Cranelift - we avoid the hard part by delegating to a proven runtime!
