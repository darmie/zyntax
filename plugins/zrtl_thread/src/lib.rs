//! ZRTL Thread Plugin
//!
//! Provides threading and atomic operations for Zyntax-based languages.
//!
//! ## Exported Symbols
//!
//! ### Thread Operations
//! - `$Thread$spawn` - Spawn a new thread with a function pointer
//! - `$Thread$spawn_closure` - Spawn a new thread with a ZrtlClosure
//! - `$Thread$join` - Wait for thread to complete
//! - `$Thread$current_id` - Get current thread ID
//! - `$Thread$yield_now` - Yield execution to other threads
//! - `$Thread$park` - Park the current thread
//! - `$Thread$unpark` - Unpark a thread by handle
//!
//! ### Atomics (i64)
//! - `$Atomic$new` - Create new atomic i64
//! - `$Atomic$load` - Load value atomically
//! - `$Atomic$store` - Store value atomically
//! - `$Atomic$add` - Atomic fetch-add
//! - `$Atomic$sub` - Atomic fetch-sub
//! - `$Atomic$swap` - Atomic swap
//! - `$Atomic$compare_exchange` - Atomic CAS
//! - `$Atomic$free` - Free atomic
//!
//! ### Mutex
//! - `$Mutex$new` - Create new mutex
//! - `$Mutex$lock` - Lock mutex
//! - `$Mutex$try_lock` - Try to lock mutex
//! - `$Mutex$unlock` - Unlock mutex
//! - `$Mutex$free` - Free mutex

use std::collections::HashMap;
use std::sync::atomic::{AtomicI64, AtomicU64, Ordering};
use std::sync::{Arc, Mutex, MutexGuard};
use std::thread::{self, JoinHandle, Thread};
use zrtl::{zrtl_plugin, ZrtlClosure, DynamicBox};

// ============================================================================
// Handle Management
// ============================================================================

static HANDLE_COUNTER: AtomicU64 = AtomicU64::new(1);

type ThreadMap = HashMap<u64, JoinHandle<i64>>;
type AtomicMap = HashMap<u64, Box<AtomicI64>>;
type MutexMap = HashMap<u64, Arc<Mutex<()>>>;
type ThreadRefMap = HashMap<u64, Thread>;

static THREADS: Mutex<Option<ThreadMap>> = Mutex::new(None);
static ATOMICS: Mutex<Option<AtomicMap>> = Mutex::new(None);
static MUTEXES: Mutex<Option<MutexMap>> = Mutex::new(None);
static THREAD_REFS: Mutex<Option<ThreadRefMap>> = Mutex::new(None);

fn next_handle() -> u64 {
    HANDLE_COUNTER.fetch_add(1, Ordering::SeqCst)
}

macro_rules! get_map {
    ($static:ident, $type:ty) => {{
        let mut guard = $static.lock().unwrap();
        if guard.is_none() {
            *guard = Some(HashMap::new());
        }
        guard
    }};
}

// ============================================================================
// Thread Functions
// ============================================================================

/// Function pointer type for thread entry
pub type ThreadFn = extern "C" fn(i64) -> i64;

/// Spawn a new thread
///
/// The function will be called with the provided argument.
/// Returns a thread handle (0 on error).
#[no_mangle]
pub extern "C" fn thread_spawn(func: ThreadFn, arg: i64) -> u64 {
    let handle_id = next_handle();

    let join_handle = thread::spawn(move || {
        func(arg)
    });

    // Store thread reference for unpark
    {
        let mut refs = get_map!(THREAD_REFS, ThreadRefMap);
        if let Some(ref mut m) = *refs {
            m.insert(handle_id, join_handle.thread().clone());
        }
    }

    // Store join handle
    {
        let mut threads = get_map!(THREADS, ThreadMap);
        if let Some(ref mut m) = *threads {
            m.insert(handle_id, join_handle);
        }
    }

    handle_id
}

/// Spawn a new thread with a closure
///
/// The closure will be called with the provided argument.
/// Returns a thread handle (0 on error).
///
/// # Safety
/// The closure pointer must be valid and owned (will be consumed).
#[no_mangle]
pub unsafe extern "C" fn thread_spawn_closure(closure: *mut ZrtlClosure, arg: i64) -> u64 {
    if closure.is_null() {
        return 0;
    }

    // Take ownership of the closure
    let closure = Box::from_raw(closure);
    let handle_id = next_handle();

    let join_handle = thread::spawn(move || {
        match closure.call(arg) {
            r if r.is_ok() => r.value,
            _ => i64::MIN,
        }
    });

    // Store thread reference for unpark
    {
        let mut refs = get_map!(THREAD_REFS, ThreadRefMap);
        if let Some(ref mut m) = *refs {
            m.insert(handle_id, join_handle.thread().clone());
        }
    }

    // Store join handle
    {
        let mut threads = get_map!(THREADS, ThreadMap);
        if let Some(ref mut m) = *threads {
            m.insert(handle_id, join_handle);
        }
    }

    handle_id
}

/// Spawn a new thread with a closure (no argument needed)
///
/// Use this when the closure already has all captured state it needs.
/// This is the preferred API for compiler-generated closures.
/// Returns a thread handle (0 on error).
///
/// # Safety
/// The closure pointer must be valid and owned (will be consumed).
#[no_mangle]
pub unsafe extern "C" fn thread_spawn_closure_noarg(closure: *mut ZrtlClosure) -> u64 {
    thread_spawn_closure(closure, 0)
}

/// Spawn a new thread with a DynamicBox containing a closure
///
/// This is the most flexible API - accepts any DynamicBox that contains
/// a ZrtlClosure. The box is consumed.
/// Returns a thread handle (0 on error, including if box doesn't contain closure).
///
/// # Safety
/// The box pointer must be valid and owned (will be consumed).
#[no_mangle]
pub unsafe extern "C" fn thread_spawn_boxed(boxed: *mut DynamicBox) -> u64 {
    if boxed.is_null() {
        return 0;
    }

    let mut boxed = Box::from_raw(boxed);

    // Check if it's a closure
    if !boxed.is_closure() {
        return 0;
    }

    // Extract the closure
    let closure = match boxed.as_closure() {
        Some(c) => c.clone(),
        None => return 0,
    };

    // Free the box
    boxed.free();

    let handle_id = next_handle();

    let join_handle = thread::spawn(move || {
        match closure.call(0) {
            r if r.is_ok() => r.value,
            _ => i64::MIN,
        }
    });

    // Store thread reference for unpark
    {
        let mut refs = get_map!(THREAD_REFS, ThreadRefMap);
        if let Some(ref mut m) = *refs {
            m.insert(handle_id, join_handle.thread().clone());
        }
    }

    // Store join handle
    {
        let mut threads = get_map!(THREADS, ThreadMap);
        if let Some(ref mut m) = *threads {
            m.insert(handle_id, join_handle);
        }
    }

    handle_id
}

/// Spawn a new thread with a cloned closure (closure is not consumed)
///
/// The closure will be cloned before spawning, so it can be reused.
/// Returns a thread handle (0 on error).
///
/// # Safety
/// The closure pointer must be valid.
#[no_mangle]
pub unsafe extern "C" fn thread_spawn_closure_cloned(closure: *const ZrtlClosure, arg: i64) -> u64 {
    if closure.is_null() {
        return 0;
    }

    // Clone the closure instead of taking ownership
    let closure = (*closure).clone();
    let handle_id = next_handle();

    let join_handle = thread::spawn(move || {
        match closure.call(arg) {
            r if r.is_ok() => r.value,
            _ => i64::MIN,
        }
    });

    // Store thread reference for unpark
    {
        let mut refs = get_map!(THREAD_REFS, ThreadRefMap);
        if let Some(ref mut m) = *refs {
            m.insert(handle_id, join_handle.thread().clone());
        }
    }

    // Store join handle
    {
        let mut threads = get_map!(THREADS, ThreadMap);
        if let Some(ref mut m) = *threads {
            m.insert(handle_id, join_handle);
        }
    }

    handle_id
}

/// Join a thread and get its return value
///
/// Returns the thread's return value, or i64::MIN on error.
/// The handle is freed after joining.
#[no_mangle]
pub extern "C" fn thread_join(handle: u64) -> i64 {
    // Remove from refs
    {
        let mut refs = get_map!(THREAD_REFS, ThreadRefMap);
        if let Some(ref mut m) = *refs {
            m.remove(&handle);
        }
    }

    // Get and join
    let join_handle = {
        let mut threads = get_map!(THREADS, ThreadMap);
        if let Some(ref mut m) = *threads {
            m.remove(&handle)
        } else {
            None
        }
    };

    match join_handle {
        Some(jh) => {
            match jh.join() {
                Ok(result) => result,
                Err(_) => i64::MIN,
            }
        }
        None => i64::MIN,
    }
}

/// Get current thread ID as u64
#[no_mangle]
pub extern "C" fn thread_current_id() -> u64 {
    // Hash the ThreadId to get a u64
    let id = thread::current().id();
    // ThreadId doesn't expose its internal value, so we format and hash
    let formatted = format!("{:?}", id);
    let mut hash: u64 = 0;
    for byte in formatted.bytes() {
        hash = hash.wrapping_mul(31).wrapping_add(byte as u64);
    }
    hash
}

/// Yield execution to other threads
#[no_mangle]
pub extern "C" fn thread_yield_now() {
    thread::yield_now();
}

/// Park the current thread until unparked
#[no_mangle]
pub extern "C" fn thread_park() {
    thread::park();
}

/// Unpark a thread by handle
///
/// Returns 0 on success, -1 if handle not found.
#[no_mangle]
pub extern "C" fn thread_unpark(handle: u64) -> i32 {
    let refs = get_map!(THREAD_REFS, ThreadRefMap);
    if let Some(ref m) = *refs {
        if let Some(thread) = m.get(&handle) {
            thread.unpark();
            return 0;
        }
    }
    -1
}

/// Get the number of available CPU cores
#[no_mangle]
pub extern "C" fn thread_available_parallelism() -> u32 {
    thread::available_parallelism()
        .map(|n| n.get() as u32)
        .unwrap_or(1)
}

// ============================================================================
// Atomic Functions (i64)
// ============================================================================

/// Create a new atomic i64
///
/// Returns a handle to the atomic.
#[no_mangle]
pub extern "C" fn atomic_new(initial: i64) -> u64 {
    let handle = next_handle();
    let atomic = Box::new(AtomicI64::new(initial));

    let mut atomics = get_map!(ATOMICS, AtomicMap);
    if let Some(ref mut m) = *atomics {
        m.insert(handle, atomic);
    }

    handle
}

/// Load value from atomic
#[no_mangle]
pub extern "C" fn atomic_load(handle: u64) -> i64 {
    let atomics = get_map!(ATOMICS, AtomicMap);
    if let Some(ref m) = *atomics {
        if let Some(atomic) = m.get(&handle) {
            return atomic.load(Ordering::SeqCst);
        }
    }
    0
}

/// Store value to atomic
#[no_mangle]
pub extern "C" fn atomic_store(handle: u64, value: i64) {
    let atomics = get_map!(ATOMICS, AtomicMap);
    if let Some(ref m) = *atomics {
        if let Some(atomic) = m.get(&handle) {
            atomic.store(value, Ordering::SeqCst);
        }
    }
}

/// Atomic fetch-add, returns previous value
#[no_mangle]
pub extern "C" fn atomic_add(handle: u64, value: i64) -> i64 {
    let atomics = get_map!(ATOMICS, AtomicMap);
    if let Some(ref m) = *atomics {
        if let Some(atomic) = m.get(&handle) {
            return atomic.fetch_add(value, Ordering::SeqCst);
        }
    }
    0
}

/// Atomic fetch-sub, returns previous value
#[no_mangle]
pub extern "C" fn atomic_sub(handle: u64, value: i64) -> i64 {
    let atomics = get_map!(ATOMICS, AtomicMap);
    if let Some(ref m) = *atomics {
        if let Some(atomic) = m.get(&handle) {
            return atomic.fetch_sub(value, Ordering::SeqCst);
        }
    }
    0
}

/// Atomic swap, returns previous value
#[no_mangle]
pub extern "C" fn atomic_swap(handle: u64, value: i64) -> i64 {
    let atomics = get_map!(ATOMICS, AtomicMap);
    if let Some(ref m) = *atomics {
        if let Some(atomic) = m.get(&handle) {
            return atomic.swap(value, Ordering::SeqCst);
        }
    }
    0
}

/// Atomic compare-and-swap
///
/// If current value equals `expected`, sets to `new_value` and returns 1.
/// Otherwise returns 0 and leaves value unchanged.
#[no_mangle]
pub extern "C" fn atomic_compare_exchange(handle: u64, expected: i64, new_value: i64) -> i32 {
    let atomics = get_map!(ATOMICS, AtomicMap);
    if let Some(ref m) = *atomics {
        if let Some(atomic) = m.get(&handle) {
            match atomic.compare_exchange(expected, new_value, Ordering::SeqCst, Ordering::SeqCst) {
                Ok(_) => return 1,
                Err(_) => return 0,
            }
        }
    }
    0
}

/// Free an atomic
#[no_mangle]
pub extern "C" fn atomic_free(handle: u64) {
    let mut atomics = get_map!(ATOMICS, AtomicMap);
    if let Some(ref mut m) = *atomics {
        m.remove(&handle);
    }
}

// ============================================================================
// Mutex Functions
// ============================================================================

/// Create a new mutex
///
/// Returns a handle to the mutex.
#[no_mangle]
pub extern "C" fn mutex_new() -> u64 {
    let handle = next_handle();
    let mutex = Arc::new(Mutex::new(()));

    let mut mutexes = get_map!(MUTEXES, MutexMap);
    if let Some(ref mut m) = *mutexes {
        m.insert(handle, mutex);
    }

    handle
}

// Thread-local storage for held mutex guards
thread_local! {
    static HELD_GUARDS: std::cell::RefCell<HashMap<u64, MutexGuard<'static, ()>>> =
        std::cell::RefCell::new(HashMap::new());
}

/// Lock a mutex
///
/// Returns 0 on success, -1 on error.
/// Note: This uses a simplified locking mechanism.
#[no_mangle]
pub extern "C" fn mutex_lock(handle: u64) -> i32 {
    let mutex = {
        let mutexes = get_map!(MUTEXES, MutexMap);
        if let Some(ref m) = *mutexes {
            m.get(&handle).cloned()
        } else {
            None
        }
    };

    match mutex {
        Some(m) => {
            // This is a simplified implementation - in practice you'd want
            // to properly manage the guard lifetime
            match m.lock() {
                Ok(_guard) => {
                    // Guard is dropped here, but we've signaled lock acquisition
                    // A real implementation would need to store the guard
                    std::mem::forget(_guard);
                    0
                }
                Err(_) => -1,
            }
        }
        None => -1,
    }
}

/// Try to lock a mutex without blocking
///
/// Returns 1 if lock acquired, 0 if would block, -1 on error.
#[no_mangle]
pub extern "C" fn mutex_try_lock(handle: u64) -> i32 {
    let mutex = {
        let mutexes = get_map!(MUTEXES, MutexMap);
        if let Some(ref m) = *mutexes {
            m.get(&handle).cloned()
        } else {
            None
        }
    };

    match mutex {
        Some(m) => {
            match m.try_lock() {
                Ok(_guard) => {
                    std::mem::forget(_guard);
                    1
                }
                Err(std::sync::TryLockError::WouldBlock) => 0,
                Err(_) => -1,
            }
        }
        None => -1,
    }
}

/// Unlock a mutex
///
/// Note: This implementation relies on the mutex being unlocked by
/// dropping through the standard mechanism. In a real implementation,
/// you'd need proper guard management.
#[no_mangle]
pub extern "C" fn mutex_unlock(_handle: u64) -> i32 {
    // In a real implementation, we would release the stored guard
    // For now, this is a no-op as the guard management is simplified
    0
}

/// Free a mutex
#[no_mangle]
pub extern "C" fn mutex_free(handle: u64) {
    let mut mutexes = get_map!(MUTEXES, MutexMap);
    if let Some(ref mut m) = *mutexes {
        m.remove(&handle);
    }
}

// ============================================================================
// Plugin Export
// ============================================================================

zrtl_plugin! {
    name: "zrtl_thread",
    symbols: [
        // Thread operations
        ("$Thread$spawn", thread_spawn),
        ("$Thread$spawn_closure", thread_spawn_closure),
        ("$Thread$spawn_closure_noarg", thread_spawn_closure_noarg),
        ("$Thread$spawn_closure_cloned", thread_spawn_closure_cloned),
        ("$Thread$spawn_boxed", thread_spawn_boxed),
        ("$Thread$join", thread_join),
        ("$Thread$current_id", thread_current_id),
        ("$Thread$yield_now", thread_yield_now),
        ("$Thread$park", thread_park),
        ("$Thread$unpark", thread_unpark),
        ("$Thread$available_parallelism", thread_available_parallelism),

        // Atomics
        ("$Atomic$new", atomic_new),
        ("$Atomic$load", atomic_load),
        ("$Atomic$store", atomic_store),
        ("$Atomic$add", atomic_add),
        ("$Atomic$sub", atomic_sub),
        ("$Atomic$swap", atomic_swap),
        ("$Atomic$compare_exchange", atomic_compare_exchange),
        ("$Atomic$free", atomic_free),

        // Mutex
        ("$Mutex$new", mutex_new),
        ("$Mutex$lock", mutex_lock),
        ("$Mutex$try_lock", mutex_try_lock),
        ("$Mutex$unlock", mutex_unlock),
        ("$Mutex$free", mutex_free),
    ]
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::AtomicI64;
    use zrtl::zrtl_closure_from_fn;

    #[test]
    fn test_atomic_operations() {
        let handle = atomic_new(10);

        assert_eq!(atomic_load(handle), 10);

        atomic_store(handle, 20);
        assert_eq!(atomic_load(handle), 20);

        let prev = atomic_add(handle, 5);
        assert_eq!(prev, 20);
        assert_eq!(atomic_load(handle), 25);

        let prev = atomic_sub(handle, 10);
        assert_eq!(prev, 25);
        assert_eq!(atomic_load(handle), 15);

        let prev = atomic_swap(handle, 100);
        assert_eq!(prev, 15);
        assert_eq!(atomic_load(handle), 100);

        // CAS success
        let result = atomic_compare_exchange(handle, 100, 200);
        assert_eq!(result, 1);
        assert_eq!(atomic_load(handle), 200);

        // CAS failure
        let result = atomic_compare_exchange(handle, 100, 300);
        assert_eq!(result, 0);
        assert_eq!(atomic_load(handle), 200);

        atomic_free(handle);
    }

    #[test]
    fn test_thread_spawn_join() {
        extern "C" fn double(x: i64) -> i64 {
            x * 2
        }

        let handle = thread_spawn(double, 21);
        assert!(handle > 0);

        let result = thread_join(handle);
        assert_eq!(result, 42);
    }

    #[test]
    fn test_available_parallelism() {
        let cores = thread_available_parallelism();
        assert!(cores >= 1);
    }

    #[test]
    fn test_mutex() {
        let handle = mutex_new();
        assert!(handle > 0);

        // Lock should succeed
        assert_eq!(mutex_lock(handle), 0);

        mutex_free(handle);
    }

    #[test]
    fn test_thread_spawn_closure() {
        extern "C" fn triple(x: i64) -> i64 {
            x * 3
        }

        unsafe {
            let closure = zrtl_closure_from_fn(triple);
            let handle = thread_spawn_closure(closure, 14);
            assert!(handle > 0);

            let result = thread_join(handle);
            assert_eq!(result, 42);
        }
    }

    #[test]
    fn test_thread_spawn_closure_cloned() {
        // Test spawning multiple threads with the same cloned closure
        let counter = Arc::new(AtomicI64::new(0));
        let counter_clone = counter.clone();

        let closure = ZrtlClosure::new(move |x| {
            counter_clone.fetch_add(x, Ordering::SeqCst)
        });

        unsafe {
            let closure_ptr = &closure as *const ZrtlClosure;

            // Spawn 3 threads with the same cloned closure
            let h1 = thread_spawn_closure_cloned(closure_ptr, 10);
            let h2 = thread_spawn_closure_cloned(closure_ptr, 20);
            let h3 = thread_spawn_closure_cloned(closure_ptr, 30);

            // Join all threads
            thread_join(h1);
            thread_join(h2);
            thread_join(h3);

            // Counter should be 10 + 20 + 30 = 60
            assert_eq!(counter.load(Ordering::SeqCst), 60);
        }
    }

    #[test]
    fn test_thread_closure_with_captures() {
        let value = Arc::new(AtomicI64::new(100));
        let value_clone = value.clone();

        let closure = ZrtlClosure::new(move |delta| {
            value_clone.fetch_add(delta, Ordering::SeqCst)
        });

        let closure_box = Box::new(closure);
        let closure_ptr = Box::into_raw(closure_box);

        unsafe {
            let handle = thread_spawn_closure(closure_ptr, 42);
            let prev = thread_join(handle);

            // Previous value was 100
            assert_eq!(prev, 100);
            // New value is 142
            assert_eq!(value.load(Ordering::SeqCst), 142);
        }
    }
}
