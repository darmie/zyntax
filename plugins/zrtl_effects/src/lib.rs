//! ZRTL Effects Plugin
//!
//! Provides runtime support for algebraic effects in Zyntax-based languages.
//!
//! ## Overview
//!
//! Algebraic effects provide a structured way to handle side effects:
//! - Effect definitions declare operations (like `log`, `get`, `put`)
//! - Handlers implement these operations
//! - Code can "perform" effects, which dispatch to the nearest handler
//!
//! ## Architecture
//!
//! This plugin manages a thread-local handler stack. When code enters a
//! `with handler { ... }` block, the handler is pushed onto the stack.
//! When code performs an effect, we search the stack for a matching handler.
//!
//! ### Tier 1: Simple Effects (Current)
//!
//! For non-resumable handlers (most common case):
//! - Handler functions are called directly
//! - No continuation capture
//! - Very low overhead
//!
//! ### Tier 2: Stateful Effects (Future)
//!
//! For handlers with mutable state:
//! - State is allocated alongside handler
//! - Operations can read/write handler state
//!
//! ### Tier 3: Full Continuations (Future)
//!
//! For handlers that need to suspend and resume:
//! - Continuation capture via CPS transformation
//! - Trampoline execution model
//!
//! ## Exported Symbols
//!
//! ### Handler Management
//! - `$Effects$push_handler` - Push handler onto stack
//! - `$Effects$pop_handler` - Pop handler from stack
//! - `$Effects$get_handler` - Get handler for effect
//! - `$Effects$has_handler` - Check if handler exists
//! - `$Effects$handler_depth` - Get current handler stack depth
//!
//! ### Effect Operations
//! - `$Effects$perform` - Invoke effect operation (runtime dispatch)
//! - `$Effects$perform_direct` - Invoke with known handler (direct call)
//!
//! ### State Management
//! - `$Effects$alloc_state` - Allocate handler state
//! - `$Effects$free_state` - Free handler state
//! - `$Effects$get_state` - Get handler state pointer

use std::cell::RefCell;
use std::collections::HashMap;
use std::ptr;
use zrtl::{zrtl_plugin, StringConstPtr, string_as_str};

// ============================================================================
// Handler Types
// ============================================================================

/// Unique identifier for an effect
pub type EffectId = u64;

/// Unique identifier for a handler instance
pub type HandlerId = u64;

/// Function pointer for handler operations
/// Signature: fn(handler_state: *mut u8, args: *const *const u8, num_args: u32) -> *mut u8
pub type HandlerOpFn = extern "C" fn(*mut u8, *const *const u8, u32) -> *mut u8;

/// VTable for handler operations
#[repr(C)]
pub struct HandlerVTable {
    /// Effect ID this handler handles
    pub effect_id: EffectId,
    /// Number of operations
    pub num_ops: u32,
    /// Operation names (null-terminated strings)
    pub op_names: *const *const i8,
    /// Operation function pointers
    pub op_fns: *const HandlerOpFn,
}

/// Handler instance on the stack
#[repr(C)]
pub struct HandlerEntry {
    /// Unique ID for this handler instance
    pub id: HandlerId,
    /// Effect ID this handles
    pub effect_id: EffectId,
    /// VTable pointer (for operation dispatch)
    pub vtable: *const HandlerVTable,
    /// Handler state (user-defined struct)
    pub state: *mut u8,
    /// Size of state in bytes
    pub state_size: u32,
    /// Whether this handler owns its state (should free on pop)
    pub owns_state: bool,
}

// ============================================================================
// Thread-Local Handler Stack
// ============================================================================

thread_local! {
    /// Stack of active effect handlers (innermost at the end)
    static HANDLER_STACK: RefCell<Vec<HandlerEntry>> = RefCell::new(Vec::with_capacity(16));

    /// Counter for generating unique handler IDs
    static HANDLER_ID_COUNTER: RefCell<u64> = RefCell::new(1);

    /// Cache of effect_id -> handler_id for fast lookup
    static EFFECT_HANDLER_CACHE: RefCell<HashMap<EffectId, HandlerId>> = RefCell::new(HashMap::new());
}

fn next_handler_id() -> HandlerId {
    HANDLER_ID_COUNTER.with(|counter| {
        let mut c = counter.borrow_mut();
        let id = *c;
        *c += 1;
        id
    })
}

// ============================================================================
// Handler Stack Management
// ============================================================================

/// Push a handler onto the effect handler stack
///
/// # Arguments
/// - `effect_id`: The effect this handler handles
/// - `vtable`: Pointer to the handler's VTable
/// - `state`: Pointer to the handler's state (can be null)
/// - `state_size`: Size of the state in bytes
/// - `owns_state`: Whether to free state when handler is popped
///
/// # Returns
/// Handler ID for this instance
#[no_mangle]
pub extern "C" fn effects_push_handler(
    effect_id: EffectId,
    vtable: *const HandlerVTable,
    state: *mut u8,
    state_size: u32,
    owns_state: bool,
) -> HandlerId {
    let handler_id = next_handler_id();

    let entry = HandlerEntry {
        id: handler_id,
        effect_id,
        vtable,
        state,
        state_size,
        owns_state,
    };

    HANDLER_STACK.with(|stack| {
        stack.borrow_mut().push(entry);
    });

    // Update cache
    EFFECT_HANDLER_CACHE.with(|cache| {
        cache.borrow_mut().insert(effect_id, handler_id);
    });

    handler_id
}

/// Pop the most recent handler from the stack
///
/// # Returns
/// The handler ID that was popped, or 0 if stack was empty
#[no_mangle]
pub extern "C" fn effects_pop_handler() -> HandlerId {
    HANDLER_STACK.with(|stack| {
        let mut stack = stack.borrow_mut();
        if let Some(entry) = stack.pop() {
            // Free state if owned
            if entry.owns_state && !entry.state.is_null() {
                unsafe {
                    // Use libc free for consistency with alloc_state
                    libc::free(entry.state as *mut libc::c_void);
                }
            }

            // Update cache - find next handler for this effect
            let effect_id = entry.effect_id;
            EFFECT_HANDLER_CACHE.with(|cache| {
                let mut cache = cache.borrow_mut();
                // Search for another handler for this effect
                let new_handler = stack.iter().rev()
                    .find(|h| h.effect_id == effect_id)
                    .map(|h| h.id);

                if let Some(id) = new_handler {
                    cache.insert(effect_id, id);
                } else {
                    cache.remove(&effect_id);
                }
            });

            entry.id
        } else {
            0
        }
    })
}

/// Get the handler for a specific effect
///
/// # Arguments
/// - `effect_id`: The effect to look up
///
/// # Returns
/// Pointer to the handler entry, or null if no handler found
#[no_mangle]
pub extern "C" fn effects_get_handler(effect_id: EffectId) -> *const HandlerEntry {
    HANDLER_STACK.with(|stack| {
        let stack = stack.borrow();
        // Search from innermost (end) to outermost (start)
        for entry in stack.iter().rev() {
            if entry.effect_id == effect_id {
                return entry as *const HandlerEntry;
            }
        }
        ptr::null()
    })
}

/// Check if a handler exists for an effect
///
/// # Arguments
/// - `effect_id`: The effect to check
///
/// # Returns
/// true if a handler exists, false otherwise
#[no_mangle]
pub extern "C" fn effects_has_handler(effect_id: EffectId) -> bool {
    EFFECT_HANDLER_CACHE.with(|cache| {
        cache.borrow().contains_key(&effect_id)
    })
}

/// Get the current handler stack depth
///
/// # Returns
/// Number of handlers on the stack
#[no_mangle]
pub extern "C" fn effects_handler_depth() -> u32 {
    HANDLER_STACK.with(|stack| {
        stack.borrow().len() as u32
    })
}

/// Get the handler state pointer
///
/// # Arguments
/// - `effect_id`: The effect whose handler state to get
///
/// # Returns
/// Pointer to handler state, or null if no handler found
#[no_mangle]
pub extern "C" fn effects_get_state(effect_id: EffectId) -> *mut u8 {
    let handler = effects_get_handler(effect_id);
    if handler.is_null() {
        return ptr::null_mut();
    }
    unsafe { (*handler).state }
}

// ============================================================================
// Effect Operations
// ============================================================================

/// Perform an effect operation with runtime dispatch
///
/// This function looks up the handler for the effect and dispatches
/// to the appropriate operation implementation.
///
/// # Arguments
/// - `effect_id`: The effect to perform
/// - `op_index`: Index of the operation in the handler's VTable
/// - `args`: Pointer to array of argument pointers
/// - `num_args`: Number of arguments
///
/// # Returns
/// Result pointer from the handler, or null if no handler found
#[no_mangle]
pub extern "C" fn effects_perform(
    effect_id: EffectId,
    op_index: u32,
    args: *const *const u8,
    num_args: u32,
) -> *mut u8 {
    let handler = effects_get_handler(effect_id);
    if handler.is_null() {
        // No handler found - this is a runtime error
        // In a real implementation, we'd want to panic or return an error
        eprintln!("[ZRTL Effects] ERROR: No handler found for effect {}", effect_id);
        return ptr::null_mut();
    }

    unsafe {
        let entry = &*handler;
        if entry.vtable.is_null() {
            eprintln!("[ZRTL Effects] ERROR: Handler has null vtable");
            return ptr::null_mut();
        }

        let vtable = &*entry.vtable;
        if op_index >= vtable.num_ops {
            eprintln!("[ZRTL Effects] ERROR: Operation index {} out of bounds (max {})",
                     op_index, vtable.num_ops);
            return ptr::null_mut();
        }

        // Get the operation function
        let op_fn = *vtable.op_fns.add(op_index as usize);

        // Call the operation with handler state
        op_fn(entry.state, args, num_args)
    }
}

/// Perform an effect operation with direct dispatch
///
/// This is an optimized path when the handler is known at compile time.
/// Skips the handler lookup.
///
/// # Arguments
/// - `handler_state`: Pointer to handler state
/// - `op_fn`: Function pointer for the operation
/// - `args`: Pointer to array of argument pointers
/// - `num_args`: Number of arguments
///
/// # Returns
/// Result pointer from the handler
#[no_mangle]
pub extern "C" fn effects_perform_direct(
    handler_state: *mut u8,
    op_fn: HandlerOpFn,
    args: *const *const u8,
    num_args: u32,
) -> *mut u8 {
    op_fn(handler_state, args, num_args)
}

// ============================================================================
// State Management
// ============================================================================

/// Allocate handler state
///
/// # Arguments
/// - `size`: Size in bytes to allocate
///
/// # Returns
/// Pointer to allocated memory, or null on failure
#[no_mangle]
pub extern "C" fn effects_alloc_state(size: u32) -> *mut u8 {
    if size == 0 {
        return ptr::null_mut();
    }

    unsafe {
        let ptr = libc::malloc(size as usize) as *mut u8;
        if !ptr.is_null() {
            // Zero-initialize
            ptr::write_bytes(ptr, 0, size as usize);
        }
        ptr
    }
}

/// Free handler state
///
/// # Arguments
/// - `state`: Pointer to state to free
#[no_mangle]
pub extern "C" fn effects_free_state(state: *mut u8) {
    if !state.is_null() {
        unsafe {
            libc::free(state as *mut libc::c_void);
        }
    }
}

// ============================================================================
// Effect ID Generation
// ============================================================================

/// Hash a string to create an effect ID
///
/// Uses FNV-1a hash for good distribution.
///
/// # Arguments
/// - `name`: Effect name as ZRTL string
///
/// # Returns
/// Effect ID (hash of name)
#[no_mangle]
pub unsafe extern "C" fn effects_id_from_name(name: StringConstPtr) -> EffectId {
    if name.is_null() {
        return 0;
    }

    if let Some(s) = string_as_str(name) {
        fnv1a_hash(s.as_bytes())
    } else {
        0
    }
}

/// FNV-1a hash function
fn fnv1a_hash(data: &[u8]) -> u64 {
    const FNV_OFFSET: u64 = 14695981039346656037;
    const FNV_PRIME: u64 = 1099511628211;

    let mut hash = FNV_OFFSET;
    for byte in data {
        hash ^= *byte as u64;
        hash = hash.wrapping_mul(FNV_PRIME);
    }
    hash
}

// ============================================================================
// Debug/Introspection
// ============================================================================

/// Print the current handler stack (for debugging)
#[no_mangle]
pub extern "C" fn effects_debug_stack() {
    HANDLER_STACK.with(|stack| {
        let stack = stack.borrow();
        eprintln!("[ZRTL Effects] Handler Stack ({} entries):", stack.len());
        for (i, entry) in stack.iter().enumerate() {
            eprintln!("  [{}] id={}, effect_id={}, state={:?}, state_size={}",
                     i, entry.id, entry.effect_id, entry.state, entry.state_size);
        }
    });
}

/// Clear all handlers (for testing/cleanup)
#[no_mangle]
pub extern "C" fn effects_clear_all() {
    HANDLER_STACK.with(|stack| {
        let mut stack = stack.borrow_mut();
        // Free owned states
        for entry in stack.drain(..) {
            if entry.owns_state && !entry.state.is_null() {
                unsafe {
                    libc::free(entry.state as *mut libc::c_void);
                }
            }
        }
    });

    EFFECT_HANDLER_CACHE.with(|cache| {
        cache.borrow_mut().clear();
    });
}

// ============================================================================
// Plugin Export
// ============================================================================

zrtl_plugin! {
    name: "zrtl_effects",
    symbols: [
        // Handler management
        ("$Effects$push_handler", effects_push_handler),
        ("$Effects$pop_handler", effects_pop_handler),
        ("$Effects$get_handler", effects_get_handler),
        ("$Effects$has_handler", effects_has_handler),
        ("$Effects$handler_depth", effects_handler_depth),
        ("$Effects$get_state", effects_get_state),

        // Effect operations
        ("$Effects$perform", effects_perform),
        ("$Effects$perform_direct", effects_perform_direct),

        // State management
        ("$Effects$alloc_state", effects_alloc_state),
        ("$Effects$free_state", effects_free_state),

        // Utilities
        ("$Effects$id_from_name", effects_id_from_name),
        ("$Effects$debug_stack", effects_debug_stack),
        ("$Effects$clear_all", effects_clear_all),
    ]
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_handler_stack_basic() {
        effects_clear_all();

        // Push a handler
        let id1 = effects_push_handler(1, ptr::null(), ptr::null_mut(), 0, false);
        assert!(id1 > 0);
        assert!(effects_has_handler(1));
        assert_eq!(effects_handler_depth(), 1);

        // Push another handler for same effect
        let id2 = effects_push_handler(1, ptr::null(), ptr::null_mut(), 0, false);
        assert!(id2 > id1);
        assert_eq!(effects_handler_depth(), 2);

        // Pop handlers
        let popped = effects_pop_handler();
        assert_eq!(popped, id2);
        assert_eq!(effects_handler_depth(), 1);
        assert!(effects_has_handler(1)); // Still has id1

        let popped = effects_pop_handler();
        assert_eq!(popped, id1);
        assert_eq!(effects_handler_depth(), 0);
        assert!(!effects_has_handler(1)); // No more handlers

        effects_clear_all();
    }

    #[test]
    fn test_multiple_effects() {
        effects_clear_all();

        // Push handlers for different effects
        let _id1 = effects_push_handler(100, ptr::null(), ptr::null_mut(), 0, false);
        let _id2 = effects_push_handler(200, ptr::null(), ptr::null_mut(), 0, false);
        let _id3 = effects_push_handler(300, ptr::null(), ptr::null_mut(), 0, false);

        assert!(effects_has_handler(100));
        assert!(effects_has_handler(200));
        assert!(effects_has_handler(300));
        assert!(!effects_has_handler(400));
        assert_eq!(effects_handler_depth(), 3);

        effects_clear_all();
        assert_eq!(effects_handler_depth(), 0);
    }

    #[test]
    fn test_state_management() {
        effects_clear_all();

        // Allocate state
        let state = effects_alloc_state(64);
        assert!(!state.is_null());

        // Push handler with owned state
        let id = effects_push_handler(1, ptr::null(), state, 64, true);
        assert!(id > 0);

        // Get state
        let retrieved = effects_get_state(1);
        assert_eq!(retrieved, state);

        // Pop should free the state
        effects_pop_handler();
        // (state is freed, can't test directly)

        effects_clear_all();
    }

    #[test]
    fn test_effect_id_hash() {
        // Create a ZRTL string for testing
        let name = b"TestEffect\0";
        // Note: This would need actual ZRTL string format for real test
        // For now, just verify the hash function works
        let hash1 = fnv1a_hash(b"TestEffect");
        let hash2 = fnv1a_hash(b"TestEffect");
        let hash3 = fnv1a_hash(b"OtherEffect");

        assert_eq!(hash1, hash2);
        assert_ne!(hash1, hash3);
    }
}
