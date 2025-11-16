//! Haxe Array implementation
//!
//! Memory layout: [capacity: i32, length: i32, elem0, elem1, ...]
//! - capacity: Total allocated slots (including header)
//! - length: Number of elements currently stored
//! - elements: Stored starting at offset 2

use zyntax_plugin_macros::{runtime_export, runtime_method};

/// Array header structure
#[repr(C)]
pub struct ArrayHeader {
    pub capacity: i32,
    pub length: i32,
}

const HEADER_SIZE: isize = 2;
const DEFAULT_CAPACITY: i32 = 8;

// ============================================================================
// Array Creation
// ============================================================================

/// Create an empty array with default capacity
#[runtime_export("$Array$new")]
pub extern "C" fn Array_new() -> *mut i32 {
    unsafe {
        let total_size = DEFAULT_CAPACITY as usize * std::mem::size_of::<i32>();
        let ptr = libc::malloc(total_size) as *mut i32;

        if ptr.is_null() {
            return core::ptr::null_mut();
        }

        *ptr = DEFAULT_CAPACITY;  // capacity
        *ptr.offset(1) = 0;       // length

        ptr
    }
}

/// Create an array from two initial elements (for array literal [a, b])
#[runtime_export("$Array$create")]
pub extern "C" fn Array_create(elem0: i32, elem1: i32) -> *mut i32 {
    unsafe {
        let capacity = 4.max(DEFAULT_CAPACITY);
        let size = capacity as usize * std::mem::size_of::<i32>();
        let ptr = libc::malloc(size) as *mut i32;

        if ptr.is_null() {
            return core::ptr::null_mut();
        }

        *ptr = capacity;          // capacity
        *ptr.offset(1) = 2;       // length
        *ptr.offset(2) = elem0;   // first element
        *ptr.offset(3) = elem1;   // second element

        ptr
    }
}

/// Create an array from variable number of elements (for array literals)
#[runtime_export("$Array$fromElements")]
pub extern "C" fn Array_fromElements(elements: *const i32, count: i32) -> *mut i32 {
    if count < 0 {
        return core::ptr::null_mut();
    }

    unsafe {
        let capacity = (count + HEADER_SIZE as i32).max(DEFAULT_CAPACITY);
        let size = capacity as usize * std::mem::size_of::<i32>();
        let ptr = libc::malloc(size) as *mut i32;

        if ptr.is_null() {
            return core::ptr::null_mut();
        }

        *ptr = capacity;
        *ptr.offset(1) = count;

        // Copy elements
        if !elements.is_null() && count > 0 {
            libc::memcpy(
                ptr.offset(HEADER_SIZE) as *mut libc::c_void,
                elements as *const libc::c_void,
                (count as usize) * std::mem::size_of::<i32>(),
            );
        }

        ptr
    }
}

// ============================================================================
// Array Access
// ============================================================================

/// Get element at index (returns 0 if out of bounds)
#[runtime_export("$Array$get")]
pub extern "C" fn Array_get(array_ptr: *const i32, index: i32) -> i32 {
    unsafe {
        if array_ptr.is_null() || index < 0 {
            return 0;
        }

        let length = *array_ptr.offset(1);
        if index >= length {
            return 0;
        }

        *array_ptr.offset(HEADER_SIZE + index as isize)
    }
}

/// Set element at index (returns 1 on success, 0 on failure)
#[runtime_export("$Array$set")]
pub extern "C" fn Array_set(array_ptr: *mut i32, index: i32, value: i32) -> i32 {
    unsafe {
        if array_ptr.is_null() || index < 0 {
            return 0;
        }

        let length = *array_ptr.offset(1);
        if index >= length {
            return 0;
        }

        *array_ptr.offset(HEADER_SIZE + index as isize) = value;
        1
    }
}

/// Get array length
#[runtime_method(
    symbol = "$Array$length",
    haxe_type = "Array",
    haxe_property = "length"
)]
pub extern "C" fn Array_length(array_ptr: *const i32) -> i32 {
    if array_ptr.is_null() {
        return 0;
    }

    unsafe { *array_ptr.offset(1) }
}

// ============================================================================
// Array Modification
// ============================================================================

/// Push element onto array (may reallocate, returns new pointer)
#[runtime_method(
    symbol = "$Array$push",
    haxe_type = "Array",
    haxe_method = "push",
    mutates = true,
    returns_self = true
)]
pub extern "C" fn Array_push(array_ptr: *mut i32, element: i32) -> *mut i32 {
    if array_ptr.is_null() {
        return core::ptr::null_mut();
    }

    unsafe {
        let capacity = *array_ptr;
        let length = *array_ptr.offset(1);

        let final_ptr = if length + HEADER_SIZE as i32 >= capacity {
            // Double the capacity
            let new_capacity = capacity * 2;
            let new_size = new_capacity as usize * std::mem::size_of::<i32>();
            let new_ptr = libc::realloc(array_ptr as *mut libc::c_void, new_size) as *mut i32;

            if new_ptr.is_null() {
                return core::ptr::null_mut();
            }

            *new_ptr = new_capacity;
            new_ptr
        } else {
            array_ptr
        };

        // Add the element
        *final_ptr.offset(HEADER_SIZE + length as isize) = element;
        *final_ptr.offset(1) = length + 1;

        final_ptr
    }
}

/// Pop element from array (returns popped value, or 0 if empty)
#[runtime_method(
    symbol = "$Array$pop",
    haxe_type = "Array",
    haxe_method = "pop",
    mutates = true
)]
pub extern "C" fn Array_pop(array_ptr: *mut i32) -> i32 {
    if array_ptr.is_null() {
        return 0;
    }

    unsafe {
        let length = *array_ptr.offset(1);
        if length == 0 {
            return 0;
        }

        let value = *array_ptr.offset(HEADER_SIZE + (length - 1) as isize);
        *array_ptr.offset(1) = length - 1;
        value
    }
}

/// Remove first element and shift (returns removed element, or 0 if empty)
#[runtime_method(
    symbol = "$Array$shift",
    haxe_type = "Array",
    haxe_method = "shift",
    mutates = true
)]
pub extern "C" fn Array_shift(array_ptr: *mut i32) -> i32 {
    if array_ptr.is_null() {
        return 0;
    }

    unsafe {
        let length = *array_ptr.offset(1);
        if length == 0 {
            return 0;
        }

        let first = *array_ptr.offset(HEADER_SIZE);

        // Shift all elements left
        libc::memmove(
            array_ptr.offset(HEADER_SIZE) as *mut libc::c_void,
            array_ptr.offset(HEADER_SIZE + 1) as *const libc::c_void,
            ((length - 1) as usize) * std::mem::size_of::<i32>(),
        );

        *array_ptr.offset(1) = length - 1;
        first
    }
}

/// Insert element at beginning and shift right (may reallocate)
#[runtime_method(
    symbol = "$Array$unshift",
    haxe_type = "Array",
    haxe_method = "unshift",
    mutates = true,
    returns_self = true
)]
pub extern "C" fn Array_unshift(array_ptr: *mut i32, element: i32) -> *mut i32 {
    if array_ptr.is_null() {
        return core::ptr::null_mut();
    }

    unsafe {
        let capacity = *array_ptr;
        let length = *array_ptr.offset(1);

        let final_ptr = if length + HEADER_SIZE as i32 >= capacity {
            let new_capacity = capacity * 2;
            let new_size = new_capacity as usize * std::mem::size_of::<i32>();
            let new_ptr = libc::realloc(array_ptr as *mut libc::c_void, new_size) as *mut i32;

            if new_ptr.is_null() {
                return core::ptr::null_mut();
            }

            *new_ptr = new_capacity;
            new_ptr
        } else {
            array_ptr
        };

        // Shift all elements right
        if length > 0 {
            libc::memmove(
                final_ptr.offset(HEADER_SIZE + 1) as *mut libc::c_void,
                final_ptr.offset(HEADER_SIZE) as *const libc::c_void,
                (length as usize) * std::mem::size_of::<i32>(),
            );
        }

        // Insert new element at beginning
        *final_ptr.offset(HEADER_SIZE) = element;
        *final_ptr.offset(1) = length + 1;

        final_ptr
    }
}

/// Remove element at index and shift (returns removed element, or 0 if out of bounds)
#[runtime_method(
    symbol = "$Array$remove",
    haxe_type = "Array",
    haxe_method = "remove",
    mutates = true
)]
pub extern "C" fn Array_remove(array_ptr: *mut i32, index: i32) -> i32 {
    if array_ptr.is_null() || index < 0 {
        return 0;
    }

    unsafe {
        let length = *array_ptr.offset(1);
        if index >= length {
            return 0;
        }

        let value = *array_ptr.offset(HEADER_SIZE + index as isize);

        // Shift elements after index left
        if index < length - 1 {
            libc::memmove(
                array_ptr.offset(HEADER_SIZE + index as isize) as *mut libc::c_void,
                array_ptr.offset(HEADER_SIZE + index as isize + 1) as *const libc::c_void,
                ((length - index - 1) as usize) * std::mem::size_of::<i32>(),
            );
        }

        *array_ptr.offset(1) = length - 1;
        value
    }
}

/// Insert element at index and shift right (may reallocate)
#[runtime_method(
    symbol = "$Array$insert",
    haxe_type = "Array",
    haxe_method = "insert",
    mutates = true,
    returns_self = true
)]
pub extern "C" fn Array_insert(array_ptr: *mut i32, index: i32, element: i32) -> *mut i32 {
    if array_ptr.is_null() || index < 0 {
        return core::ptr::null_mut();
    }

    unsafe {
        let capacity = *array_ptr;
        let length = *array_ptr.offset(1);

        // Clamp index to valid range
        let insert_index = if index > length { length } else { index };

        let final_ptr = if length + HEADER_SIZE as i32 >= capacity {
            let new_capacity = capacity * 2;
            let new_size = new_capacity as usize * std::mem::size_of::<i32>();
            let new_ptr = libc::realloc(array_ptr as *mut libc::c_void, new_size) as *mut i32;

            if new_ptr.is_null() {
                return core::ptr::null_mut();
            }

            *new_ptr = new_capacity;
            new_ptr
        } else {
            array_ptr
        };

        // Shift elements after insert position right
        if insert_index < length {
            libc::memmove(
                final_ptr.offset(HEADER_SIZE + insert_index as isize + 1) as *mut libc::c_void,
                final_ptr.offset(HEADER_SIZE + insert_index as isize) as *const libc::c_void,
                ((length - insert_index) as usize) * std::mem::size_of::<i32>(),
            );
        }

        *final_ptr.offset(HEADER_SIZE + insert_index as isize) = element;
        *final_ptr.offset(1) = length + 1;

        final_ptr
    }
}

// ============================================================================
// Array Queries
// ============================================================================

/// Find index of element (returns -1 if not found)
#[runtime_method(
    symbol = "$Array$indexOf",
    haxe_type = "Array",
    haxe_method = "indexOf"
)]
pub extern "C" fn Array_indexOf(array_ptr: *const i32, element: i32) -> i32 {
    if array_ptr.is_null() {
        return -1;
    }

    unsafe {
        let length = *array_ptr.offset(1);
        for i in 0..length {
            if *array_ptr.offset(HEADER_SIZE + i as isize) == element {
                return i;
            }
        }
        -1
    }
}

/// Check if array contains element
#[runtime_method(
    symbol = "$Array$contains",
    haxe_type = "Array",
    haxe_method = "contains"
)]
pub extern "C" fn Array_contains(array_ptr: *const i32, element: i32) -> i32 {
    if Array_indexOf(array_ptr, element) >= 0 { 1 } else { 0 }
}

// ============================================================================
// Array Utilities
// ============================================================================

/// Reverse array in place
#[runtime_method(
    symbol = "$Array$reverse",
    haxe_type = "Array",
    haxe_method = "reverse",
    mutates = true
)]
pub extern "C" fn Array_reverse(array_ptr: *mut i32) {
    if array_ptr.is_null() {
        return;
    }

    unsafe {
        let length = *array_ptr.offset(1);
        let mut left = 0;
        let mut right = length - 1;

        while left < right {
            let temp = *array_ptr.offset(HEADER_SIZE + left as isize);
            *array_ptr.offset(HEADER_SIZE + left as isize) =
                *array_ptr.offset(HEADER_SIZE + right as isize);
            *array_ptr.offset(HEADER_SIZE + right as isize) = temp;

            left += 1;
            right -= 1;
        }
    }
}

/// Create a shallow copy of array
#[runtime_method(
    symbol = "$Array$copy",
    haxe_type = "Array",
    haxe_method = "copy"
)]
pub extern "C" fn Array_copy(array_ptr: *const i32) -> *mut i32 {
    if array_ptr.is_null() {
        return core::ptr::null_mut();
    }

    unsafe {
        let capacity = *array_ptr;
        let length = *array_ptr.offset(1);

        let size = capacity as usize * std::mem::size_of::<i32>();
        let new_ptr = libc::malloc(size) as *mut i32;

        if new_ptr.is_null() {
            return core::ptr::null_mut();
        }

        libc::memcpy(
            new_ptr as *mut libc::c_void,
            array_ptr as *const libc::c_void,
            size,
        );

        new_ptr
    }
}

/// Free array memory
#[runtime_export("$Array$free")]
pub extern "C" fn Array_free(array_ptr: *mut i32) {
    if !array_ptr.is_null() {
        unsafe {
            libc::free(array_ptr as *mut libc::c_void);
        }
    }
}
