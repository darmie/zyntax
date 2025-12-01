//! # ZRTL Tensor Plugin
//!
//! Provides tensor data structures and operations for machine learning workloads.
//! This is the foundational data structure for ZynML.
//!
//! ## Features
//!
//! - Multi-dimensional tensor with arbitrary shapes
//! - Multiple data types (f32, f64, i32, i64, etc.)
//! - Memory-efficient views and slicing
//! - Broadcasting support for element-wise operations
//! - Reference counting for shared tensors
//!
//! ## Memory Layout
//!
//! Tensors use a contiguous memory layout with row-major (C) ordering by default.
//! Strides allow for efficient views without data copying.

use std::alloc::{alloc_zeroed, dealloc, Layout as AllocLayout};
use std::ptr;
use std::sync::atomic::{AtomicUsize, Ordering};
use zrtl::zrtl_plugin;

// Import SIMD functions for optimized operations
use zrtl_simd::{
    vec_fill_f32, vec_sum_f32, vec_max_f32, vec_min_f32,
    vec_argmax_with_val_f32,
};

// ============================================================================
// Data Types
// ============================================================================

/// Tensor data type
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DType {
    F32 = 0,
    F64 = 1,
    I8 = 2,
    I16 = 3,
    I32 = 4,
    I64 = 5,
    U8 = 6,
    U16 = 7,
    U32 = 8,
    U64 = 9,
    Bool = 10,
    F16 = 11,  // Half precision (stored as u16)
    BF16 = 12, // Brain float16 (stored as u16)
}

impl DType {
    /// Get the size of this data type in bytes
    pub fn size_bytes(self) -> usize {
        match self {
            DType::Bool | DType::I8 | DType::U8 => 1,
            DType::I16 | DType::U16 | DType::F16 | DType::BF16 => 2,
            DType::I32 | DType::U32 | DType::F32 => 4,
            DType::I64 | DType::U64 | DType::F64 => 8,
        }
    }

    /// Get the alignment of this data type
    pub fn alignment(self) -> usize {
        self.size_bytes()
    }
}

/// Memory layout ordering
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MemoryLayout {
    RowMajor = 0,    // C-style, last dimension varies fastest
    ColumnMajor = 1, // Fortran-style, first dimension varies fastest
}

// ============================================================================
// Tensor Handle
// ============================================================================

/// Maximum number of dimensions supported
pub const MAX_DIMS: usize = 8;

/// Internal tensor storage with reference counting
struct TensorStorage {
    /// Reference count
    refcount: AtomicUsize,
    /// Data pointer (owned)
    data: *mut u8,
    /// Total size in bytes
    size_bytes: usize,
    /// Memory layout for deallocation
    alloc_layout: AllocLayout,
}

impl TensorStorage {
    fn new(size_bytes: usize, dtype: DType) -> Option<*mut Self> {
        if size_bytes == 0 {
            return None;
        }

        let align = dtype.alignment().max(8);
        let alloc_layout = AllocLayout::from_size_align(size_bytes, align).ok()?;

        let data = unsafe { alloc_zeroed(alloc_layout) };
        if data.is_null() {
            return None;
        }

        let storage = Box::new(TensorStorage {
            refcount: AtomicUsize::new(1),
            data,
            size_bytes,
            alloc_layout,
        });

        Some(Box::into_raw(storage))
    }

    fn increment_refcount(ptr: *mut Self) {
        if !ptr.is_null() {
            unsafe {
                (*ptr).refcount.fetch_add(1, Ordering::Relaxed);
            }
        }
    }

    fn decrement_refcount(ptr: *mut Self) -> bool {
        if ptr.is_null() {
            return false;
        }

        unsafe {
            if (*ptr).refcount.fetch_sub(1, Ordering::Release) == 1 {
                std::sync::atomic::fence(Ordering::Acquire);
                // Free the data
                dealloc((*ptr).data, (*ptr).alloc_layout);
                // Free the storage struct
                drop(Box::from_raw(ptr));
                return true;
            }
        }
        false
    }
}

/// Tensor handle - C ABI compatible
#[repr(C)]
pub struct TensorHandle {
    /// Pointer to storage (reference counted)
    storage: *mut TensorStorage,
    /// Data pointer (may be offset for views)
    data: *mut u8,
    /// Shape array
    shape: [usize; MAX_DIMS],
    /// Strides array (in elements, not bytes)
    strides: [isize; MAX_DIMS],
    /// Number of dimensions
    ndim: u32,
    /// Data type
    dtype: DType,
    /// Memory layout
    layout: MemoryLayout,
    /// Flags
    flags: u32,
}

// Tensor flags
const FLAG_CONTIGUOUS: u32 = 1 << 0;
const FLAG_OWNS_DATA: u32 = 1 << 1;
const FLAG_WRITEABLE: u32 = 1 << 2;

impl TensorHandle {
    /// Check if tensor is contiguous in memory
    pub fn is_contiguous(&self) -> bool {
        self.flags & FLAG_CONTIGUOUS != 0
    }

    /// Check if tensor owns its data
    pub fn owns_data(&self) -> bool {
        self.flags & FLAG_OWNS_DATA != 0
    }

    /// Check if tensor is writeable
    pub fn is_writeable(&self) -> bool {
        self.flags & FLAG_WRITEABLE != 0
    }

    /// Get the total number of elements
    pub fn numel(&self) -> usize {
        let mut n = 1usize;
        for i in 0..self.ndim as usize {
            n = n.saturating_mul(self.shape[i]);
        }
        n
    }

    /// Get the total size in bytes
    pub fn size_bytes(&self) -> usize {
        self.numel() * self.dtype.size_bytes()
    }

    /// Calculate strides for a contiguous tensor
    fn calculate_strides(shape: &[usize], layout: MemoryLayout) -> [isize; MAX_DIMS] {
        let mut strides = [0isize; MAX_DIMS];
        let ndim = shape.len();

        if ndim == 0 {
            return strides;
        }

        match layout {
            MemoryLayout::RowMajor => {
                strides[ndim - 1] = 1;
                for i in (0..ndim - 1).rev() {
                    strides[i] = strides[i + 1] * shape[i + 1] as isize;
                }
            }
            MemoryLayout::ColumnMajor => {
                strides[0] = 1;
                for i in 1..ndim {
                    strides[i] = strides[i - 1] * shape[i - 1] as isize;
                }
            }
        }

        strides
    }

    /// Check if strides represent contiguous memory
    fn check_contiguous(shape: &[usize], strides: &[isize], layout: MemoryLayout) -> bool {
        let expected = Self::calculate_strides(shape, layout);
        for i in 0..shape.len() {
            if strides[i] != expected[i] {
                return false;
            }
        }
        true
    }
}

// ============================================================================
// Handle Type for FFI
// ============================================================================

/// Opaque handle type for FFI
pub type TensorPtr = *mut TensorHandle;

/// Null tensor constant
pub const TENSOR_NULL: TensorPtr = std::ptr::null_mut();

// ============================================================================
// Creation Functions
// ============================================================================

/// Create a new tensor with uninitialized data
#[no_mangle]
pub extern "C" fn tensor_new(
    shape_ptr: *const usize,
    ndim: u32,
    dtype: u8,
) -> TensorPtr {
    if shape_ptr.is_null() || ndim == 0 || ndim as usize > MAX_DIMS {
        return TENSOR_NULL;
    }

    let dtype = match dtype {
        0 => DType::F32,
        1 => DType::F64,
        2 => DType::I8,
        3 => DType::I16,
        4 => DType::I32,
        5 => DType::I64,
        6 => DType::U8,
        7 => DType::U16,
        8 => DType::U32,
        9 => DType::U64,
        10 => DType::Bool,
        11 => DType::F16,
        12 => DType::BF16,
        _ => return TENSOR_NULL,
    };

    // Copy shape
    let mut shape = [0usize; MAX_DIMS];
    let mut numel = 1usize;
    for i in 0..ndim as usize {
        unsafe {
            shape[i] = *shape_ptr.add(i);
            numel = numel.saturating_mul(shape[i]);
        }
    }

    if numel == 0 {
        return TENSOR_NULL;
    }

    let size_bytes = numel * dtype.size_bytes();
    let storage = match TensorStorage::new(size_bytes, dtype) {
        Some(s) => s,
        None => return TENSOR_NULL,
    };

    let strides = TensorHandle::calculate_strides(&shape[..ndim as usize], MemoryLayout::RowMajor);

    let handle = Box::new(TensorHandle {
        storage,
        data: unsafe { (*storage).data },
        shape,
        strides,
        ndim,
        dtype,
        layout: MemoryLayout::RowMajor,
        flags: FLAG_CONTIGUOUS | FLAG_OWNS_DATA | FLAG_WRITEABLE,
    });

    Box::into_raw(handle)
}

/// Create a tensor filled with zeros
#[no_mangle]
pub extern "C" fn tensor_zeros(
    shape_ptr: *const usize,
    ndim: u32,
    dtype: u8,
) -> TensorPtr {
    // tensor_new already uses alloc_zeroed
    tensor_new(shape_ptr, ndim, dtype)
}

/// Create a tensor filled with ones
#[no_mangle]
pub extern "C" fn tensor_ones(
    shape_ptr: *const usize,
    ndim: u32,
    dtype: u8,
) -> TensorPtr {
    let tensor = tensor_new(shape_ptr, ndim, dtype);
    if tensor.is_null() {
        return TENSOR_NULL;
    }

    unsafe {
        let t = &*tensor;
        let numel = t.numel();

        match t.dtype {
            DType::F32 => {
                // Use SIMD-optimized fill
                vec_fill_f32(t.data as *mut f32, 1.0, numel as u64);
            }
            DType::F64 => {
                let ptr = t.data as *mut f64;
                for i in 0..numel {
                    *ptr.add(i) = 1.0;
                }
            }
            DType::I32 => {
                let ptr = t.data as *mut i32;
                for i in 0..numel {
                    *ptr.add(i) = 1;
                }
            }
            DType::I64 => {
                let ptr = t.data as *mut i64;
                for i in 0..numel {
                    *ptr.add(i) = 1;
                }
            }
            DType::U8 | DType::Bool => {
                // Use memset for single-byte types
                ptr::write_bytes(t.data, 1, numel);
            }
            _ => {
                // For other types, write 1 as bytes
                let elem_size = t.dtype.size_bytes();
                for i in 0..numel {
                    let ptr = t.data.add(i * elem_size);
                    match elem_size {
                        1 => *ptr = 1,
                        2 => *(ptr as *mut u16) = 1,
                        4 => *(ptr as *mut u32) = 1,
                        8 => *(ptr as *mut u64) = 1,
                        _ => {}
                    }
                }
            }
        }
    }

    tensor
}

/// Create a tensor filled with a scalar value
#[no_mangle]
pub extern "C" fn tensor_full_f32(
    shape_ptr: *const usize,
    ndim: u32,
    value: f32,
) -> TensorPtr {
    let tensor = tensor_new(shape_ptr, ndim, DType::F32 as u8);
    if tensor.is_null() {
        return TENSOR_NULL;
    }

    unsafe {
        let t = &*tensor;
        let numel = t.numel();
        // Use SIMD-optimized fill
        vec_fill_f32(t.data as *mut f32, value, numel as u64);
    }

    tensor
}

/// Create a tensor with values from 0 to n-1
#[no_mangle]
pub extern "C" fn tensor_arange_f32(start: f32, end: f32, step: f32) -> TensorPtr {
    if step == 0.0 || (end - start) / step < 0.0 {
        return TENSOR_NULL;
    }

    let n = ((end - start) / step).ceil() as usize;
    if n == 0 {
        return TENSOR_NULL;
    }

    let shape = [n];
    let tensor = tensor_new(shape.as_ptr(), 1, DType::F32 as u8);
    if tensor.is_null() {
        return TENSOR_NULL;
    }

    unsafe {
        let t = &*tensor;
        let ptr = t.data as *mut f32;
        for i in 0..n {
            *ptr.add(i) = start + (i as f32) * step;
        }
    }

    tensor
}

/// Create a tensor with n evenly spaced values between start and end
#[no_mangle]
pub extern "C" fn tensor_linspace_f32(start: f32, end: f32, n: usize) -> TensorPtr {
    if n == 0 {
        return TENSOR_NULL;
    }

    let shape = [n];
    let tensor = tensor_new(shape.as_ptr(), 1, DType::F32 as u8);
    if tensor.is_null() {
        return TENSOR_NULL;
    }

    unsafe {
        let t = &*tensor;
        let ptr = t.data as *mut f32;
        if n == 1 {
            *ptr = start;
        } else {
            let step = (end - start) / (n - 1) as f32;
            for i in 0..n {
                *ptr.add(i) = start + (i as f32) * step;
            }
        }
    }

    tensor
}

/// Create a tensor with random values from uniform distribution [0, 1)
#[no_mangle]
pub extern "C" fn tensor_rand_f32(
    shape_ptr: *const usize,
    ndim: u32,
    seed: u64,
) -> TensorPtr {
    let tensor = tensor_new(shape_ptr, ndim, DType::F32 as u8);
    if tensor.is_null() {
        return TENSOR_NULL;
    }

    // Simple xorshift64 PRNG
    let mut state = if seed == 0 { 0x853c49e6748fea9b } else { seed };

    unsafe {
        let t = &*tensor;
        let ptr = t.data as *mut f32;
        let numel = t.numel();

        for i in 0..numel {
            state ^= state << 13;
            state ^= state >> 7;
            state ^= state << 17;
            // Convert to [0, 1)
            let val = (state & 0x7FFFFF) as f32 / (0x800000 as f32);
            *ptr.add(i) = val;
        }
    }

    tensor
}

/// Create a tensor with random values from standard normal distribution
#[no_mangle]
pub extern "C" fn tensor_randn_f32(
    shape_ptr: *const usize,
    ndim: u32,
    seed: u64,
) -> TensorPtr {
    let tensor = tensor_new(shape_ptr, ndim, DType::F32 as u8);
    if tensor.is_null() {
        return TENSOR_NULL;
    }

    // Simple xorshift64 PRNG
    let mut state = if seed == 0 { 0x853c49e6748fea9b } else { seed };

    unsafe {
        let t = &*tensor;
        let ptr = t.data as *mut f32;
        let numel = t.numel();

        // Box-Muller transform for normal distribution
        let mut i = 0;
        while i < numel {
            // Generate two uniform random numbers
            state ^= state << 13;
            state ^= state >> 7;
            state ^= state << 17;
            let u1 = ((state & 0x7FFFFF) as f32 / (0x800000 as f32)).max(1e-10);

            state ^= state << 13;
            state ^= state >> 7;
            state ^= state << 17;
            let u2 = (state & 0x7FFFFF) as f32 / (0x800000 as f32);

            // Box-Muller transform
            let mag = (-2.0 * u1.ln()).sqrt();
            let z0 = mag * (2.0 * std::f32::consts::PI * u2).cos();
            let z1 = mag * (2.0 * std::f32::consts::PI * u2).sin();

            *ptr.add(i) = z0;
            if i + 1 < numel {
                *ptr.add(i + 1) = z1;
            }
            i += 2;
        }
    }

    tensor
}

// ============================================================================
// Memory Management
// ============================================================================

/// Free a tensor and its resources
#[no_mangle]
pub extern "C" fn tensor_free(tensor: TensorPtr) {
    if tensor.is_null() {
        return;
    }

    unsafe {
        let t = Box::from_raw(tensor);
        TensorStorage::decrement_refcount(t.storage);
    }
}

/// Clone a tensor (creates a new tensor with copied data)
#[no_mangle]
pub extern "C" fn tensor_clone(tensor: TensorPtr) -> TensorPtr {
    if tensor.is_null() {
        return TENSOR_NULL;
    }

    unsafe {
        let t = &*tensor;
        let new_tensor = tensor_new(t.shape.as_ptr(), t.ndim, t.dtype as u8);
        if new_tensor.is_null() {
            return TENSOR_NULL;
        }

        let new_t = &mut *new_tensor;

        if t.is_contiguous() {
            // Fast path: copy contiguous memory
            ptr::copy_nonoverlapping(t.data, new_t.data, t.size_bytes());
        } else {
            // Slow path: iterate through all elements
            let elem_size = t.dtype.size_bytes();
            let numel = t.numel();
            let shape = &t.shape[..t.ndim as usize];

            // Use indices to iterate
            let mut indices = vec![0usize; t.ndim as usize];
            for flat in 0..numel {
                // Calculate source offset using strides
                let mut src_offset = 0isize;
                for d in 0..t.ndim as usize {
                    src_offset += (indices[d] as isize) * t.strides[d];
                }

                // Copy element
                ptr::copy_nonoverlapping(
                    t.data.offset(src_offset * elem_size as isize),
                    new_t.data.add(flat * elem_size),
                    elem_size,
                );

                // Increment indices
                for d in (0..t.ndim as usize).rev() {
                    indices[d] += 1;
                    if indices[d] < shape[d] {
                        break;
                    }
                    indices[d] = 0;
                }
            }
        }

        new_tensor
    }
}

/// Create a view of a tensor (shares data, increments refcount)
#[no_mangle]
pub extern "C" fn tensor_view(tensor: TensorPtr) -> TensorPtr {
    if tensor.is_null() {
        return TENSOR_NULL;
    }

    unsafe {
        let t = &*tensor;
        TensorStorage::increment_refcount(t.storage);

        let handle = Box::new(TensorHandle {
            storage: t.storage,
            data: t.data,
            shape: t.shape,
            strides: t.strides,
            ndim: t.ndim,
            dtype: t.dtype,
            layout: t.layout,
            flags: t.flags & !FLAG_OWNS_DATA,
        });

        Box::into_raw(handle)
    }
}

// ============================================================================
// Info Functions
// ============================================================================

/// Get number of dimensions
#[no_mangle]
pub extern "C" fn tensor_ndim(tensor: TensorPtr) -> u32 {
    if tensor.is_null() {
        return 0;
    }
    unsafe { (*tensor).ndim }
}

/// Get shape at dimension d
#[no_mangle]
pub extern "C" fn tensor_shape(tensor: TensorPtr, dim: u32) -> usize {
    if tensor.is_null() {
        return 0;
    }
    unsafe {
        let t = &*tensor;
        if dim >= t.ndim {
            return 0;
        }
        t.shape[dim as usize]
    }
}

/// Get stride at dimension d
#[no_mangle]
pub extern "C" fn tensor_stride(tensor: TensorPtr, dim: u32) -> isize {
    if tensor.is_null() {
        return 0;
    }
    unsafe {
        let t = &*tensor;
        if dim >= t.ndim {
            return 0;
        }
        t.strides[dim as usize]
    }
}

/// Get total number of elements
#[no_mangle]
pub extern "C" fn tensor_numel(tensor: TensorPtr) -> usize {
    if tensor.is_null() {
        return 0;
    }
    unsafe { (*tensor).numel() }
}

/// Get data type
#[no_mangle]
pub extern "C" fn tensor_dtype(tensor: TensorPtr) -> u8 {
    if tensor.is_null() {
        return 0;
    }
    unsafe { (*tensor).dtype as u8 }
}

/// Get raw data pointer
#[no_mangle]
pub extern "C" fn tensor_data(tensor: TensorPtr) -> *mut u8 {
    if tensor.is_null() {
        return std::ptr::null_mut();
    }
    unsafe { (*tensor).data }
}

/// Check if tensor is contiguous
#[no_mangle]
pub extern "C" fn tensor_is_contiguous(tensor: TensorPtr) -> bool {
    if tensor.is_null() {
        return false;
    }
    unsafe { (*tensor).is_contiguous() }
}

// ============================================================================
// Element Access
// ============================================================================

/// Get f32 element at flat index
#[no_mangle]
pub extern "C" fn tensor_get_f32(tensor: TensorPtr, index: usize) -> f32 {
    if tensor.is_null() {
        return 0.0;
    }
    unsafe {
        let t = &*tensor;
        if t.dtype != DType::F32 || index >= t.numel() {
            return 0.0;
        }
        *((t.data as *const f32).add(index))
    }
}

/// Set f32 element at flat index
#[no_mangle]
pub extern "C" fn tensor_set_f32(tensor: TensorPtr, index: usize, value: f32) {
    if tensor.is_null() {
        return;
    }
    unsafe {
        let t = &*tensor;
        if t.dtype != DType::F32 || index >= t.numel() || !t.is_writeable() {
            return;
        }
        *((t.data as *mut f32).add(index)) = value;
    }
}

/// Get element at multi-dimensional index for f32 tensor
#[no_mangle]
pub extern "C" fn tensor_get_at_f32(tensor: TensorPtr, indices: *const usize) -> f32 {
    if tensor.is_null() || indices.is_null() {
        return 0.0;
    }
    unsafe {
        let t = &*tensor;
        if t.dtype != DType::F32 {
            return 0.0;
        }

        // Calculate offset using strides
        let mut offset = 0isize;
        for d in 0..t.ndim as usize {
            let idx = *indices.add(d);
            if idx >= t.shape[d] {
                return 0.0;
            }
            offset += (idx as isize) * t.strides[d];
        }

        *((t.data as *const f32).offset(offset))
    }
}

/// Set element at multi-dimensional index for f32 tensor
#[no_mangle]
pub extern "C" fn tensor_set_at_f32(tensor: TensorPtr, indices: *const usize, value: f32) {
    if tensor.is_null() || indices.is_null() {
        return;
    }
    unsafe {
        let t = &*tensor;
        if t.dtype != DType::F32 || !t.is_writeable() {
            return;
        }

        // Calculate offset using strides
        let mut offset = 0isize;
        for d in 0..t.ndim as usize {
            let idx = *indices.add(d);
            if idx >= t.shape[d] {
                return;
            }
            offset += (idx as isize) * t.strides[d];
        }

        *((t.data as *mut f32).offset(offset)) = value;
    }
}

// ============================================================================
// Shape Operations
// ============================================================================

/// Reshape tensor (returns view if possible, clone if needed)
#[no_mangle]
pub extern "C" fn tensor_reshape(
    tensor: TensorPtr,
    new_shape: *const usize,
    new_ndim: u32,
) -> TensorPtr {
    if tensor.is_null() || new_shape.is_null() || new_ndim == 0 || new_ndim as usize > MAX_DIMS {
        return TENSOR_NULL;
    }

    unsafe {
        let t = &*tensor;

        // Calculate new total elements
        let mut new_numel = 1usize;
        let mut shape = [0usize; MAX_DIMS];
        for i in 0..new_ndim as usize {
            shape[i] = *new_shape.add(i);
            new_numel = new_numel.saturating_mul(shape[i]);
        }

        // Check that total elements match
        if new_numel != t.numel() {
            return TENSOR_NULL;
        }

        if t.is_contiguous() {
            // Create a view with new shape
            TensorStorage::increment_refcount(t.storage);

            let strides = TensorHandle::calculate_strides(&shape[..new_ndim as usize], t.layout);

            let handle = Box::new(TensorHandle {
                storage: t.storage,
                data: t.data,
                shape,
                strides,
                ndim: new_ndim,
                dtype: t.dtype,
                layout: t.layout,
                flags: t.flags & !FLAG_OWNS_DATA,
            });

            Box::into_raw(handle)
        } else {
            // Need to make contiguous first
            let contiguous = tensor_clone(tensor);
            if contiguous.is_null() {
                return TENSOR_NULL;
            }

            // Reshape the contiguous clone
            let c = &mut *contiguous;
            c.shape = shape;
            c.strides = TensorHandle::calculate_strides(&shape[..new_ndim as usize], c.layout);
            c.ndim = new_ndim;

            contiguous
        }
    }
}

/// Transpose tensor (swap two dimensions)
#[no_mangle]
pub extern "C" fn tensor_transpose(tensor: TensorPtr, dim0: u32, dim1: u32) -> TensorPtr {
    if tensor.is_null() {
        return TENSOR_NULL;
    }

    unsafe {
        let t = &*tensor;
        if dim0 >= t.ndim || dim1 >= t.ndim {
            return TENSOR_NULL;
        }

        TensorStorage::increment_refcount(t.storage);

        let mut shape = t.shape;
        let mut strides = t.strides;

        // Swap dimensions
        shape.swap(dim0 as usize, dim1 as usize);
        strides.swap(dim0 as usize, dim1 as usize);

        let is_contiguous = TensorHandle::check_contiguous(
            &shape[..t.ndim as usize],
            &strides[..t.ndim as usize],
            t.layout,
        );

        let handle = Box::new(TensorHandle {
            storage: t.storage,
            data: t.data,
            shape,
            strides,
            ndim: t.ndim,
            dtype: t.dtype,
            layout: t.layout,
            flags: if is_contiguous {
                (t.flags | FLAG_CONTIGUOUS) & !FLAG_OWNS_DATA
            } else {
                (t.flags & !FLAG_CONTIGUOUS) & !FLAG_OWNS_DATA
            },
        });

        Box::into_raw(handle)
    }
}

/// Squeeze - remove dimensions of size 1
#[no_mangle]
pub extern "C" fn tensor_squeeze(tensor: TensorPtr) -> TensorPtr {
    if tensor.is_null() {
        return TENSOR_NULL;
    }

    unsafe {
        let t = &*tensor;
        TensorStorage::increment_refcount(t.storage);

        let mut new_shape = [0usize; MAX_DIMS];
        let mut new_strides = [0isize; MAX_DIMS];
        let mut new_ndim = 0u32;

        for d in 0..t.ndim as usize {
            if t.shape[d] != 1 {
                new_shape[new_ndim as usize] = t.shape[d];
                new_strides[new_ndim as usize] = t.strides[d];
                new_ndim += 1;
            }
        }

        // Handle scalar case
        if new_ndim == 0 {
            new_ndim = 1;
            new_shape[0] = 1;
            new_strides[0] = 1;
        }

        let handle = Box::new(TensorHandle {
            storage: t.storage,
            data: t.data,
            shape: new_shape,
            strides: new_strides,
            ndim: new_ndim,
            dtype: t.dtype,
            layout: t.layout,
            flags: t.flags & !FLAG_OWNS_DATA,
        });

        Box::into_raw(handle)
    }
}

/// Unsqueeze - add dimension of size 1 at position
#[no_mangle]
pub extern "C" fn tensor_unsqueeze(tensor: TensorPtr, dim: u32) -> TensorPtr {
    if tensor.is_null() {
        return TENSOR_NULL;
    }

    unsafe {
        let t = &*tensor;
        if dim > t.ndim || t.ndim as usize >= MAX_DIMS {
            return TENSOR_NULL;
        }

        TensorStorage::increment_refcount(t.storage);

        let mut new_shape = [0usize; MAX_DIMS];
        let mut new_strides = [0isize; MAX_DIMS];
        let new_ndim = t.ndim + 1;

        // Copy dimensions before insertion point
        for d in 0..dim as usize {
            new_shape[d] = t.shape[d];
            new_strides[d] = t.strides[d];
        }

        // Insert new dimension
        new_shape[dim as usize] = 1;
        // Stride for size-1 dimension doesn't matter, use next dim's stride
        new_strides[dim as usize] = if dim < t.ndim {
            t.strides[dim as usize]
        } else if t.ndim > 0 {
            1
        } else {
            1
        };

        // Copy dimensions after insertion point
        for d in dim as usize..t.ndim as usize {
            new_shape[d + 1] = t.shape[d];
            new_strides[d + 1] = t.strides[d];
        }

        let handle = Box::new(TensorHandle {
            storage: t.storage,
            data: t.data,
            shape: new_shape,
            strides: new_strides,
            ndim: new_ndim,
            dtype: t.dtype,
            layout: t.layout,
            flags: t.flags & !FLAG_OWNS_DATA,
        });

        Box::into_raw(handle)
    }
}

// ============================================================================
// Reduction Operations
// ============================================================================

/// Sum all elements
#[no_mangle]
pub extern "C" fn tensor_sum_f32(tensor: TensorPtr) -> f32 {
    if tensor.is_null() {
        return 0.0;
    }

    unsafe {
        let t = &*tensor;
        if t.dtype != DType::F32 {
            return 0.0;
        }

        let numel = t.numel();

        if t.is_contiguous() {
            // Use SIMD-optimized sum for contiguous tensors
            vec_sum_f32(t.data as *const f32, numel as u64)
        } else {
            // Iterate using indices for non-contiguous tensors
            let shape = &t.shape[..t.ndim as usize];
            let mut indices = vec![0usize; t.ndim as usize];
            let mut sum = 0.0f32;

            for _ in 0..numel {
                let mut offset = 0isize;
                for d in 0..t.ndim as usize {
                    offset += (indices[d] as isize) * t.strides[d];
                }
                sum += *((t.data as *const f32).offset(offset));

                // Increment indices
                for d in (0..t.ndim as usize).rev() {
                    indices[d] += 1;
                    if indices[d] < shape[d] {
                        break;
                    }
                    indices[d] = 0;
                }
            }
            sum
        }
    }
}

/// Mean of all elements
#[no_mangle]
pub extern "C" fn tensor_mean_f32(tensor: TensorPtr) -> f32 {
    if tensor.is_null() {
        return 0.0;
    }

    unsafe {
        let t = &*tensor;
        let numel = t.numel();
        if numel == 0 {
            return 0.0;
        }
        tensor_sum_f32(tensor) / numel as f32
    }
}

/// Maximum value
#[no_mangle]
pub extern "C" fn tensor_max_f32(tensor: TensorPtr) -> f32 {
    if tensor.is_null() {
        return f32::NEG_INFINITY;
    }

    unsafe {
        let t = &*tensor;
        if t.dtype != DType::F32 {
            return f32::NEG_INFINITY;
        }

        let numel = t.numel();
        if numel == 0 {
            return f32::NEG_INFINITY;
        }

        if t.is_contiguous() {
            // Use SIMD-optimized max for contiguous tensors
            vec_max_f32(t.data as *const f32, numel as u64)
        } else {
            let shape = &t.shape[..t.ndim as usize];
            let mut indices = vec![0usize; t.ndim as usize];
            let mut max = f32::NEG_INFINITY;

            for _ in 0..numel {
                let mut offset = 0isize;
                for d in 0..t.ndim as usize {
                    offset += (indices[d] as isize) * t.strides[d];
                }
                let val = *((t.data as *const f32).offset(offset));
                if val > max {
                    max = val;
                }

                for d in (0..t.ndim as usize).rev() {
                    indices[d] += 1;
                    if indices[d] < shape[d] {
                        break;
                    }
                    indices[d] = 0;
                }
            }
            max
        }
    }
}

/// Minimum value
#[no_mangle]
pub extern "C" fn tensor_min_f32(tensor: TensorPtr) -> f32 {
    if tensor.is_null() {
        return f32::INFINITY;
    }

    unsafe {
        let t = &*tensor;
        if t.dtype != DType::F32 {
            return f32::INFINITY;
        }

        let numel = t.numel();
        if numel == 0 {
            return f32::INFINITY;
        }

        if t.is_contiguous() {
            // Use SIMD-optimized min for contiguous tensors
            vec_min_f32(t.data as *const f32, numel as u64)
        } else {
            let shape = &t.shape[..t.ndim as usize];
            let mut indices = vec![0usize; t.ndim as usize];
            let mut min = f32::INFINITY;

            for _ in 0..numel {
                let mut offset = 0isize;
                for d in 0..t.ndim as usize {
                    offset += (indices[d] as isize) * t.strides[d];
                }
                let val = *((t.data as *const f32).offset(offset));
                if val < min {
                    min = val;
                }

                for d in (0..t.ndim as usize).rev() {
                    indices[d] += 1;
                    if indices[d] < shape[d] {
                        break;
                    }
                    indices[d] = 0;
                }
            }
            min
        }
    }
}

/// Argmax - index of maximum value
#[no_mangle]
pub extern "C" fn tensor_argmax_f32(tensor: TensorPtr) -> usize {
    if tensor.is_null() {
        return 0;
    }

    unsafe {
        let t = &*tensor;
        if t.dtype != DType::F32 {
            return 0;
        }

        let numel = t.numel();
        if numel == 0 {
            return 0;
        }

        if t.is_contiguous() {
            // Use SIMD-optimized argmax for contiguous tensors
            let mut idx: u64 = 0;
            let mut val: f32 = 0.0;
            vec_argmax_with_val_f32(t.data as *const f32, numel as u64, &mut idx, &mut val);
            idx as usize
        } else {
            let shape = &t.shape[..t.ndim as usize];
            let mut indices = vec![0usize; t.ndim as usize];
            let mut max = f32::NEG_INFINITY;
            let mut max_idx = 0;

            for flat in 0..numel {
                let mut offset = 0isize;
                for d in 0..t.ndim as usize {
                    offset += (indices[d] as isize) * t.strides[d];
                }
                let val = *((t.data as *const f32).offset(offset));
                if val > max {
                    max = val;
                    max_idx = flat;
                }

                for d in (0..t.ndim as usize).rev() {
                    indices[d] += 1;
                    if indices[d] < shape[d] {
                        break;
                    }
                    indices[d] = 0;
                }
            }
            max_idx
        }
    }
}

// ============================================================================
// Slicing
// ============================================================================

/// Slice tensor along dimension
#[no_mangle]
pub extern "C" fn tensor_slice(
    tensor: TensorPtr,
    dim: u32,
    start: usize,
    end: usize,
) -> TensorPtr {
    if tensor.is_null() {
        return TENSOR_NULL;
    }

    unsafe {
        let t = &*tensor;
        if dim >= t.ndim || start >= end || end > t.shape[dim as usize] {
            return TENSOR_NULL;
        }

        TensorStorage::increment_refcount(t.storage);

        let mut new_shape = t.shape;
        new_shape[dim as usize] = end - start;

        // Calculate new data pointer offset
        let offset_bytes =
            (start as isize) * t.strides[dim as usize] * (t.dtype.size_bytes() as isize);
        let new_data = t.data.offset(offset_bytes);

        let is_contiguous = TensorHandle::check_contiguous(
            &new_shape[..t.ndim as usize],
            &t.strides[..t.ndim as usize],
            t.layout,
        );

        let handle = Box::new(TensorHandle {
            storage: t.storage,
            data: new_data,
            shape: new_shape,
            strides: t.strides,
            ndim: t.ndim,
            dtype: t.dtype,
            layout: t.layout,
            flags: if is_contiguous {
                (t.flags | FLAG_CONTIGUOUS) & !FLAG_OWNS_DATA
            } else {
                (t.flags & !FLAG_CONTIGUOUS) & !FLAG_OWNS_DATA
            },
        });

        Box::into_raw(handle)
    }
}

// ============================================================================
// Type Conversion
// ============================================================================

/// Convert tensor to different dtype
#[no_mangle]
pub extern "C" fn tensor_to_dtype(tensor: TensorPtr, new_dtype: u8) -> TensorPtr {
    if tensor.is_null() {
        return TENSOR_NULL;
    }

    let dtype = match new_dtype {
        0 => DType::F32,
        1 => DType::F64,
        4 => DType::I32,
        5 => DType::I64,
        _ => return TENSOR_NULL,
    };

    unsafe {
        let t = &*tensor;
        if t.dtype as u8 == new_dtype {
            // Same dtype, just return a view
            return tensor_view(tensor);
        }

        let new_tensor = tensor_new(t.shape.as_ptr(), t.ndim, new_dtype);
        if new_tensor.is_null() {
            return TENSOR_NULL;
        }

        let new_t = &*new_tensor;
        let numel = t.numel();

        // Convert each element
        for i in 0..numel {
            let src_val: f64 = match t.dtype {
                DType::F32 => *((t.data as *const f32).add(i)) as f64,
                DType::F64 => *((t.data as *const f64).add(i)),
                DType::I32 => *((t.data as *const i32).add(i)) as f64,
                DType::I64 => *((t.data as *const i64).add(i)) as f64,
                _ => 0.0,
            };

            match dtype {
                DType::F32 => *((new_t.data as *mut f32).add(i)) = src_val as f32,
                DType::F64 => *((new_t.data as *mut f64).add(i)) = src_val,
                DType::I32 => *((new_t.data as *mut i32).add(i)) = src_val as i32,
                DType::I64 => *((new_t.data as *mut i64).add(i)) = src_val as i64,
                _ => {}
            }
        }

        new_tensor
    }
}

/// Make tensor contiguous (copy if needed)
#[no_mangle]
pub extern "C" fn tensor_contiguous(tensor: TensorPtr) -> TensorPtr {
    if tensor.is_null() {
        return TENSOR_NULL;
    }

    unsafe {
        let t = &*tensor;
        if t.is_contiguous() {
            return tensor_view(tensor);
        }

        tensor_clone(tensor)
    }
}

// ============================================================================
// Plugin Registration
// ============================================================================

zrtl_plugin! {
    name: "tensor",
    symbols: [
        // Creation
        ("$Tensor$new", tensor_new),
        ("$Tensor$zeros", tensor_zeros),
        ("$Tensor$ones", tensor_ones),
        ("$Tensor$full_f32", tensor_full_f32),
        ("$Tensor$arange_f32", tensor_arange_f32),
        ("$Tensor$linspace_f32", tensor_linspace_f32),
        ("$Tensor$rand_f32", tensor_rand_f32),
        ("$Tensor$randn_f32", tensor_randn_f32),

        // Memory
        ("$Tensor$free", tensor_free),
        ("$Tensor$clone", tensor_clone),
        ("$Tensor$view", tensor_view),
        ("$Tensor$contiguous", tensor_contiguous),

        // Info
        ("$Tensor$ndim", tensor_ndim),
        ("$Tensor$shape", tensor_shape),
        ("$Tensor$stride", tensor_stride),
        ("$Tensor$numel", tensor_numel),
        ("$Tensor$dtype", tensor_dtype),
        ("$Tensor$data", tensor_data),
        ("$Tensor$is_contiguous", tensor_is_contiguous),

        // Element access
        ("$Tensor$get_f32", tensor_get_f32),
        ("$Tensor$set_f32", tensor_set_f32),
        ("$Tensor$get_at_f32", tensor_get_at_f32),
        ("$Tensor$set_at_f32", tensor_set_at_f32),

        // Shape operations
        ("$Tensor$reshape", tensor_reshape),
        ("$Tensor$transpose", tensor_transpose),
        ("$Tensor$squeeze", tensor_squeeze),
        ("$Tensor$unsqueeze", tensor_unsqueeze),
        ("$Tensor$slice", tensor_slice),

        // Reductions
        ("$Tensor$sum_f32", tensor_sum_f32),
        ("$Tensor$mean_f32", tensor_mean_f32),
        ("$Tensor$max_f32", tensor_max_f32),
        ("$Tensor$min_f32", tensor_min_f32),
        ("$Tensor$argmax_f32", tensor_argmax_f32),

        // Type conversion
        ("$Tensor$to_dtype", tensor_to_dtype),
    ]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tensor_creation() {
        let shape = [2, 3];
        let tensor = tensor_new(shape.as_ptr(), 2, DType::F32 as u8);
        assert!(!tensor.is_null());

        unsafe {
            assert_eq!(tensor_ndim(tensor), 2);
            assert_eq!(tensor_shape(tensor, 0), 2);
            assert_eq!(tensor_shape(tensor, 1), 3);
            assert_eq!(tensor_numel(tensor), 6);
            assert!(tensor_is_contiguous(tensor));
        }

        tensor_free(tensor);
    }

    #[test]
    fn test_tensor_ones() {
        let shape = [3];
        let tensor = tensor_ones(shape.as_ptr(), 1, DType::F32 as u8);
        assert!(!tensor.is_null());

        for i in 0..3 {
            assert_eq!(tensor_get_f32(tensor, i), 1.0);
        }

        tensor_free(tensor);
    }

    #[test]
    fn test_tensor_arange() {
        let tensor = tensor_arange_f32(0.0, 5.0, 1.0);
        assert!(!tensor.is_null());

        assert_eq!(tensor_numel(tensor), 5);
        for i in 0..5 {
            assert_eq!(tensor_get_f32(tensor, i), i as f32);
        }

        tensor_free(tensor);
    }

    #[test]
    fn test_tensor_reshape() {
        let shape = [2, 3];
        let tensor = tensor_ones(shape.as_ptr(), 2, DType::F32 as u8);

        let new_shape = [6];
        let reshaped = tensor_reshape(tensor, new_shape.as_ptr(), 1);
        assert!(!reshaped.is_null());

        unsafe {
            assert_eq!(tensor_ndim(reshaped), 1);
            assert_eq!(tensor_shape(reshaped, 0), 6);
        }

        tensor_free(reshaped);
        tensor_free(tensor);
    }

    #[test]
    fn test_tensor_transpose() {
        let shape = [2, 3];
        let tensor = tensor_arange_f32(0.0, 6.0, 1.0);
        let reshaped = tensor_reshape(tensor, shape.as_ptr(), 2);
        let transposed = tensor_transpose(reshaped, 0, 1);

        unsafe {
            assert_eq!(tensor_shape(transposed, 0), 3);
            assert_eq!(tensor_shape(transposed, 1), 2);
        }

        tensor_free(transposed);
        tensor_free(reshaped);
        tensor_free(tensor);
    }

    #[test]
    fn test_tensor_sum() {
        let tensor = tensor_arange_f32(1.0, 6.0, 1.0);
        let sum = tensor_sum_f32(tensor);
        assert_eq!(sum, 15.0); // 1+2+3+4+5

        tensor_free(tensor);
    }

    #[test]
    fn test_tensor_slice() {
        let tensor = tensor_arange_f32(0.0, 10.0, 1.0);
        let sliced = tensor_slice(tensor, 0, 2, 7);

        assert_eq!(tensor_numel(sliced), 5);
        assert_eq!(tensor_get_f32(sliced, 0), 2.0);
        assert_eq!(tensor_get_f32(sliced, 4), 6.0);

        tensor_free(sliced);
        tensor_free(tensor);
    }

    #[test]
    fn test_tensor_clone() {
        let tensor = tensor_arange_f32(0.0, 5.0, 1.0);
        let cloned = tensor_clone(tensor);

        // Modify original
        tensor_set_f32(tensor, 0, 100.0);

        // Clone should be unchanged
        assert_eq!(tensor_get_f32(cloned, 0), 0.0);

        tensor_free(cloned);
        tensor_free(tensor);
    }
}
