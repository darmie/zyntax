//! ZRTL Plugin Support
//!
//! Types and macros for creating ZRTL plugins that can be loaded by the Zyntax runtime.

use std::ffi::c_char;

/// Current ZRTL format version
pub const ZRTL_VERSION: u32 = 1;

/// Symbol entry in the ZRTL symbol table (C ABI compatible)
///
/// Matches the layout of `ZrtlSymbol` in `zrtl.h`.
#[repr(C)]
pub struct ZrtlSymbol {
    /// Symbol name (null-terminated C string)
    /// Convention: `$TypeName$method_name`
    pub name: *const c_char,
    /// Function pointer
    pub ptr: *const u8,
}

impl ZrtlSymbol {
    /// Create a null/sentinel symbol
    pub const fn null() -> Self {
        Self {
            name: std::ptr::null(),
            ptr: std::ptr::null(),
        }
    }

    /// Create a new symbol
    pub const fn new(name: *const c_char, ptr: *const u8) -> Self {
        Self { name, ptr }
    }
}

// SAFETY: ZrtlSymbol contains only immutable pointers to static data
unsafe impl Sync for ZrtlSymbol {}
unsafe impl Send for ZrtlSymbol {}

/// Plugin metadata (C ABI compatible)
///
/// Matches the layout of `ZrtlInfo` in `zrtl.h`.
#[repr(C)]
pub struct ZrtlInfo {
    /// ZRTL format version (must match ZRTL_VERSION)
    pub version: u32,
    /// Plugin name (null-terminated C string)
    pub name: *const c_char,
}

impl ZrtlInfo {
    /// Create plugin info
    pub const fn new(name: *const c_char) -> Self {
        Self {
            version: ZRTL_VERSION,
            name,
        }
    }
}

// SAFETY: ZrtlInfo contains only immutable data
unsafe impl Sync for ZrtlInfo {}
unsafe impl Send for ZrtlInfo {}

/// Symbol entry for the inventory crate
///
/// Used by the `#[zrtl_export]` attribute macro.
pub struct ZrtlSymbolEntry {
    /// Symbol name (with null terminator)
    pub name: &'static str,
    /// Function pointer
    pub ptr: *const u8,
}

// SAFETY: ZrtlSymbolEntry contains only static data
unsafe impl Sync for ZrtlSymbolEntry {}
unsafe impl Send for ZrtlSymbolEntry {}

// Register the inventory collector
inventory::collect!(ZrtlSymbolEntry);

/// Macro to define a ZRTL symbol table entry
///
/// # Example
///
/// ```ignore
/// ZRTL_SYMBOLS_BEGIN
///     zrtl_symbol!("$Array$push", array_push),
///     zrtl_symbol!("$Array$pop", array_pop),
/// ZRTL_SYMBOLS_END
/// ```
#[macro_export]
macro_rules! zrtl_symbol {
    ($name:expr, $func:ident) => {
        $crate::ZrtlSymbol::new(
            concat!($name, "\0").as_ptr() as *const ::std::ffi::c_char,
            $func as *const u8,
        )
    };
}

/// Macro to define a complete ZRTL plugin
///
/// This creates both the symbol table and plugin info exports.
///
/// # Example
///
/// ```ignore
/// zrtl_plugin! {
///     name: "my_runtime",
///     symbols: [
///         ("$Math$add", math_add),
///         ("$Math$sub", math_sub),
///     ]
/// }
/// ```
#[macro_export]
macro_rules! zrtl_plugin {
    (name: $name:expr, symbols: [$( ($sym_name:expr, $func:ident) ),* $(,)?]) => {
        // Plugin info export
        #[no_mangle]
        pub static _zrtl_info: $crate::ZrtlInfo = $crate::ZrtlInfo::new(
            concat!($name, "\0").as_ptr() as *const ::std::ffi::c_char
        );

        // Symbol table export
        #[no_mangle]
        pub static _zrtl_symbols: &[$crate::ZrtlSymbol] = &[
            $(
                $crate::zrtl_symbol!($sym_name, $func),
            )*
            $crate::ZrtlSymbol::null(), // Sentinel
        ];
    };
}

/// Trait for types that can be registered as ZRTL types
pub trait ZrtlTyped: Sized {
    /// Get the type name
    fn type_name() -> &'static str;

    /// Get the type size
    fn type_size() -> usize {
        std::mem::size_of::<Self>()
    }

    /// Get the type alignment
    fn type_alignment() -> usize {
        std::mem::align_of::<Self>()
    }

    /// Get the type category
    fn type_category() -> crate::TypeCategory;
}

/// Type info for registered custom types
#[derive(Debug, Clone)]
pub struct TypeInfo {
    /// Type name
    pub name: &'static str,
    /// Size in bytes
    pub size: u32,
    /// Alignment requirement
    pub alignment: u32,
    /// Type category
    pub category: crate::TypeCategory,
}

impl TypeInfo {
    /// Create type info for a type implementing ZrtlTyped
    pub fn from_typed<T: ZrtlTyped>() -> Self {
        Self {
            name: T::type_name(),
            size: T::type_size() as u32,
            alignment: T::type_alignment() as u32,
            category: T::type_category(),
        }
    }
}

// Implement ZrtlTyped for primitive types
macro_rules! impl_zrtl_typed_primitive {
    ($ty:ty, $category:ident) => {
        impl ZrtlTyped for $ty {
            fn type_name() -> &'static str {
                stringify!($ty)
            }

            fn type_category() -> crate::TypeCategory {
                crate::TypeCategory::$category
            }
        }
    };
}

impl_zrtl_typed_primitive!(bool, Bool);
impl_zrtl_typed_primitive!(i8, Int);
impl_zrtl_typed_primitive!(i16, Int);
impl_zrtl_typed_primitive!(i32, Int);
impl_zrtl_typed_primitive!(i64, Int);
impl_zrtl_typed_primitive!(isize, Int);
impl_zrtl_typed_primitive!(u8, UInt);
impl_zrtl_typed_primitive!(u16, UInt);
impl_zrtl_typed_primitive!(u32, UInt);
impl_zrtl_typed_primitive!(u64, UInt);
impl_zrtl_typed_primitive!(usize, UInt);
impl_zrtl_typed_primitive!(f32, Float);
impl_zrtl_typed_primitive!(f64, Float);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_symbol_layout() {
        // Verify C ABI compatibility
        assert_eq!(
            std::mem::size_of::<ZrtlSymbol>(),
            std::mem::size_of::<*const u8>() * 2
        );
    }

    #[test]
    fn test_info_layout() {
        // Verify C ABI compatibility - should be 16 bytes on 64-bit (4 byte version + padding + 8 byte pointer)
        assert!(std::mem::size_of::<ZrtlInfo>() >= 12);
    }

    #[test]
    fn test_type_info() {
        let info = TypeInfo::from_typed::<i32>();
        assert_eq!(info.name, "i32");
        assert_eq!(info.size, 4);
        assert_eq!(info.category, crate::TypeCategory::Int);
    }
}
