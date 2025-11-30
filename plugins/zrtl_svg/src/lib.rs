//! ZRTL SVG Plugin - SVG parsing and rendering
//!
//! This plugin provides SVG parsing and rasterization using resvg/usvg.
//!
//! ## Parsing
//! - `$Svg$parse` - Parse SVG from string
//! - `$Svg$parse_file` - Parse SVG from file
//! - `$Svg$free` - Free SVG tree
//!
//! ## Information
//! - `$Svg$get_width` - Get SVG width
//! - `$Svg$get_height` - Get SVG height
//!
//! ## Rendering
//! - `$Svg$render` - Render at natural size
//! - `$Svg$render_scaled` - Render with scale factor
//!
//! ## Pixmap Operations
//! - `$Svg$pixmap_free` - Free rendered pixmap
//! - `$Svg$pixmap_width` - Get pixmap width
//! - `$Svg$pixmap_height` - Get pixmap height
//! - `$Svg$pixmap_data` - Get raw RGBA pixel data
//! - `$Svg$pixmap_save_png` - Save to PNG file

use std::collections::HashMap;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Mutex, OnceLock};

use resvg::tiny_skia::{Pixmap, Transform};
use usvg::{Options, Tree, TreeParsing};
use zrtl::{
    zrtl_plugin, StringPtr, ArrayPtr,
    string_as_str, array_new, array_push,
};

// ============================================================================
// Handle Management
// ============================================================================

static NEXT_SVG_ID: AtomicU64 = AtomicU64::new(1);
static NEXT_PIXMAP_ID: AtomicU64 = AtomicU64::new(1);

// Store SVG data as strings since Tree is not Send/Sync
static SVG_DATA: OnceLock<Mutex<HashMap<u64, String>>> = OnceLock::new();
static PIXMAP_MAP: OnceLock<Mutex<HashMap<u64, Pixmap>>> = OnceLock::new();

fn get_svg_data() -> &'static Mutex<HashMap<u64, String>> {
    SVG_DATA.get_or_init(|| Mutex::new(HashMap::new()))
}

fn get_pixmap_map() -> &'static Mutex<HashMap<u64, Pixmap>> {
    PIXMAP_MAP.get_or_init(|| Mutex::new(HashMap::new()))
}

// ============================================================================
// FFI Types
// ============================================================================

/// Render result with dimensions
#[repr(C)]
pub struct RenderResult {
    pub pixmap_handle: u64,
    pub width: u32,
    pub height: u32,
}

// ============================================================================
// SVG Parsing
// ============================================================================

/// Parse SVG from a string
/// Returns handle to SVG, or 0 on error
#[no_mangle]
pub extern "C" fn svg_parse(data: StringPtr) -> u64 {
    let svg_str = match unsafe { string_as_str(data) } {
        Some(s) => s,
        None => return 0,
    };

    // Validate by parsing
    let opt = Options::default();
    if Tree::from_str(svg_str, &opt).is_err() {
        return 0;
    }

    let id = NEXT_SVG_ID.fetch_add(1, Ordering::SeqCst);
    get_svg_data().lock().unwrap().insert(id, svg_str.to_string());
    id
}

/// Parse SVG from a file
/// Returns handle to SVG, or 0 on error
#[no_mangle]
pub extern "C" fn svg_parse_file(path: StringPtr) -> u64 {
    let path_str = match unsafe { string_as_str(path) } {
        Some(s) => s,
        None => return 0,
    };

    let svg_data = match std::fs::read_to_string(path_str) {
        Ok(d) => d,
        Err(_) => return 0,
    };

    // Validate by parsing
    let opt = Options::default();
    if Tree::from_str(&svg_data, &opt).is_err() {
        return 0;
    }

    let id = NEXT_SVG_ID.fetch_add(1, Ordering::SeqCst);
    get_svg_data().lock().unwrap().insert(id, svg_data);
    id
}

/// Free an SVG
#[no_mangle]
pub extern "C" fn svg_free(handle: u64) {
    get_svg_data().lock().unwrap().remove(&handle);
}

// ============================================================================
// SVG Information
// ============================================================================

/// Get the width of an SVG
#[no_mangle]
pub extern "C" fn svg_get_width(handle: u64) -> f32 {
    let map = get_svg_data().lock().unwrap();
    if let Some(svg_str) = map.get(&handle) {
        let opt = Options::default();
        if let Ok(tree) = Tree::from_str(svg_str, &opt) {
            return tree.size.width();
        }
    }
    0.0
}

/// Get the height of an SVG
#[no_mangle]
pub extern "C" fn svg_get_height(handle: u64) -> f32 {
    let map = get_svg_data().lock().unwrap();
    if let Some(svg_str) = map.get(&handle) {
        let opt = Options::default();
        if let Ok(tree) = Tree::from_str(svg_str, &opt) {
            return tree.size.height();
        }
    }
    0.0
}

// ============================================================================
// SVG Rendering
// ============================================================================

/// Render SVG at its natural size
/// Returns pixmap handle, or 0 on error
#[no_mangle]
pub extern "C" fn svg_render(handle: u64) -> RenderResult {
    let svg_str = {
        let map = get_svg_data().lock().unwrap();
        match map.get(&handle) {
            Some(s) => s.clone(),
            None => return RenderResult { pixmap_handle: 0, width: 0, height: 0 },
        }
    };

    let opt = Options::default();
    let tree = match Tree::from_str(&svg_str, &opt) {
        Ok(t) => t,
        Err(_) => return RenderResult { pixmap_handle: 0, width: 0, height: 0 },
    };

    let width = tree.size.width().ceil() as u32;
    let height = tree.size.height().ceil() as u32;

    if let Some(mut pixmap) = Pixmap::new(width, height) {
        let rtree = resvg::Tree::from_usvg(&tree);
        rtree.render(Transform::default(), &mut pixmap.as_mut());

        let id = NEXT_PIXMAP_ID.fetch_add(1, Ordering::SeqCst);
        get_pixmap_map().lock().unwrap().insert(id, pixmap);

        RenderResult {
            pixmap_handle: id,
            width,
            height,
        }
    } else {
        RenderResult { pixmap_handle: 0, width: 0, height: 0 }
    }
}

/// Render SVG with a specific scale factor
#[no_mangle]
pub extern "C" fn svg_render_scaled(handle: u64, scale: f32) -> RenderResult {
    let svg_str = {
        let map = get_svg_data().lock().unwrap();
        match map.get(&handle) {
            Some(s) => s.clone(),
            None => return RenderResult { pixmap_handle: 0, width: 0, height: 0 },
        }
    };

    let opt = Options::default();
    let tree = match Tree::from_str(&svg_str, &opt) {
        Ok(t) => t,
        Err(_) => return RenderResult { pixmap_handle: 0, width: 0, height: 0 },
    };

    let width = (tree.size.width() * scale).ceil() as u32;
    let height = (tree.size.height() * scale).ceil() as u32;

    if let Some(mut pixmap) = Pixmap::new(width, height) {
        let rtree = resvg::Tree::from_usvg(&tree);
        rtree.render(Transform::from_scale(scale, scale), &mut pixmap.as_mut());

        let id = NEXT_PIXMAP_ID.fetch_add(1, Ordering::SeqCst);
        get_pixmap_map().lock().unwrap().insert(id, pixmap);

        RenderResult {
            pixmap_handle: id,
            width,
            height,
        }
    } else {
        RenderResult { pixmap_handle: 0, width: 0, height: 0 }
    }
}

// ============================================================================
// Pixmap Operations
// ============================================================================

/// Free a rendered pixmap
#[no_mangle]
pub extern "C" fn svg_pixmap_free(handle: u64) {
    get_pixmap_map().lock().unwrap().remove(&handle);
}

/// Get the width of a pixmap
#[no_mangle]
pub extern "C" fn svg_pixmap_width(handle: u64) -> u32 {
    get_pixmap_map()
        .lock()
        .unwrap()
        .get(&handle)
        .map(|p| p.width())
        .unwrap_or(0)
}

/// Get the height of a pixmap
#[no_mangle]
pub extern "C" fn svg_pixmap_height(handle: u64) -> u32 {
    get_pixmap_map()
        .lock()
        .unwrap()
        .get(&handle)
        .map(|p| p.height())
        .unwrap_or(0)
}

/// Get raw pixel data from a pixmap (RGBA format)
/// Returns pointer to pixel data, or null on error
/// The data is owned by the pixmap - do not free it
#[no_mangle]
pub extern "C" fn svg_pixmap_data(handle: u64) -> *const u8 {
    let map = get_pixmap_map().lock().unwrap();
    if let Some(pixmap) = map.get(&handle) {
        pixmap.data().as_ptr()
    } else {
        std::ptr::null()
    }
}

/// Get the size of the pixel data buffer
#[no_mangle]
pub extern "C" fn svg_pixmap_data_len(handle: u64) -> u64 {
    get_pixmap_map()
        .lock()
        .unwrap()
        .get(&handle)
        .map(|p| p.data().len() as u64)
        .unwrap_or(0)
}

/// Save a pixmap to a PNG file
/// Returns 1 on success, 0 on error
#[no_mangle]
pub extern "C" fn svg_pixmap_save_png(handle: u64, path: StringPtr) -> i32 {
    let path_str = match unsafe { string_as_str(path) } {
        Some(s) => s,
        None => return 0,
    };

    let map = get_pixmap_map().lock().unwrap();
    if let Some(pixmap) = map.get(&handle) {
        match pixmap.save_png(path_str) {
            Ok(_) => 1,
            Err(_) => 0,
        }
    } else {
        0
    }
}

/// Encode a pixmap to PNG bytes
/// Returns array of bytes, or null on error
#[no_mangle]
pub extern "C" fn svg_pixmap_encode_png(handle: u64) -> ArrayPtr {
    let map = get_pixmap_map().lock().unwrap();
    if let Some(pixmap) = map.get(&handle) {
        match pixmap.encode_png() {
            Ok(data) => {
                let arr = array_new::<u8>(data.len());
                if !arr.is_null() {
                    unsafe {
                        for byte in data {
                            array_push(arr, byte);
                        }
                    }
                }
                arr
            }
            Err(_) => std::ptr::null_mut(),
        }
    } else {
        std::ptr::null_mut()
    }
}

/// Get version string
#[no_mangle]
pub extern "C" fn svg_version() -> *const std::ffi::c_char {
    static VERSION: &[u8] = b"0.1.0\0";
    VERSION.as_ptr() as *const std::ffi::c_char
}

// ============================================================================
// Plugin Export
// ============================================================================

zrtl_plugin! {
    name: "zrtl_svg",
    symbols: [
        // Parsing
        ("$Svg$parse", svg_parse),
        ("$Svg$parse_file", svg_parse_file),
        ("$Svg$free", svg_free),

        // Information
        ("$Svg$get_width", svg_get_width),
        ("$Svg$get_height", svg_get_height),

        // Rendering
        ("$Svg$render", svg_render),
        ("$Svg$render_scaled", svg_render_scaled),

        // Pixmap Operations
        ("$Svg$pixmap_free", svg_pixmap_free),
        ("$Svg$pixmap_width", svg_pixmap_width),
        ("$Svg$pixmap_height", svg_pixmap_height),
        ("$Svg$pixmap_data", svg_pixmap_data),
        ("$Svg$pixmap_data_len", svg_pixmap_data_len),
        ("$Svg$pixmap_save_png", svg_pixmap_save_png),
        ("$Svg$pixmap_encode_png", svg_pixmap_encode_png),

        // Version
        ("$Svg$version", svg_version),
    ]
}

#[cfg(test)]
mod tests {
    use super::*;
    use zrtl::string_new;

    #[test]
    fn test_parse_simple_svg() {
        let svg = string_new(r#"<svg xmlns="http://www.w3.org/2000/svg" width="100" height="100"><rect width="100" height="100" fill="red"/></svg>"#);
        let handle = svg_parse(svg);
        assert_ne!(handle, 0);

        let width = svg_get_width(handle);
        let height = svg_get_height(handle);
        assert_eq!(width, 100.0);
        assert_eq!(height, 100.0);

        svg_free(handle);
    }

    #[test]
    fn test_render_svg() {
        let svg = string_new(r#"<svg xmlns="http://www.w3.org/2000/svg" width="50" height="50"><circle cx="25" cy="25" r="20" fill="blue"/></svg>"#);
        let handle = svg_parse(svg);
        assert_ne!(handle, 0);

        let result = svg_render(handle);
        assert_ne!(result.pixmap_handle, 0);
        assert_eq!(result.width, 50);
        assert_eq!(result.height, 50);

        let data = svg_pixmap_data(result.pixmap_handle);
        assert!(!data.is_null());

        let len = svg_pixmap_data_len(result.pixmap_handle);
        assert_eq!(len, 50 * 50 * 4); // RGBA

        svg_pixmap_free(result.pixmap_handle);
        svg_free(handle);
    }

    #[test]
    fn test_render_scaled() {
        let svg = string_new(r#"<svg xmlns="http://www.w3.org/2000/svg" width="100" height="100"><rect width="100" height="100" fill="green"/></svg>"#);
        let handle = svg_parse(svg);
        assert_ne!(handle, 0);

        let result = svg_render_scaled(handle, 2.0);
        assert_ne!(result.pixmap_handle, 0);
        assert_eq!(result.width, 200);
        assert_eq!(result.height, 200);

        svg_pixmap_free(result.pixmap_handle);
        svg_free(handle);
    }

    #[test]
    fn test_invalid_svg() {
        let svg = string_new("not valid svg");
        let handle = svg_parse(svg);
        assert_eq!(handle, 0);
    }
}
