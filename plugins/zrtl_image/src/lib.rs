//! ZRTL Image Plugin
//!
//! Image encoding and decoding for common formats.
//!
//! ## Supported Formats
//! - PNG, JPEG, GIF, WebP, BMP, ICO, TIFF
//!
//! ## Image Operations
//! - `$Image$load` - Load image from file
//! - `$Image$load_bytes` - Load image from byte array
//! - `$Image$save` - Save image to file
//! - `$Image$encode` - Encode image to byte array
//! - `$Image$create` - Create new blank image
//! - `$Image$free` - Free image handle
//!
//! ## Image Info
//! - `$Image$width` - Get image width
//! - `$Image$height` - Get image height
//! - `$Image$get_pixel` - Get pixel color at position
//! - `$Image$set_pixel` - Set pixel color at position
//!
//! ## Image Manipulation
//! - `$Image$resize` - Resize image
//! - `$Image$crop` - Crop image region
//! - `$Image$rotate90` - Rotate 90 degrees clockwise
//! - `$Image$rotate180` - Rotate 180 degrees
//! - `$Image$rotate270` - Rotate 270 degrees clockwise
//! - `$Image$flip_horizontal` - Flip horizontally
//! - `$Image$flip_vertical` - Flip vertically
//! - `$Image$grayscale` - Convert to grayscale
//! - `$Image$blur` - Apply Gaussian blur
//! - `$Image$brighten` - Adjust brightness
//! - `$Image$contrast` - Adjust contrast

use std::collections::HashMap;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Mutex, OnceLock};
use std::io::Cursor;
use image::{
    DynamicImage, GenericImageView, GenericImage, ImageFormat, Rgba,
    imageops::FilterType,
};
use zrtl::{
    zrtl_plugin, StringPtr, ArrayPtr,
    string_as_str, array_new, array_push, array_as_slice,
};

// ============================================================================
// Handle Management
// ============================================================================

static HANDLE_COUNTER: AtomicU64 = AtomicU64::new(1);
static IMAGES: OnceLock<Mutex<HashMap<u64, DynamicImage>>> = OnceLock::new();

fn get_images() -> &'static Mutex<HashMap<u64, DynamicImage>> {
    IMAGES.get_or_init(|| Mutex::new(HashMap::new()))
}

fn next_handle() -> u64 {
    HANDLE_COUNTER.fetch_add(1, Ordering::SeqCst)
}

// ============================================================================
// Format Constants
// ============================================================================

pub const FORMAT_PNG: i32 = 0;
pub const FORMAT_JPEG: i32 = 1;
pub const FORMAT_GIF: i32 = 2;
pub const FORMAT_WEBP: i32 = 3;
pub const FORMAT_BMP: i32 = 4;
pub const FORMAT_ICO: i32 = 5;
pub const FORMAT_TIFF: i32 = 6;

fn format_to_image_format(format: i32) -> Option<ImageFormat> {
    match format {
        FORMAT_PNG => Some(ImageFormat::Png),
        FORMAT_JPEG => Some(ImageFormat::Jpeg),
        FORMAT_GIF => Some(ImageFormat::Gif),
        FORMAT_WEBP => Some(ImageFormat::WebP),
        FORMAT_BMP => Some(ImageFormat::Bmp),
        FORMAT_ICO => Some(ImageFormat::Ico),
        FORMAT_TIFF => Some(ImageFormat::Tiff),
        _ => None,
    }
}

// ============================================================================
// Color Structure
// ============================================================================

/// RGBA color
#[repr(C)]
#[derive(Clone, Copy, Default)]
pub struct Color {
    pub r: u8,
    pub g: u8,
    pub b: u8,
    pub a: u8,
}

// ============================================================================
// Loading and Saving
// ============================================================================

/// Load image from file path
///
/// Returns image handle on success, 0 on error.
#[no_mangle]
pub extern "C" fn image_load(path: StringPtr) -> u64 {
    let path_str = match unsafe { string_as_str(path) } {
        Some(s) => s,
        None => return 0,
    };

    match image::open(path_str) {
        Ok(img) => {
            let handle = next_handle();
            if let Ok(mut images) = get_images().lock() {
                images.insert(handle, img);
            }
            handle
        }
        Err(_) => 0,
    }
}

/// Load image from byte array
///
/// Returns image handle on success, 0 on error.
#[no_mangle]
pub unsafe extern "C" fn image_load_bytes(data: ArrayPtr) -> u64 {
    if data.is_null() {
        return 0;
    }

    let bytes = array_as_slice::<u8>(data);

    match image::load_from_memory(bytes) {
        Ok(img) => {
            let handle = next_handle();
            if let Ok(mut images) = get_images().lock() {
                images.insert(handle, img);
            }
            handle
        }
        Err(_) => 0,
    }
}

/// Load image from bytes with explicit format
#[no_mangle]
pub unsafe extern "C" fn image_load_bytes_format(data: ArrayPtr, format: i32) -> u64 {
    if data.is_null() {
        return 0;
    }

    let bytes = array_as_slice::<u8>(data);
    let img_format = match format_to_image_format(format) {
        Some(f) => f,
        None => return 0,
    };

    match image::load_from_memory_with_format(bytes, img_format) {
        Ok(img) => {
            let handle = next_handle();
            if let Ok(mut images) = get_images().lock() {
                images.insert(handle, img);
            }
            handle
        }
        Err(_) => 0,
    }
}

/// Save image to file
///
/// Format is auto-detected from file extension.
/// Returns 0 on success, -1 on error.
#[no_mangle]
pub extern "C" fn image_save(handle: u64, path: StringPtr) -> i32 {
    let path_str = match unsafe { string_as_str(path) } {
        Some(s) => s,
        None => return -1,
    };

    if let Ok(images) = get_images().lock() {
        if let Some(img) = images.get(&handle) {
            match img.save(path_str) {
                Ok(()) => return 0,
                Err(_) => return -1,
            }
        }
    }
    -1
}

/// Save image to file with explicit format
#[no_mangle]
pub extern "C" fn image_save_format(handle: u64, path: StringPtr, format: i32) -> i32 {
    let path_str = match unsafe { string_as_str(path) } {
        Some(s) => s,
        None => return -1,
    };

    let img_format = match format_to_image_format(format) {
        Some(f) => f,
        None => return -1,
    };

    if let Ok(images) = get_images().lock() {
        if let Some(img) = images.get(&handle) {
            match img.save_with_format(path_str, img_format) {
                Ok(()) => return 0,
                Err(_) => return -1,
            }
        }
    }
    -1
}

/// Encode image to byte array
///
/// Returns byte array, or null on error.
#[no_mangle]
pub extern "C" fn image_encode(handle: u64, format: i32) -> ArrayPtr {
    let img_format = match format_to_image_format(format) {
        Some(f) => f,
        None => return std::ptr::null_mut(),
    };

    if let Ok(images) = get_images().lock() {
        if let Some(img) = images.get(&handle) {
            let mut buffer = Cursor::new(Vec::new());
            if img.write_to(&mut buffer, img_format).is_ok() {
                let bytes = buffer.into_inner();
                let arr = array_new::<u8>(bytes.len());
                if !arr.is_null() {
                    unsafe {
                        for byte in bytes {
                            array_push(arr, byte);
                        }
                    }
                    return arr;
                }
            }
        }
    }
    std::ptr::null_mut()
}

/// Encode image to PNG bytes (convenience function)
#[no_mangle]
pub extern "C" fn image_encode_png(handle: u64) -> ArrayPtr {
    image_encode(handle, FORMAT_PNG)
}

/// Encode image to JPEG bytes with quality (0-100)
#[no_mangle]
pub extern "C" fn image_encode_jpeg(handle: u64, quality: u8) -> ArrayPtr {
    // For JPEG with quality, we need to use a different approach
    if let Ok(images) = get_images().lock() {
        if let Some(img) = images.get(&handle) {
            let rgb = img.to_rgb8();
            let mut buffer = Cursor::new(Vec::new());

            let encoder = image::codecs::jpeg::JpegEncoder::new_with_quality(&mut buffer, quality);
            if rgb.write_with_encoder(encoder).is_ok() {
                let bytes = buffer.into_inner();
                let arr = array_new::<u8>(bytes.len());
                if !arr.is_null() {
                    unsafe {
                        for byte in bytes {
                            array_push(arr, byte);
                        }
                    }
                    return arr;
                }
            }
        }
    }
    std::ptr::null_mut()
}

// ============================================================================
// Image Creation and Info
// ============================================================================

/// Create a new blank image with RGBA
///
/// Returns image handle.
#[no_mangle]
pub extern "C" fn image_create(width: u32, height: u32) -> u64 {
    let img = DynamicImage::new_rgba8(width, height);
    let handle = next_handle();

    if let Ok(mut images) = get_images().lock() {
        images.insert(handle, img);
    }

    handle
}

/// Create image filled with a color
#[no_mangle]
pub extern "C" fn image_create_filled(width: u32, height: u32, color: Color) -> u64 {
    let mut img = image::RgbaImage::new(width, height);

    for pixel in img.pixels_mut() {
        *pixel = Rgba([color.r, color.g, color.b, color.a]);
    }

    let handle = next_handle();
    if let Ok(mut images) = get_images().lock() {
        images.insert(handle, DynamicImage::ImageRgba8(img));
    }

    handle
}

/// Free an image
#[no_mangle]
pub extern "C" fn image_free(handle: u64) {
    if let Ok(mut images) = get_images().lock() {
        images.remove(&handle);
    }
}

/// Clone an image
#[no_mangle]
pub extern "C" fn image_clone(handle: u64) -> u64 {
    if let Ok(images) = get_images().lock() {
        if let Some(img) = images.get(&handle) {
            let cloned = img.clone();
            let new_handle = next_handle();
            drop(images);

            if let Ok(mut images) = get_images().lock() {
                images.insert(new_handle, cloned);
            }
            return new_handle;
        }
    }
    0
}

/// Get image width
#[no_mangle]
pub extern "C" fn image_width(handle: u64) -> u32 {
    if let Ok(images) = get_images().lock() {
        if let Some(img) = images.get(&handle) {
            return img.width();
        }
    }
    0
}

/// Get image height
#[no_mangle]
pub extern "C" fn image_height(handle: u64) -> u32 {
    if let Ok(images) = get_images().lock() {
        if let Some(img) = images.get(&handle) {
            return img.height();
        }
    }
    0
}

// ============================================================================
// Pixel Operations
// ============================================================================

/// Get pixel color at position
#[no_mangle]
pub extern "C" fn image_get_pixel(handle: u64, x: u32, y: u32) -> Color {
    if let Ok(images) = get_images().lock() {
        if let Some(img) = images.get(&handle) {
            if x < img.width() && y < img.height() {
                let pixel = img.get_pixel(x, y);
                return Color {
                    r: pixel[0],
                    g: pixel[1],
                    b: pixel[2],
                    a: pixel[3],
                };
            }
        }
    }
    Color::default()
}

/// Set pixel color at position
#[no_mangle]
pub extern "C" fn image_set_pixel(handle: u64, x: u32, y: u32, color: Color) {
    if let Ok(mut images) = get_images().lock() {
        if let Some(img) = images.get_mut(&handle) {
            if x < img.width() && y < img.height() {
                img.put_pixel(x, y, Rgba([color.r, color.g, color.b, color.a]));
            }
        }
    }
}

/// Get raw pixel data as byte array (RGBA format)
#[no_mangle]
pub extern "C" fn image_get_pixels(handle: u64) -> ArrayPtr {
    if let Ok(images) = get_images().lock() {
        if let Some(img) = images.get(&handle) {
            let rgba = img.to_rgba8();
            let bytes = rgba.into_raw();
            let arr = array_new::<u8>(bytes.len());
            if !arr.is_null() {
                unsafe {
                    for byte in bytes {
                        array_push(arr, byte);
                    }
                }
                return arr;
            }
        }
    }
    std::ptr::null_mut()
}

/// Set raw pixel data from byte array (RGBA format)
#[no_mangle]
pub unsafe extern "C" fn image_set_pixels(handle: u64, data: ArrayPtr) {
    if data.is_null() {
        return;
    }

    if let Ok(mut images) = get_images().lock() {
        if let Some(img) = images.get_mut(&handle) {
            let bytes = array_as_slice::<u8>(data);
            let width = img.width();
            let height = img.height();

            if bytes.len() == (width * height * 4) as usize {
                if let Some(rgba) = image::RgbaImage::from_raw(width, height, bytes.to_vec()) {
                    *img = DynamicImage::ImageRgba8(rgba);
                }
            }
        }
    }
}

// ============================================================================
// Image Manipulation
// ============================================================================

/// Resize image (returns new handle)
#[no_mangle]
pub extern "C" fn image_resize(handle: u64, width: u32, height: u32) -> u64 {
    if let Ok(images) = get_images().lock() {
        if let Some(img) = images.get(&handle) {
            let resized = img.resize_exact(width, height, FilterType::Lanczos3);
            let new_handle = next_handle();
            drop(images);

            if let Ok(mut images) = get_images().lock() {
                images.insert(new_handle, resized);
            }
            return new_handle;
        }
    }
    0
}

/// Resize with aspect ratio preserved
#[no_mangle]
pub extern "C" fn image_resize_fit(handle: u64, max_width: u32, max_height: u32) -> u64 {
    if let Ok(images) = get_images().lock() {
        if let Some(img) = images.get(&handle) {
            let resized = img.resize(max_width, max_height, FilterType::Lanczos3);
            let new_handle = next_handle();
            drop(images);

            if let Ok(mut images) = get_images().lock() {
                images.insert(new_handle, resized);
            }
            return new_handle;
        }
    }
    0
}

/// Crop image region (returns new handle)
#[no_mangle]
pub extern "C" fn image_crop(handle: u64, x: u32, y: u32, width: u32, height: u32) -> u64 {
    if let Ok(images) = get_images().lock() {
        if let Some(img) = images.get(&handle) {
            let cropped = img.crop_imm(x, y, width, height);
            let new_handle = next_handle();
            drop(images);

            if let Ok(mut images) = get_images().lock() {
                images.insert(new_handle, cropped);
            }
            return new_handle;
        }
    }
    0
}

/// Rotate 90 degrees clockwise
#[no_mangle]
pub extern "C" fn image_rotate90(handle: u64) -> u64 {
    if let Ok(images) = get_images().lock() {
        if let Some(img) = images.get(&handle) {
            let rotated = img.rotate90();
            let new_handle = next_handle();
            drop(images);

            if let Ok(mut images) = get_images().lock() {
                images.insert(new_handle, rotated);
            }
            return new_handle;
        }
    }
    0
}

/// Rotate 180 degrees
#[no_mangle]
pub extern "C" fn image_rotate180(handle: u64) -> u64 {
    if let Ok(images) = get_images().lock() {
        if let Some(img) = images.get(&handle) {
            let rotated = img.rotate180();
            let new_handle = next_handle();
            drop(images);

            if let Ok(mut images) = get_images().lock() {
                images.insert(new_handle, rotated);
            }
            return new_handle;
        }
    }
    0
}

/// Rotate 270 degrees clockwise (90 counter-clockwise)
#[no_mangle]
pub extern "C" fn image_rotate270(handle: u64) -> u64 {
    if let Ok(images) = get_images().lock() {
        if let Some(img) = images.get(&handle) {
            let rotated = img.rotate270();
            let new_handle = next_handle();
            drop(images);

            if let Ok(mut images) = get_images().lock() {
                images.insert(new_handle, rotated);
            }
            return new_handle;
        }
    }
    0
}

/// Flip horizontally
#[no_mangle]
pub extern "C" fn image_flip_horizontal(handle: u64) -> u64 {
    if let Ok(images) = get_images().lock() {
        if let Some(img) = images.get(&handle) {
            let flipped = img.fliph();
            let new_handle = next_handle();
            drop(images);

            if let Ok(mut images) = get_images().lock() {
                images.insert(new_handle, flipped);
            }
            return new_handle;
        }
    }
    0
}

/// Flip vertically
#[no_mangle]
pub extern "C" fn image_flip_vertical(handle: u64) -> u64 {
    if let Ok(images) = get_images().lock() {
        if let Some(img) = images.get(&handle) {
            let flipped = img.flipv();
            let new_handle = next_handle();
            drop(images);

            if let Ok(mut images) = get_images().lock() {
                images.insert(new_handle, flipped);
            }
            return new_handle;
        }
    }
    0
}

/// Convert to grayscale
#[no_mangle]
pub extern "C" fn image_grayscale(handle: u64) -> u64 {
    if let Ok(images) = get_images().lock() {
        if let Some(img) = images.get(&handle) {
            let gray = img.grayscale();
            let new_handle = next_handle();
            drop(images);

            if let Ok(mut images) = get_images().lock() {
                images.insert(new_handle, gray);
            }
            return new_handle;
        }
    }
    0
}

/// Apply Gaussian blur with sigma
#[no_mangle]
pub extern "C" fn image_blur(handle: u64, sigma: f32) -> u64 {
    if let Ok(images) = get_images().lock() {
        if let Some(img) = images.get(&handle) {
            let blurred = img.blur(sigma);
            let new_handle = next_handle();
            drop(images);

            if let Ok(mut images) = get_images().lock() {
                images.insert(new_handle, blurred);
            }
            return new_handle;
        }
    }
    0
}

/// Adjust brightness (positive = brighter, negative = darker)
#[no_mangle]
pub extern "C" fn image_brighten(handle: u64, amount: i32) -> u64 {
    if let Ok(images) = get_images().lock() {
        if let Some(img) = images.get(&handle) {
            let brightened = img.brighten(amount);
            let new_handle = next_handle();
            drop(images);

            if let Ok(mut images) = get_images().lock() {
                images.insert(new_handle, brightened);
            }
            return new_handle;
        }
    }
    0
}

/// Adjust contrast (positive = more, negative = less)
#[no_mangle]
pub extern "C" fn image_contrast(handle: u64, amount: f32) -> u64 {
    if let Ok(images) = get_images().lock() {
        if let Some(img) = images.get(&handle) {
            let adjusted = img.adjust_contrast(amount);
            let new_handle = next_handle();
            drop(images);

            if let Ok(mut images) = get_images().lock() {
                images.insert(new_handle, adjusted);
            }
            return new_handle;
        }
    }
    0
}

/// Invert colors
#[no_mangle]
pub extern "C" fn image_invert(handle: u64) -> u64 {
    if let Ok(images) = get_images().lock() {
        if let Some(img) = images.get(&handle) {
            let mut inverted = img.clone();
            inverted.invert();
            let new_handle = next_handle();
            drop(images);

            if let Ok(mut images) = get_images().lock() {
                images.insert(new_handle, inverted);
            }
            return new_handle;
        }
    }
    0
}

// ============================================================================
// Plugin Export
// ============================================================================

zrtl_plugin! {
    name: "zrtl_image",
    symbols: [
        // Loading/Saving (with signatures)
        // Functions with opaque params (StringPtr/ArrayPtr) use dynamic boxing
        // image_load(path: StringPtr) -> u64 (handle)
        ("$Image$load", image_load, dynamic(1) -> void),
        // image_load_bytes(data: ArrayPtr) -> u64
        ("$Image$load_bytes", image_load_bytes, dynamic(1) -> void),
        // image_load_bytes_format(data: ArrayPtr, format: i32) -> u64
        ("$Image$load_bytes_format", image_load_bytes_format, dynamic(2) -> void),
        // image_save(handle: u64, path: StringPtr) -> void
        ("$Image$save", image_save, dynamic(2) -> void),
        // image_save_format(handle: u64, path: StringPtr, format: i32) -> i32
        ("$Image$save_format", image_save_format, dynamic(3) -> void),
        // image_encode(handle: u64, format: i32) -> ArrayPtr
        ("$Image$encode", image_encode, dynamic(2) -> dynamic),
        // image_encode_png(handle: u64) -> ArrayPtr
        ("$Image$encode_png", image_encode_png, dynamic(1) -> dynamic),
        // image_encode_jpeg(handle: u64, quality: u8) -> ArrayPtr
        ("$Image$encode_jpeg", image_encode_jpeg, dynamic(2) -> dynamic),

        // Creation/Info (with signatures)
        // image_create(width: u32, height: u32) -> u64
        ("$Image$create", image_create, (u32, u32) -> u64),
        // image_create_filled(width: u32, height: u32, color: Color) -> u64
        // Note: Color is a struct, no signature for now
        ("$Image$create_filled", image_create_filled),
        // image_free(handle: u64) -> void
        ("$Image$free", image_free, (u64) -> void),
        // image_clone(handle: u64) -> u64
        ("$Image$clone", image_clone, (u64) -> u64),
        // image_width(handle: u64) -> u32
        ("$Image$width", image_width, (u64) -> u32),
        // image_height(handle: u64) -> u32
        ("$Image$height", image_height, (u64) -> u32),

        // Pixel operations (some without signatures due to struct params)
        ("$Image$get_pixel", image_get_pixel),
        ("$Image$set_pixel", image_set_pixel),
        // image_get_pixels(handle: u64) -> ArrayPtr
        ("$Image$get_pixels", image_get_pixels, dynamic(1) -> dynamic),
        ("$Image$set_pixels", image_set_pixels),

        // Manipulation (with signatures)
        // image_resize(handle: u64, width: u32, height: u32) -> u64
        ("$Image$resize", image_resize, (u64, u32, u32) -> u64),
        // image_resize_fit(handle: u64, max_width: u32, max_height: u32) -> u64
        ("$Image$resize_fit", image_resize_fit, (u64, u32, u32) -> u64),
        // image_crop(handle: u64, x: u32, y: u32, width: u32, height: u32) -> u64
        ("$Image$crop", image_crop, (u64, u32, u32, u32, u32) -> u64),
        // image_rotate90(handle: u64) -> u64
        ("$Image$rotate90", image_rotate90, (u64) -> u64),
        // image_rotate180(handle: u64) -> u64
        ("$Image$rotate180", image_rotate180, (u64) -> u64),
        // image_rotate270(handle: u64) -> u64
        ("$Image$rotate270", image_rotate270, (u64) -> u64),
        // image_flip_horizontal(handle: u64) -> u64
        ("$Image$flip_horizontal", image_flip_horizontal, (u64) -> u64),
        // image_flip_vertical(handle: u64) -> u64
        ("$Image$flip_vertical", image_flip_vertical, (u64) -> u64),
        // image_grayscale(handle: u64) -> u64
        ("$Image$grayscale", image_grayscale, (u64) -> u64),
        // image_blur(handle: u64, sigma: f32) -> u64
        ("$Image$blur", image_blur, (u64, f32) -> u64),
        // image_brighten(handle: u64, value: i32) -> u64
        ("$Image$brighten", image_brighten, (u64, i32) -> u64),
        // image_contrast(handle: u64, value: f32) -> u64
        ("$Image$contrast", image_contrast, (u64, f32) -> u64),
        // image_invert(handle: u64) -> u64
        ("$Image$invert", image_invert, (u64) -> u64),
    ]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_create_image() {
        let handle = image_create(100, 100);
        assert!(handle > 0);
        assert_eq!(image_width(handle), 100);
        assert_eq!(image_height(handle), 100);
        image_free(handle);
    }

    #[test]
    fn test_pixel_operations() {
        let handle = image_create(10, 10);

        let color = Color { r: 255, g: 128, b: 64, a: 255 };
        image_set_pixel(handle, 5, 5, color);

        let pixel = image_get_pixel(handle, 5, 5);
        assert_eq!(pixel.r, 255);
        assert_eq!(pixel.g, 128);
        assert_eq!(pixel.b, 64);
        assert_eq!(pixel.a, 255);

        image_free(handle);
    }

    #[test]
    fn test_image_resize() {
        let handle = image_create(100, 100);
        let resized = image_resize(handle, 50, 50);

        assert!(resized > 0);
        assert_eq!(image_width(resized), 50);
        assert_eq!(image_height(resized), 50);

        image_free(handle);
        image_free(resized);
    }

    #[test]
    fn test_image_rotate() {
        let handle = image_create(100, 50);
        let rotated = image_rotate90(handle);

        assert!(rotated > 0);
        assert_eq!(image_width(rotated), 50);
        assert_eq!(image_height(rotated), 100);

        image_free(handle);
        image_free(rotated);
    }
}
