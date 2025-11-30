//! ZRTL Paint Plugin
//!
//! 2D software rasterization using tiny-skia for creative programming,
//! charts, and general 2D graphics.
//!
//! ## Canvas Operations
//! - `$Paint$canvas_create` - Create a new canvas
//! - `$Paint$canvas_free` - Free canvas
//! - `$Paint$canvas_clear` - Clear canvas with color
//! - `$Paint$canvas_get_pixels` - Get raw pixel data (copies to array)
//! - `$Paint$canvas_data_ptr` - Get raw pointer to pixel buffer (for SDL/Window rendering)
//! - `$Paint$canvas_data_len` - Get pixel buffer length in bytes
//! - `$Paint$canvas_pitch` - Get row stride (bytes per row)
//! - `$Paint$canvas_save_png` - Save canvas to PNG file
//!
//! ## Drawing Primitives
//! - `$Paint$fill_rect` - Fill rectangle
//! - `$Paint$stroke_rect` - Stroke rectangle
//! - `$Paint$fill_circle` - Fill circle
//! - `$Paint$stroke_circle` - Stroke circle
//! - `$Paint$fill_ellipse` - Fill ellipse
//! - `$Paint$stroke_ellipse` - Stroke ellipse
//! - `$Paint$fill_rounded_rect` - Fill rounded rectangle
//! - `$Paint$stroke_rounded_rect` - Stroke rounded rectangle
//!
//! ## Path Operations
//! - `$Paint$path_create` - Create new path
//! - `$Paint$path_move_to` - Move to point
//! - `$Paint$path_line_to` - Line to point
//! - `$Paint$path_quad_to` - Quadratic bezier
//! - `$Paint$path_cubic_to` - Cubic bezier
//! - `$Paint$path_arc_to` - Arc
//! - `$Paint$path_close` - Close path
//! - `$Paint$path_fill` - Fill path
//! - `$Paint$path_stroke` - Stroke path
//! - `$Paint$path_free` - Free path
//!
//! ## Lines
//! - `$Paint$line` - Draw line
//! - `$Paint$polyline` - Draw connected lines
//! - `$Paint$polygon` - Draw filled polygon
//!
//! ## Transform
//! - `$Paint$transform_reset` - Reset transform
//! - `$Paint$transform_translate` - Translate
//! - `$Paint$transform_rotate` - Rotate (radians)
//! - `$Paint$transform_scale` - Scale

use std::collections::HashMap;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Mutex, OnceLock};
use tiny_skia::{
    Pixmap, Paint, Color, PathBuilder, Stroke, FillRule, Transform,
    LineCap, LineJoin,
};
use zrtl::{
    zrtl_plugin, StringPtr, ArrayPtr,
    string_as_str, array_new, array_push,
};

// ============================================================================
// Handle Management
// ============================================================================

static HANDLE_COUNTER: AtomicU64 = AtomicU64::new(1);
static CANVASES: OnceLock<Mutex<HashMap<u64, CanvasState>>> = OnceLock::new();
static PATHS: OnceLock<Mutex<HashMap<u64, PathBuilder>>> = OnceLock::new();

fn get_canvases() -> &'static Mutex<HashMap<u64, CanvasState>> {
    CANVASES.get_or_init(|| Mutex::new(HashMap::new()))
}

fn get_paths() -> &'static Mutex<HashMap<u64, PathBuilder>> {
    PATHS.get_or_init(|| Mutex::new(HashMap::new()))
}

fn next_handle() -> u64 {
    HANDLE_COUNTER.fetch_add(1, Ordering::SeqCst)
}

struct CanvasState {
    pixmap: Pixmap,
    transform: Transform,
    stroke_width: f32,
    line_cap: LineCap,
    line_join: LineJoin,
}

// ============================================================================
// Color
// ============================================================================

/// RGBA color (0-255 range)
#[repr(C)]
#[derive(Clone, Copy, Default)]
pub struct PaintColor {
    pub r: u8,
    pub g: u8,
    pub b: u8,
    pub a: u8,
}

impl PaintColor {
    fn to_color(self) -> Color {
        Color::from_rgba8(self.r, self.g, self.b, self.a)
    }
}

/// Create color from RGBA values
#[no_mangle]
pub extern "C" fn paint_rgba(r: u8, g: u8, b: u8, a: u8) -> PaintColor {
    PaintColor { r, g, b, a }
}

/// Create color from RGB values (alpha = 255)
#[no_mangle]
pub extern "C" fn paint_rgb(r: u8, g: u8, b: u8) -> PaintColor {
    PaintColor { r, g, b, a: 255 }
}

/// Create color from hex value (0xRRGGBB or 0xRRGGBBAA)
#[no_mangle]
pub extern "C" fn paint_hex(hex: u32) -> PaintColor {
    if hex > 0xFFFFFF {
        PaintColor {
            r: ((hex >> 24) & 0xFF) as u8,
            g: ((hex >> 16) & 0xFF) as u8,
            b: ((hex >> 8) & 0xFF) as u8,
            a: (hex & 0xFF) as u8,
        }
    } else {
        PaintColor {
            r: ((hex >> 16) & 0xFF) as u8,
            g: ((hex >> 8) & 0xFF) as u8,
            b: (hex & 0xFF) as u8,
            a: 255,
        }
    }
}

// ============================================================================
// Canvas Operations
// ============================================================================

/// Create a new canvas
#[no_mangle]
pub extern "C" fn canvas_create(width: u32, height: u32) -> u64 {
    match Pixmap::new(width, height) {
        Some(pixmap) => {
            let handle = next_handle();
            let state = CanvasState {
                pixmap,
                transform: Transform::identity(),
                stroke_width: 1.0,
                line_cap: LineCap::Butt,
                line_join: LineJoin::Miter,
            };

            if let Ok(mut canvases) = get_canvases().lock() {
                canvases.insert(handle, state);
            }
            handle
        }
        None => 0,
    }
}

/// Free a canvas
#[no_mangle]
pub extern "C" fn canvas_free(handle: u64) {
    if let Ok(mut canvases) = get_canvases().lock() {
        canvases.remove(&handle);
    }
}

/// Get canvas width
#[no_mangle]
pub extern "C" fn canvas_width(handle: u64) -> u32 {
    if let Ok(canvases) = get_canvases().lock() {
        if let Some(state) = canvases.get(&handle) {
            return state.pixmap.width();
        }
    }
    0
}

/// Get canvas height
#[no_mangle]
pub extern "C" fn canvas_height(handle: u64) -> u32 {
    if let Ok(canvases) = get_canvases().lock() {
        if let Some(state) = canvases.get(&handle) {
            return state.pixmap.height();
        }
    }
    0
}

/// Clear canvas with color
#[no_mangle]
pub extern "C" fn canvas_clear(handle: u64, color: PaintColor) {
    if let Ok(mut canvases) = get_canvases().lock() {
        if let Some(state) = canvases.get_mut(&handle) {
            state.pixmap.fill(color.to_color());
        }
    }
}

/// Get raw pixel data (RGBA)
#[no_mangle]
pub extern "C" fn canvas_get_pixels(handle: u64) -> ArrayPtr {
    if let Ok(canvases) = get_canvases().lock() {
        if let Some(state) = canvases.get(&handle) {
            let data = state.pixmap.data();
            let arr = array_new::<u8>(data.len());
            if !arr.is_null() {
                unsafe {
                    for &byte in data {
                        array_push(arr, byte);
                    }
                }
                return arr;
            }
        }
    }
    std::ptr::null_mut()
}

/// Save canvas to PNG file
#[no_mangle]
pub extern "C" fn canvas_save_png(handle: u64, path: StringPtr) -> i32 {
    let path_str = match unsafe { string_as_str(path) } {
        Some(s) => s,
        None => return -1,
    };

    if let Ok(canvases) = get_canvases().lock() {
        if let Some(state) = canvases.get(&handle) {
            match state.pixmap.save_png(path_str) {
                Ok(()) => return 0,
                Err(_) => return -1,
            }
        }
    }
    -1
}

/// Encode canvas to PNG bytes
#[no_mangle]
pub extern "C" fn canvas_encode_png(handle: u64) -> ArrayPtr {
    if let Ok(canvases) = get_canvases().lock() {
        if let Some(state) = canvases.get(&handle) {
            match state.pixmap.encode_png() {
                Ok(bytes) => {
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
                Err(_) => {}
            }
        }
    }
    std::ptr::null_mut()
}

// ============================================================================
// Buffer Access (for SDL/Window rendering)
// ============================================================================

/// Buffer info for rendering to SDL/Window
/// Contains all info needed to create/update a texture
#[repr(C)]
#[derive(Clone, Copy, Default)]
pub struct CanvasBuffer {
    /// Pointer to raw RGBA pixel data (premultiplied alpha)
    pub data: *const u8,
    /// Length of data in bytes
    pub len: u64,
    /// Width in pixels
    pub width: u32,
    /// Height in pixels
    pub height: u32,
    /// Bytes per row (pitch/stride)
    pub pitch: u32,
}

/// Get buffer info for direct rendering
/// Returns a CanvasBuffer struct with pointer to pixel data
///
/// IMPORTANT: The returned pointer is only valid while the canvas exists
/// and while no drawing operations are in progress. Copy the data or
/// use it immediately for SDL texture updates.
///
/// The pixel format is RGBA with premultiplied alpha (tiny-skia format).
#[no_mangle]
pub extern "C" fn canvas_get_buffer(handle: u64) -> CanvasBuffer {
    if let Ok(canvases) = get_canvases().lock() {
        if let Some(state) = canvases.get(&handle) {
            let data = state.pixmap.data();
            return CanvasBuffer {
                data: data.as_ptr(),
                len: data.len() as u64,
                width: state.pixmap.width(),
                height: state.pixmap.height(),
                pitch: state.pixmap.width() * 4, // RGBA = 4 bytes per pixel
            };
        }
    }
    CanvasBuffer::default()
}

/// Get raw pointer to pixel data
/// Returns null if canvas doesn't exist
///
/// Pixel format: RGBA with premultiplied alpha
/// Use canvas_data_len() to get the buffer size
#[no_mangle]
pub extern "C" fn canvas_data_ptr(handle: u64) -> *const u8 {
    if let Ok(canvases) = get_canvases().lock() {
        if let Some(state) = canvases.get(&handle) {
            return state.pixmap.data().as_ptr();
        }
    }
    std::ptr::null()
}

/// Get mutable pointer to pixel data (for direct manipulation)
/// Returns null if canvas doesn't exist
#[no_mangle]
pub extern "C" fn canvas_data_ptr_mut(handle: u64) -> *mut u8 {
    if let Ok(mut canvases) = get_canvases().lock() {
        if let Some(state) = canvases.get_mut(&handle) {
            return state.pixmap.data_mut().as_mut_ptr();
        }
    }
    std::ptr::null_mut()
}

/// Get pixel buffer length in bytes
#[no_mangle]
pub extern "C" fn canvas_data_len(handle: u64) -> u64 {
    if let Ok(canvases) = get_canvases().lock() {
        if let Some(state) = canvases.get(&handle) {
            return state.pixmap.data().len() as u64;
        }
    }
    0
}

/// Get row stride (bytes per row)
/// For tiny-skia this is width * 4 (RGBA)
#[no_mangle]
pub extern "C" fn canvas_pitch(handle: u64) -> u32 {
    if let Ok(canvases) = get_canvases().lock() {
        if let Some(state) = canvases.get(&handle) {
            return state.pixmap.width() * 4;
        }
    }
    0
}

// ============================================================================
// Stroke Settings
// ============================================================================

/// Set stroke width
#[no_mangle]
pub extern "C" fn set_stroke_width(handle: u64, width: f32) {
    if let Ok(mut canvases) = get_canvases().lock() {
        if let Some(state) = canvases.get_mut(&handle) {
            state.stroke_width = width;
        }
    }
}

/// Line cap constants
pub const LINE_CAP_BUTT: i32 = 0;
pub const LINE_CAP_ROUND: i32 = 1;
pub const LINE_CAP_SQUARE: i32 = 2;

/// Set line cap style
#[no_mangle]
pub extern "C" fn set_line_cap(handle: u64, cap: i32) {
    if let Ok(mut canvases) = get_canvases().lock() {
        if let Some(state) = canvases.get_mut(&handle) {
            state.line_cap = match cap {
                LINE_CAP_ROUND => LineCap::Round,
                LINE_CAP_SQUARE => LineCap::Square,
                _ => LineCap::Butt,
            };
        }
    }
}

/// Line join constants
pub const LINE_JOIN_MITER: i32 = 0;
pub const LINE_JOIN_ROUND: i32 = 1;
pub const LINE_JOIN_BEVEL: i32 = 2;

/// Set line join style
#[no_mangle]
pub extern "C" fn set_line_join(handle: u64, join: i32) {
    if let Ok(mut canvases) = get_canvases().lock() {
        if let Some(state) = canvases.get_mut(&handle) {
            state.line_join = match join {
                LINE_JOIN_ROUND => LineJoin::Round,
                LINE_JOIN_BEVEL => LineJoin::Bevel,
                _ => LineJoin::Miter,
            };
        }
    }
}

// ============================================================================
// Transform
// ============================================================================

/// Reset transform to identity
#[no_mangle]
pub extern "C" fn transform_reset(handle: u64) {
    if let Ok(mut canvases) = get_canvases().lock() {
        if let Some(state) = canvases.get_mut(&handle) {
            state.transform = Transform::identity();
        }
    }
}

/// Translate
#[no_mangle]
pub extern "C" fn transform_translate(handle: u64, x: f32, y: f32) {
    if let Ok(mut canvases) = get_canvases().lock() {
        if let Some(state) = canvases.get_mut(&handle) {
            state.transform = state.transform.post_translate(x, y);
        }
    }
}

/// Rotate (angle in radians)
#[no_mangle]
pub extern "C" fn transform_rotate(handle: u64, angle: f32) {
    if let Ok(mut canvases) = get_canvases().lock() {
        if let Some(state) = canvases.get_mut(&handle) {
            state.transform = state.transform.post_rotate(angle.to_degrees());
        }
    }
}

/// Scale
#[no_mangle]
pub extern "C" fn transform_scale(handle: u64, sx: f32, sy: f32) {
    if let Ok(mut canvases) = get_canvases().lock() {
        if let Some(state) = canvases.get_mut(&handle) {
            state.transform = state.transform.post_scale(sx, sy);
        }
    }
}

// ============================================================================
// Drawing Primitives
// ============================================================================

fn make_paint(color: PaintColor) -> Paint<'static> {
    let mut paint = Paint::default();
    paint.set_color(color.to_color());
    paint.anti_alias = true;
    paint
}

fn make_stroke(state: &CanvasState) -> Stroke {
    let mut stroke = Stroke::default();
    stroke.width = state.stroke_width;
    stroke.line_cap = state.line_cap;
    stroke.line_join = state.line_join;
    stroke
}

/// Fill rectangle
#[no_mangle]
pub extern "C" fn fill_rect(handle: u64, x: f32, y: f32, width: f32, height: f32, color: PaintColor) {
    if let Ok(mut canvases) = get_canvases().lock() {
        if let Some(state) = canvases.get_mut(&handle) {
            if let Some(rect) = tiny_skia::Rect::from_xywh(x, y, width, height) {
                let path = PathBuilder::from_rect(rect);
                let paint = make_paint(color);
                state.pixmap.fill_path(&path, &paint, FillRule::Winding, state.transform, None);
            }
        }
    }
}

/// Stroke rectangle
#[no_mangle]
pub extern "C" fn stroke_rect(handle: u64, x: f32, y: f32, width: f32, height: f32, color: PaintColor) {
    if let Ok(mut canvases) = get_canvases().lock() {
        if let Some(state) = canvases.get_mut(&handle) {
            if let Some(rect) = tiny_skia::Rect::from_xywh(x, y, width, height) {
                let path = PathBuilder::from_rect(rect);
                let paint = make_paint(color);
                let stroke = make_stroke(state);
                state.pixmap.stroke_path(&path, &paint, &stroke, state.transform, None);
            }
        }
    }
}

/// Fill circle
#[no_mangle]
pub extern "C" fn fill_circle(handle: u64, cx: f32, cy: f32, radius: f32, color: PaintColor) {
    if let Ok(mut canvases) = get_canvases().lock() {
        if let Some(state) = canvases.get_mut(&handle) {
            if let Some(path) = PathBuilder::from_circle(cx, cy, radius) {
                let paint = make_paint(color);
                state.pixmap.fill_path(&path, &paint, FillRule::Winding, state.transform, None);
            }
        }
    }
}

/// Stroke circle
#[no_mangle]
pub extern "C" fn stroke_circle(handle: u64, cx: f32, cy: f32, radius: f32, color: PaintColor) {
    if let Ok(mut canvases) = get_canvases().lock() {
        if let Some(state) = canvases.get_mut(&handle) {
            if let Some(path) = PathBuilder::from_circle(cx, cy, radius) {
                let paint = make_paint(color);
                let stroke = make_stroke(state);
                state.pixmap.stroke_path(&path, &paint, &stroke, state.transform, None);
            }
        }
    }
}

/// Fill ellipse
#[no_mangle]
pub extern "C" fn fill_ellipse(handle: u64, cx: f32, cy: f32, rx: f32, ry: f32, color: PaintColor) {
    if let Ok(mut canvases) = get_canvases().lock() {
        if let Some(state) = canvases.get_mut(&handle) {
            if let Some(rect) = tiny_skia::Rect::from_xywh(cx - rx, cy - ry, rx * 2.0, ry * 2.0) {
                if let Some(path) = PathBuilder::from_oval(rect) {
                    let paint = make_paint(color);
                    state.pixmap.fill_path(&path, &paint, FillRule::Winding, state.transform, None);
                }
            }
        }
    }
}

/// Stroke ellipse
#[no_mangle]
pub extern "C" fn stroke_ellipse(handle: u64, cx: f32, cy: f32, rx: f32, ry: f32, color: PaintColor) {
    if let Ok(mut canvases) = get_canvases().lock() {
        if let Some(state) = canvases.get_mut(&handle) {
            if let Some(rect) = tiny_skia::Rect::from_xywh(cx - rx, cy - ry, rx * 2.0, ry * 2.0) {
                if let Some(path) = PathBuilder::from_oval(rect) {
                    let paint = make_paint(color);
                    let stroke = make_stroke(state);
                    state.pixmap.stroke_path(&path, &paint, &stroke, state.transform, None);
                }
            }
        }
    }
}

/// Fill rounded rectangle
#[no_mangle]
pub extern "C" fn fill_rounded_rect(handle: u64, x: f32, y: f32, width: f32, height: f32, radius: f32, color: PaintColor) {
    if let Ok(mut canvases) = get_canvases().lock() {
        if let Some(state) = canvases.get_mut(&handle) {
            if tiny_skia::Rect::from_xywh(x, y, width, height).is_some() {
                let mut pb = PathBuilder::new();
                let r = radius.min(width / 2.0).min(height / 2.0);

                pb.move_to(x + r, y);
                pb.line_to(x + width - r, y);
                pb.quad_to(x + width, y, x + width, y + r);
                pb.line_to(x + width, y + height - r);
                pb.quad_to(x + width, y + height, x + width - r, y + height);
                pb.line_to(x + r, y + height);
                pb.quad_to(x, y + height, x, y + height - r);
                pb.line_to(x, y + r);
                pb.quad_to(x, y, x + r, y);
                pb.close();

                if let Some(path) = pb.finish() {
                    let paint = make_paint(color);
                    state.pixmap.fill_path(&path, &paint, FillRule::Winding, state.transform, None);
                }
            }
        }
    }
}

/// Stroke rounded rectangle
#[no_mangle]
pub extern "C" fn stroke_rounded_rect(handle: u64, x: f32, y: f32, width: f32, height: f32, radius: f32, color: PaintColor) {
    if let Ok(mut canvases) = get_canvases().lock() {
        if let Some(state) = canvases.get_mut(&handle) {
            let r = radius.min(width / 2.0).min(height / 2.0);
            let mut pb = PathBuilder::new();

            pb.move_to(x + r, y);
            pb.line_to(x + width - r, y);
            pb.quad_to(x + width, y, x + width, y + r);
            pb.line_to(x + width, y + height - r);
            pb.quad_to(x + width, y + height, x + width - r, y + height);
            pb.line_to(x + r, y + height);
            pb.quad_to(x, y + height, x, y + height - r);
            pb.line_to(x, y + r);
            pb.quad_to(x, y, x + r, y);
            pb.close();

            if let Some(path) = pb.finish() {
                let paint = make_paint(color);
                let stroke = make_stroke(state);
                state.pixmap.stroke_path(&path, &paint, &stroke, state.transform, None);
            }
        }
    }
}

/// Draw line
#[no_mangle]
pub extern "C" fn draw_line(handle: u64, x1: f32, y1: f32, x2: f32, y2: f32, color: PaintColor) {
    if let Ok(mut canvases) = get_canvases().lock() {
        if let Some(state) = canvases.get_mut(&handle) {
            let mut pb = PathBuilder::new();
            pb.move_to(x1, y1);
            pb.line_to(x2, y2);

            if let Some(path) = pb.finish() {
                let paint = make_paint(color);
                let stroke = make_stroke(state);
                state.pixmap.stroke_path(&path, &paint, &stroke, state.transform, None);
            }
        }
    }
}

// ============================================================================
// Path Operations
// ============================================================================

/// Create new path
#[no_mangle]
pub extern "C" fn path_create() -> u64 {
    let handle = next_handle();
    if let Ok(mut paths) = get_paths().lock() {
        paths.insert(handle, PathBuilder::new());
    }
    handle
}

/// Free path
#[no_mangle]
pub extern "C" fn path_free(handle: u64) {
    if let Ok(mut paths) = get_paths().lock() {
        paths.remove(&handle);
    }
}

/// Move to point
#[no_mangle]
pub extern "C" fn path_move_to(handle: u64, x: f32, y: f32) {
    if let Ok(mut paths) = get_paths().lock() {
        if let Some(pb) = paths.get_mut(&handle) {
            pb.move_to(x, y);
        }
    }
}

/// Line to point
#[no_mangle]
pub extern "C" fn path_line_to(handle: u64, x: f32, y: f32) {
    if let Ok(mut paths) = get_paths().lock() {
        if let Some(pb) = paths.get_mut(&handle) {
            pb.line_to(x, y);
        }
    }
}

/// Quadratic bezier curve
#[no_mangle]
pub extern "C" fn path_quad_to(handle: u64, cx: f32, cy: f32, x: f32, y: f32) {
    if let Ok(mut paths) = get_paths().lock() {
        if let Some(pb) = paths.get_mut(&handle) {
            pb.quad_to(cx, cy, x, y);
        }
    }
}

/// Cubic bezier curve
#[no_mangle]
pub extern "C" fn path_cubic_to(handle: u64, c1x: f32, c1y: f32, c2x: f32, c2y: f32, x: f32, y: f32) {
    if let Ok(mut paths) = get_paths().lock() {
        if let Some(pb) = paths.get_mut(&handle) {
            pb.cubic_to(c1x, c1y, c2x, c2y, x, y);
        }
    }
}

/// Close path
#[no_mangle]
pub extern "C" fn path_close(handle: u64) {
    if let Ok(mut paths) = get_paths().lock() {
        if let Some(pb) = paths.get_mut(&handle) {
            pb.close();
        }
    }
}

/// Fill path on canvas
#[no_mangle]
pub extern "C" fn path_fill(canvas_handle: u64, path_handle: u64, color: PaintColor) {
    let path = {
        if let Ok(mut paths) = get_paths().lock() {
            if let Some(pb) = paths.remove(&path_handle) {
                pb.finish()
            } else {
                None
            }
        } else {
            None
        }
    };

    if let Some(path) = path {
        if let Ok(mut canvases) = get_canvases().lock() {
            if let Some(state) = canvases.get_mut(&canvas_handle) {
                let paint = make_paint(color);
                state.pixmap.fill_path(&path, &paint, FillRule::Winding, state.transform, None);
            }
        }
    }
}

/// Stroke path on canvas
#[no_mangle]
pub extern "C" fn path_stroke(canvas_handle: u64, path_handle: u64, color: PaintColor) {
    let path = {
        if let Ok(mut paths) = get_paths().lock() {
            if let Some(pb) = paths.remove(&path_handle) {
                pb.finish()
            } else {
                None
            }
        } else {
            None
        }
    };

    if let Some(path) = path {
        if let Ok(mut canvases) = get_canvases().lock() {
            if let Some(state) = canvases.get_mut(&canvas_handle) {
                let paint = make_paint(color);
                let stroke = make_stroke(state);
                state.pixmap.stroke_path(&path, &paint, &stroke, state.transform, None);
            }
        }
    }
}

// ============================================================================
// Plugin Export
// ============================================================================

zrtl_plugin! {
    name: "zrtl_paint",
    symbols: [
        // Colors
        ("$Paint$rgba", paint_rgba),
        ("$Paint$rgb", paint_rgb),
        ("$Paint$hex", paint_hex),

        // Canvas
        ("$Paint$canvas_create", canvas_create),
        ("$Paint$canvas_free", canvas_free),
        ("$Paint$canvas_width", canvas_width),
        ("$Paint$canvas_height", canvas_height),
        ("$Paint$canvas_clear", canvas_clear),
        ("$Paint$canvas_get_pixels", canvas_get_pixels),
        ("$Paint$canvas_save_png", canvas_save_png),
        ("$Paint$canvas_encode_png", canvas_encode_png),

        // Buffer Access (for SDL/Window rendering)
        ("$Paint$canvas_get_buffer", canvas_get_buffer),
        ("$Paint$canvas_data_ptr", canvas_data_ptr),
        ("$Paint$canvas_data_ptr_mut", canvas_data_ptr_mut),
        ("$Paint$canvas_data_len", canvas_data_len),
        ("$Paint$canvas_pitch", canvas_pitch),

        // Stroke settings
        ("$Paint$set_stroke_width", set_stroke_width),
        ("$Paint$set_line_cap", set_line_cap),
        ("$Paint$set_line_join", set_line_join),

        // Transform
        ("$Paint$transform_reset", transform_reset),
        ("$Paint$transform_translate", transform_translate),
        ("$Paint$transform_rotate", transform_rotate),
        ("$Paint$transform_scale", transform_scale),

        // Primitives
        ("$Paint$fill_rect", fill_rect),
        ("$Paint$stroke_rect", stroke_rect),
        ("$Paint$fill_circle", fill_circle),
        ("$Paint$stroke_circle", stroke_circle),
        ("$Paint$fill_ellipse", fill_ellipse),
        ("$Paint$stroke_ellipse", stroke_ellipse),
        ("$Paint$fill_rounded_rect", fill_rounded_rect),
        ("$Paint$stroke_rounded_rect", stroke_rounded_rect),
        ("$Paint$draw_line", draw_line),

        // Paths
        ("$Paint$path_create", path_create),
        ("$Paint$path_free", path_free),
        ("$Paint$path_move_to", path_move_to),
        ("$Paint$path_line_to", path_line_to),
        ("$Paint$path_quad_to", path_quad_to),
        ("$Paint$path_cubic_to", path_cubic_to),
        ("$Paint$path_close", path_close),
        ("$Paint$path_fill", path_fill),
        ("$Paint$path_stroke", path_stroke),
    ]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_create_canvas() {
        let handle = canvas_create(100, 100);
        assert!(handle > 0);
        assert_eq!(canvas_width(handle), 100);
        assert_eq!(canvas_height(handle), 100);
        canvas_free(handle);
    }

    #[test]
    fn test_color_helpers() {
        let color = paint_rgba(255, 128, 64, 200);
        assert_eq!(color.r, 255);
        assert_eq!(color.g, 128);
        assert_eq!(color.b, 64);
        assert_eq!(color.a, 200);

        let rgb = paint_rgb(100, 150, 200);
        assert_eq!(rgb.a, 255);

        let hex = paint_hex(0xFF8040);
        assert_eq!(hex.r, 255);
        assert_eq!(hex.g, 128);
        assert_eq!(hex.b, 64);
    }

    #[test]
    fn test_draw_shapes() {
        let canvas = canvas_create(200, 200);
        let white = paint_rgb(255, 255, 255);
        let red = paint_rgb(255, 0, 0);

        canvas_clear(canvas, white);
        fill_rect(canvas, 10.0, 10.0, 50.0, 50.0, red);
        fill_circle(canvas, 100.0, 100.0, 30.0, red);

        // Should not crash
        canvas_free(canvas);
    }

    #[test]
    fn test_path() {
        let canvas = canvas_create(100, 100);
        let path = path_create();

        path_move_to(path, 10.0, 10.0);
        path_line_to(path, 90.0, 10.0);
        path_line_to(path, 50.0, 90.0);
        path_close(path);

        let blue = paint_rgb(0, 0, 255);
        path_fill(canvas, path, blue);

        canvas_free(canvas);
    }

    #[test]
    fn test_buffer_access() {
        let canvas = canvas_create(100, 100);
        let red = paint_rgb(255, 0, 0);
        canvas_clear(canvas, red);

        // Test buffer info struct
        let buffer = canvas_get_buffer(canvas);
        assert!(!buffer.data.is_null());
        assert_eq!(buffer.width, 100);
        assert_eq!(buffer.height, 100);
        assert_eq!(buffer.pitch, 400); // 100 * 4 bytes
        assert_eq!(buffer.len, 40000); // 100 * 100 * 4 bytes

        // Test individual functions
        let ptr = canvas_data_ptr(canvas);
        assert!(!ptr.is_null());
        assert_eq!(canvas_data_len(canvas), 40000);
        assert_eq!(canvas_pitch(canvas), 400);

        // Verify pixel data (first pixel should be red with premultiplied alpha)
        unsafe {
            let r = *ptr;
            let g = *ptr.add(1);
            let b = *ptr.add(2);
            let a = *ptr.add(3);
            assert_eq!(r, 255); // Red
            assert_eq!(g, 0);
            assert_eq!(b, 0);
            assert_eq!(a, 255); // Full alpha
        }

        canvas_free(canvas);
    }
}
