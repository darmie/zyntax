//! ZRTL SDL Plugin - SDL2 windowing, events, and rendering
//!
//! This plugin provides SDL2 bindings for window management and 2D rendering.
//!
//! ## Architecture
//!
//! SDL2 types are NOT thread-safe (they use Rc and raw pointers). This plugin
//! uses a single-threaded design with thread_local storage to ensure safety.
//! All SDL operations MUST be called from the main thread.
//!
//! ## Initialization
//! - `$Sdl$init` - Initialize SDL (returns context handle)
//! - `$Sdl$quit` - Quit SDL
//!
//! ## Window Management
//! - `$Sdl$window_create` - Create window (returns window handle)
//! - `$Sdl$window_destroy` - Destroy window
//! - `$Sdl$window_set_title` - Set window title
//! - `$Sdl$window_get_size` - Get window size
//! - `$Sdl$window_show` / `$Sdl$window_hide` - Show/hide window
//!
//! ## Rendering
//! - `$Sdl$clear` - Clear with draw color
//! - `$Sdl$present` - Present rendered content
//! - `$Sdl$set_draw_color` - Set draw color
//! - `$Sdl$draw_point` / `$Sdl$draw_line` / `$Sdl$draw_rect` / `$Sdl$fill_rect`
//!
//! ## Events
//! - `$Sdl$poll_event` - Poll for events
//! - `$Sdl$wait_event` - Wait for events

use std::cell::RefCell;
use std::collections::HashMap;
use std::ffi::c_char;

use sdl2::event::Event;
use sdl2::pixels::Color;
use sdl2::rect::Rect;
use sdl2::render::Canvas;
use sdl2::video::Window;
use sdl2::EventPump;
use sdl2::Sdl;

use zrtl::{zrtl_plugin, StringPtr, string_as_str};

// ============================================================================
// Thread-Local State (SDL2 is NOT thread-safe)
// ============================================================================

/// SDL state stored in thread-local storage for safety.
/// SDL2 types use Rc and raw pointers, so they cannot be shared across threads.
struct SdlContext {
    #[allow(dead_code)]
    context: Sdl,
    event_pump: EventPump,
    next_window_id: u64,
    windows: HashMap<u64, Canvas<Window>>,
}

thread_local! {
    /// Thread-local SDL context. SDL operations must happen on the main thread.
    static SDL_CONTEXT: RefCell<Option<SdlContext>> = const { RefCell::new(None) };
}

// ============================================================================
// Event Types
// ============================================================================

pub const EVENT_NONE: u32 = 0;
pub const EVENT_QUIT: u32 = 1;
pub const EVENT_KEY_DOWN: u32 = 2;
pub const EVENT_KEY_UP: u32 = 3;
pub const EVENT_MOUSE_MOTION: u32 = 4;
pub const EVENT_MOUSE_BUTTON_DOWN: u32 = 5;
pub const EVENT_MOUSE_BUTTON_UP: u32 = 6;
pub const EVENT_MOUSE_WHEEL: u32 = 7;
pub const EVENT_WINDOW_RESIZED: u32 = 8;
pub const EVENT_WINDOW_CLOSE: u32 = 9;

/// Event data returned from polling
#[repr(C)]
#[derive(Clone, Copy, Default)]
pub struct SdlEvent {
    pub event_type: u32,
    pub window_id: u32,
    pub key_scancode: i32,
    pub key_repeat: i32,
    pub mouse_x: i32,
    pub mouse_y: i32,
    pub mouse_button: u8,
    pub mouse_clicks: u8,
    pub wheel_x: i32,
    pub wheel_y: i32,
    pub window_width: i32,
    pub window_height: i32,
}

/// Window size result
#[repr(C)]
#[derive(Clone, Copy, Default)]
pub struct SdlWindowSize {
    pub width: u32,
    pub height: u32,
}

// ============================================================================
// Initialization
// ============================================================================

/// Initialize SDL
/// Returns 1 on success, 0 on error
/// MUST be called from the main thread!
#[no_mangle]
pub extern "C" fn sdl_init() -> i32 {
    SDL_CONTEXT.with(|ctx| {
        let mut ctx = ctx.borrow_mut();
        if ctx.is_some() {
            return 1; // Already initialized
        }

        match sdl2::init() {
            Ok(context) => {
                match context.event_pump() {
                    Ok(event_pump) => {
                        *ctx = Some(SdlContext {
                            context,
                            event_pump,
                            next_window_id: 1,
                            windows: HashMap::new(),
                        });
                        1
                    }
                    Err(_) => 0,
                }
            }
            Err(_) => 0,
        }
    })
}

/// Quit SDL and clean up
#[no_mangle]
pub extern "C" fn sdl_quit() {
    SDL_CONTEXT.with(|ctx| {
        *ctx.borrow_mut() = None;
    });
}

/// Check if SDL is initialized
#[no_mangle]
pub extern "C" fn sdl_is_init() -> i32 {
    SDL_CONTEXT.with(|ctx| {
        if ctx.borrow().is_some() { 1 } else { 0 }
    })
}

// ============================================================================
// Window Management
// ============================================================================

/// Create a new window
/// Returns window handle, or 0 on error
#[no_mangle]
pub extern "C" fn sdl_window_create(
    title: StringPtr,
    x: i32,
    y: i32,
    width: u32,
    height: u32,
) -> u64 {
    let title_str = match unsafe { string_as_str(title) } {
        Some(s) => s,
        None => "Window",
    };

    SDL_CONTEXT.with(|ctx| {
        let mut ctx_ref = ctx.borrow_mut();
        let state = match ctx_ref.as_mut() {
            Some(s) => s,
            None => return 0,
        };

        let video = match state.context.video() {
            Ok(v) => v,
            Err(_) => return 0,
        };

        let window = if x < 0 || y < 0 {
            // Centered
            video.window(title_str, width, height)
                .position_centered()
                .build()
        } else {
            video.window(title_str, width, height)
                .position(x, y)
                .build()
        };

        let window = match window {
            Ok(w) => w,
            Err(_) => return 0,
        };

        let canvas = match window.into_canvas().software().build() {
            Ok(c) => c,
            Err(_) => return 0,
        };

        let id = state.next_window_id;
        state.next_window_id += 1;
        state.windows.insert(id, canvas);
        id
    })
}

/// Create a centered window
#[no_mangle]
pub extern "C" fn sdl_window_create_centered(
    title: StringPtr,
    width: u32,
    height: u32,
) -> u64 {
    sdl_window_create(title, -1, -1, width, height)
}

/// Destroy a window
#[no_mangle]
pub extern "C" fn sdl_window_destroy(handle: u64) {
    SDL_CONTEXT.with(|ctx| {
        if let Some(state) = ctx.borrow_mut().as_mut() {
            state.windows.remove(&handle);
        }
    });
}

/// Set window title
#[no_mangle]
pub extern "C" fn sdl_window_set_title(handle: u64, title: StringPtr) -> i32 {
    let title_str = match unsafe { string_as_str(title) } {
        Some(s) => s,
        None => return 0,
    };

    SDL_CONTEXT.with(|ctx| {
        if let Some(state) = ctx.borrow_mut().as_mut() {
            if let Some(canvas) = state.windows.get_mut(&handle) {
                canvas.window_mut().set_title(title_str).ok();
                return 1;
            }
        }
        0
    })
}

/// Get window size
/// Returns SdlWindowSize with width/height, or {0,0} on error
#[no_mangle]
pub extern "C" fn sdl_window_get_size(handle: u64) -> SdlWindowSize {
    SDL_CONTEXT.with(|ctx| {
        if let Some(state) = ctx.borrow().as_ref() {
            if let Some(canvas) = state.windows.get(&handle) {
                let (w, h) = canvas.window().size();
                return SdlWindowSize { width: w, height: h };
            }
        }
        SdlWindowSize::default()
    })
}

/// Show window
#[no_mangle]
pub extern "C" fn sdl_window_show(handle: u64) {
    SDL_CONTEXT.with(|ctx| {
        if let Some(state) = ctx.borrow_mut().as_mut() {
            if let Some(canvas) = state.windows.get_mut(&handle) {
                canvas.window_mut().show();
            }
        }
    });
}

/// Hide window
#[no_mangle]
pub extern "C" fn sdl_window_hide(handle: u64) {
    SDL_CONTEXT.with(|ctx| {
        if let Some(state) = ctx.borrow_mut().as_mut() {
            if let Some(canvas) = state.windows.get_mut(&handle) {
                canvas.window_mut().hide();
            }
        }
    });
}

// ============================================================================
// Rendering
// ============================================================================

/// Set draw color (RGBA)
#[no_mangle]
pub extern "C" fn sdl_set_draw_color(handle: u64, r: u8, g: u8, b: u8, a: u8) {
    SDL_CONTEXT.with(|ctx| {
        if let Some(state) = ctx.borrow_mut().as_mut() {
            if let Some(canvas) = state.windows.get_mut(&handle) {
                canvas.set_draw_color(Color::RGBA(r, g, b, a));
            }
        }
    });
}

/// Clear the canvas with current draw color
#[no_mangle]
pub extern "C" fn sdl_clear(handle: u64) {
    SDL_CONTEXT.with(|ctx| {
        if let Some(state) = ctx.borrow_mut().as_mut() {
            if let Some(canvas) = state.windows.get_mut(&handle) {
                canvas.clear();
            }
        }
    });
}

/// Present the canvas (show rendered content)
#[no_mangle]
pub extern "C" fn sdl_present(handle: u64) {
    SDL_CONTEXT.with(|ctx| {
        if let Some(state) = ctx.borrow_mut().as_mut() {
            if let Some(canvas) = state.windows.get_mut(&handle) {
                canvas.present();
            }
        }
    });
}

/// Draw a point
#[no_mangle]
pub extern "C" fn sdl_draw_point(handle: u64, x: i32, y: i32) -> i32 {
    SDL_CONTEXT.with(|ctx| {
        if let Some(state) = ctx.borrow_mut().as_mut() {
            if let Some(canvas) = state.windows.get_mut(&handle) {
                return canvas.draw_point((x, y)).map(|_| 1).unwrap_or(0);
            }
        }
        0
    })
}

/// Draw a line
#[no_mangle]
pub extern "C" fn sdl_draw_line(handle: u64, x1: i32, y1: i32, x2: i32, y2: i32) -> i32 {
    SDL_CONTEXT.with(|ctx| {
        if let Some(state) = ctx.borrow_mut().as_mut() {
            if let Some(canvas) = state.windows.get_mut(&handle) {
                return canvas.draw_line((x1, y1), (x2, y2)).map(|_| 1).unwrap_or(0);
            }
        }
        0
    })
}

/// Draw a rectangle outline
#[no_mangle]
pub extern "C" fn sdl_draw_rect(handle: u64, x: i32, y: i32, w: u32, h: u32) -> i32 {
    SDL_CONTEXT.with(|ctx| {
        if let Some(state) = ctx.borrow_mut().as_mut() {
            if let Some(canvas) = state.windows.get_mut(&handle) {
                return canvas.draw_rect(Rect::new(x, y, w, h)).map(|_| 1).unwrap_or(0);
            }
        }
        0
    })
}

/// Fill a rectangle
#[no_mangle]
pub extern "C" fn sdl_fill_rect(handle: u64, x: i32, y: i32, w: u32, h: u32) -> i32 {
    SDL_CONTEXT.with(|ctx| {
        if let Some(state) = ctx.borrow_mut().as_mut() {
            if let Some(canvas) = state.windows.get_mut(&handle) {
                return canvas.fill_rect(Rect::new(x, y, w, h)).map(|_| 1).unwrap_or(0);
            }
        }
        0
    })
}

/// Draw multiple rectangles efficiently
#[no_mangle]
pub extern "C" fn sdl_draw_rects(handle: u64, rects: *const Rect, count: usize) -> i32 {
    if rects.is_null() || count == 0 {
        return 0;
    }

    SDL_CONTEXT.with(|ctx| {
        if let Some(state) = ctx.borrow_mut().as_mut() {
            if let Some(canvas) = state.windows.get_mut(&handle) {
                let slice = unsafe { std::slice::from_raw_parts(rects, count) };
                return canvas.draw_rects(slice).map(|_| 1).unwrap_or(0);
            }
        }
        0
    })
}

/// Fill multiple rectangles efficiently
#[no_mangle]
pub extern "C" fn sdl_fill_rects(handle: u64, rects: *const Rect, count: usize) -> i32 {
    if rects.is_null() || count == 0 {
        return 0;
    }

    SDL_CONTEXT.with(|ctx| {
        if let Some(state) = ctx.borrow_mut().as_mut() {
            if let Some(canvas) = state.windows.get_mut(&handle) {
                let slice = unsafe { std::slice::from_raw_parts(rects, count) };
                return canvas.fill_rects(slice).map(|_| 1).unwrap_or(0);
            }
        }
        0
    })
}

// ============================================================================
// Buffer Blitting (for zrtl_paint integration)
// ============================================================================

/// Blit RGBA pixel data to window at position
///
/// Parameters:
/// - handle: Window handle
/// - data: Pointer to RGBA pixel data
/// - width: Source image width
/// - height: Source image height
/// - pitch: Source row stride in bytes (usually width * 4)
/// - dst_x, dst_y: Destination position in window
///
/// Returns 1 on success, 0 on error
///
/// This function copies pixel data from a buffer (like zrtl_paint canvas)
/// directly to the window surface. The source format should be RGBA.
#[no_mangle]
pub extern "C" fn sdl_blit_rgba(
    handle: u64,
    data: *const u8,
    width: u32,
    height: u32,
    pitch: u32,
    dst_x: i32,
    dst_y: i32,
) -> i32 {
    if data.is_null() || width == 0 || height == 0 {
        return 0;
    }

    SDL_CONTEXT.with(|ctx| {
        if let Some(state) = ctx.borrow_mut().as_mut() {
            if let Some(canvas) = state.windows.get_mut(&handle) {
                // Get the window surface for direct pixel access
                let texture_creator = canvas.texture_creator();

                // Create a streaming texture from the pixel data
                let mut texture = match texture_creator.create_texture_streaming(
                    sdl2::pixels::PixelFormatEnum::RGBA32,
                    width,
                    height,
                ) {
                    Ok(t) => t,
                    Err(_) => return 0,
                };

                // Copy pixel data to texture
                let src_slice = unsafe {
                    std::slice::from_raw_parts(data, (height * pitch) as usize)
                };

                if texture.update(None, src_slice, pitch as usize).is_err() {
                    return 0;
                }

                // Copy texture to canvas
                let dst_rect = Rect::new(dst_x, dst_y, width, height);
                if canvas.copy(&texture, None, Some(dst_rect)).is_err() {
                    return 0;
                }

                return 1;
            }
        }
        0
    })
}

/// Blit RGBA pixel data to window, scaled to fit a destination rectangle
///
/// Parameters:
/// - handle: Window handle
/// - data: Pointer to RGBA pixel data
/// - src_width, src_height: Source image dimensions
/// - pitch: Source row stride in bytes
/// - dst_x, dst_y, dst_w, dst_h: Destination rectangle
///
/// Returns 1 on success, 0 on error
#[no_mangle]
pub extern "C" fn sdl_blit_rgba_scaled(
    handle: u64,
    data: *const u8,
    src_width: u32,
    src_height: u32,
    pitch: u32,
    dst_x: i32,
    dst_y: i32,
    dst_w: u32,
    dst_h: u32,
) -> i32 {
    if data.is_null() || src_width == 0 || src_height == 0 {
        return 0;
    }

    SDL_CONTEXT.with(|ctx| {
        if let Some(state) = ctx.borrow_mut().as_mut() {
            if let Some(canvas) = state.windows.get_mut(&handle) {
                let texture_creator = canvas.texture_creator();

                let mut texture = match texture_creator.create_texture_streaming(
                    sdl2::pixels::PixelFormatEnum::RGBA32,
                    src_width,
                    src_height,
                ) {
                    Ok(t) => t,
                    Err(_) => return 0,
                };

                let src_slice = unsafe {
                    std::slice::from_raw_parts(data, (src_height * pitch) as usize)
                };

                if texture.update(None, src_slice, pitch as usize).is_err() {
                    return 0;
                }

                let dst_rect = Rect::new(dst_x, dst_y, dst_w, dst_h);
                if canvas.copy(&texture, None, Some(dst_rect)).is_err() {
                    return 0;
                }

                return 1;
            }
        }
        0
    })
}

// ============================================================================
// Event Handling
// ============================================================================

fn convert_event(event: Event) -> SdlEvent {
    let mut data = SdlEvent::default();

    match event {
        Event::Quit { .. } => {
            data.event_type = EVENT_QUIT;
        }
        Event::KeyDown { scancode, repeat, window_id, .. } => {
            data.event_type = EVENT_KEY_DOWN;
            data.window_id = window_id;
            data.key_scancode = scancode.map(|s| s as i32).unwrap_or(0);
            data.key_repeat = if repeat { 1 } else { 0 };
        }
        Event::KeyUp { scancode, window_id, .. } => {
            data.event_type = EVENT_KEY_UP;
            data.window_id = window_id;
            data.key_scancode = scancode.map(|s| s as i32).unwrap_or(0);
        }
        Event::MouseMotion { x, y, window_id, .. } => {
            data.event_type = EVENT_MOUSE_MOTION;
            data.window_id = window_id;
            data.mouse_x = x;
            data.mouse_y = y;
        }
        Event::MouseButtonDown { x, y, mouse_btn, clicks, window_id, .. } => {
            data.event_type = EVENT_MOUSE_BUTTON_DOWN;
            data.window_id = window_id;
            data.mouse_x = x;
            data.mouse_y = y;
            data.mouse_button = mouse_btn as u8;
            data.mouse_clicks = clicks;
        }
        Event::MouseButtonUp { x, y, mouse_btn, clicks, window_id, .. } => {
            data.event_type = EVENT_MOUSE_BUTTON_UP;
            data.window_id = window_id;
            data.mouse_x = x;
            data.mouse_y = y;
            data.mouse_button = mouse_btn as u8;
            data.mouse_clicks = clicks;
        }
        Event::MouseWheel { x, y, window_id, .. } => {
            data.event_type = EVENT_MOUSE_WHEEL;
            data.window_id = window_id;
            data.wheel_x = x;
            data.wheel_y = y;
        }
        Event::Window { win_event, window_id, .. } => {
            match win_event {
                sdl2::event::WindowEvent::Resized(w, h) |
                sdl2::event::WindowEvent::SizeChanged(w, h) => {
                    data.event_type = EVENT_WINDOW_RESIZED;
                    data.window_id = window_id;
                    data.window_width = w;
                    data.window_height = h;
                }
                sdl2::event::WindowEvent::Close => {
                    data.event_type = EVENT_WINDOW_CLOSE;
                    data.window_id = window_id;
                }
                _ => {}
            }
        }
        _ => {}
    }

    data
}

/// Poll for events (non-blocking)
/// Returns event data, with event_type = EVENT_NONE if no event
#[no_mangle]
pub extern "C" fn sdl_poll_event() -> SdlEvent {
    SDL_CONTEXT.with(|ctx| {
        if let Some(state) = ctx.borrow_mut().as_mut() {
            match state.event_pump.poll_event() {
                Some(event) => convert_event(event),
                None => SdlEvent::default(),
            }
        } else {
            SdlEvent::default()
        }
    })
}

/// Wait for an event (blocking)
#[no_mangle]
pub extern "C" fn sdl_wait_event() -> SdlEvent {
    SDL_CONTEXT.with(|ctx| {
        if let Some(state) = ctx.borrow_mut().as_mut() {
            let event = state.event_pump.wait_event();
            convert_event(event)
        } else {
            SdlEvent::default()
        }
    })
}

/// Wait for an event with timeout (milliseconds)
#[no_mangle]
pub extern "C" fn sdl_wait_event_timeout(timeout_ms: u32) -> SdlEvent {
    SDL_CONTEXT.with(|ctx| {
        if let Some(state) = ctx.borrow_mut().as_mut() {
            match state.event_pump.wait_event_timeout(timeout_ms) {
                Some(event) => convert_event(event),
                None => SdlEvent::default(),
            }
        } else {
            SdlEvent::default()
        }
    })
}

// ============================================================================
// Timing
// ============================================================================

/// Get ticks since SDL initialization (milliseconds)
#[no_mangle]
pub extern "C" fn sdl_get_ticks() -> u32 {
    unsafe { sdl2::sys::SDL_GetTicks() }
}

/// Delay for specified milliseconds
#[no_mangle]
pub extern "C" fn sdl_delay(ms: u32) {
    std::thread::sleep(std::time::Duration::from_millis(ms as u64));
}

// ============================================================================
// Keyboard Constants (Scancodes)
// ============================================================================

pub const SCANCODE_A: i32 = 4;
pub const SCANCODE_B: i32 = 5;
pub const SCANCODE_C: i32 = 6;
pub const SCANCODE_D: i32 = 7;
pub const SCANCODE_E: i32 = 8;
pub const SCANCODE_F: i32 = 9;
pub const SCANCODE_G: i32 = 10;
pub const SCANCODE_H: i32 = 11;
pub const SCANCODE_I: i32 = 12;
pub const SCANCODE_J: i32 = 13;
pub const SCANCODE_K: i32 = 14;
pub const SCANCODE_L: i32 = 15;
pub const SCANCODE_M: i32 = 16;
pub const SCANCODE_N: i32 = 17;
pub const SCANCODE_O: i32 = 18;
pub const SCANCODE_P: i32 = 19;
pub const SCANCODE_Q: i32 = 20;
pub const SCANCODE_R: i32 = 21;
pub const SCANCODE_S: i32 = 22;
pub const SCANCODE_T: i32 = 23;
pub const SCANCODE_U: i32 = 24;
pub const SCANCODE_V: i32 = 25;
pub const SCANCODE_W: i32 = 26;
pub const SCANCODE_X: i32 = 27;
pub const SCANCODE_Y: i32 = 28;
pub const SCANCODE_Z: i32 = 29;
pub const SCANCODE_1: i32 = 30;
pub const SCANCODE_2: i32 = 31;
pub const SCANCODE_3: i32 = 32;
pub const SCANCODE_4: i32 = 33;
pub const SCANCODE_5: i32 = 34;
pub const SCANCODE_6: i32 = 35;
pub const SCANCODE_7: i32 = 36;
pub const SCANCODE_8: i32 = 37;
pub const SCANCODE_9: i32 = 38;
pub const SCANCODE_0: i32 = 39;
pub const SCANCODE_RETURN: i32 = 40;
pub const SCANCODE_ESCAPE: i32 = 41;
pub const SCANCODE_BACKSPACE: i32 = 42;
pub const SCANCODE_TAB: i32 = 43;
pub const SCANCODE_SPACE: i32 = 44;
pub const SCANCODE_RIGHT: i32 = 79;
pub const SCANCODE_LEFT: i32 = 80;
pub const SCANCODE_DOWN: i32 = 81;
pub const SCANCODE_UP: i32 = 82;

// ============================================================================
// Mouse Button Constants
// ============================================================================

pub const MOUSE_BUTTON_LEFT: u8 = 1;
pub const MOUSE_BUTTON_MIDDLE: u8 = 2;
pub const MOUSE_BUTTON_RIGHT: u8 = 3;

// ============================================================================
// Version
// ============================================================================

/// Get version string
#[no_mangle]
pub extern "C" fn sdl_version() -> *const c_char {
    static VERSION: &[u8] = b"0.1.0\0";
    VERSION.as_ptr() as *const c_char
}

// ============================================================================
// Plugin Export
// ============================================================================

zrtl_plugin! {
    name: "zrtl_sdl",
    symbols: [
        // Initialization
        ("$Sdl$init", sdl_init),
        ("$Sdl$quit", sdl_quit),
        ("$Sdl$is_init", sdl_is_init),

        // Window Management
        ("$Sdl$window_create", sdl_window_create),
        ("$Sdl$window_create_centered", sdl_window_create_centered),
        ("$Sdl$window_destroy", sdl_window_destroy),
        ("$Sdl$window_set_title", sdl_window_set_title),
        ("$Sdl$window_get_size", sdl_window_get_size),
        ("$Sdl$window_show", sdl_window_show),
        ("$Sdl$window_hide", sdl_window_hide),

        // Rendering
        ("$Sdl$set_draw_color", sdl_set_draw_color),
        ("$Sdl$clear", sdl_clear),
        ("$Sdl$present", sdl_present),
        ("$Sdl$draw_point", sdl_draw_point),
        ("$Sdl$draw_line", sdl_draw_line),
        ("$Sdl$draw_rect", sdl_draw_rect),
        ("$Sdl$fill_rect", sdl_fill_rect),
        ("$Sdl$draw_rects", sdl_draw_rects),
        ("$Sdl$fill_rects", sdl_fill_rects),

        // Buffer Blitting (for zrtl_paint integration)
        ("$Sdl$blit_rgba", sdl_blit_rgba),
        ("$Sdl$blit_rgba_scaled", sdl_blit_rgba_scaled),

        // Events
        ("$Sdl$poll_event", sdl_poll_event),
        ("$Sdl$wait_event", sdl_wait_event),
        ("$Sdl$wait_event_timeout", sdl_wait_event_timeout),

        // Timing
        ("$Sdl$get_ticks", sdl_get_ticks),
        ("$Sdl$delay", sdl_delay),

        // Version
        ("$Sdl$version", sdl_version),
    ]
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::CStr;

    #[test]
    fn test_event_struct_default() {
        let event = SdlEvent::default();
        assert_eq!(event.event_type, EVENT_NONE);
        assert_eq!(event.key_scancode, 0);
        assert_eq!(event.mouse_x, 0);
    }

    #[test]
    fn test_version() {
        let version = sdl_version();
        assert!(!version.is_null());
        let v = unsafe { CStr::from_ptr(version) };
        assert_eq!(v.to_str().unwrap(), "0.1.0");
    }

    #[test]
    fn test_delay() {
        use std::time::Instant;
        let start = Instant::now();
        sdl_delay(10);
        let elapsed = start.elapsed();
        assert!(elapsed.as_millis() >= 10);
    }

    // Note: Full SDL tests require a display and would need to be run manually
    // These tests verify the basic API without initializing SDL
    #[test]
    fn test_not_initialized() {
        assert_eq!(sdl_is_init(), 0);
    }

    #[test]
    fn test_window_size_default() {
        let size = SdlWindowSize::default();
        assert_eq!(size.width, 0);
        assert_eq!(size.height, 0);
    }
}
