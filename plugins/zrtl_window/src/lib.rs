//! ZRTL Window Plugin - Cross-Platform Windowing
//!
//! This plugin provides a simplified, high-level windowing API built on SDL2.
//! It abstracts away SDL initialization and provides a cleaner interface for
//! common windowing operations.
//!
//! ## Quick Start
//!
//! ```text
//! // Create a window
//! let win = $Window$create("My App", 800, 600);
//!
//! // Main loop
//! while ($Window$is_open(win)) {
//!     // Poll events
//!     let event = $Window$poll_event();
//!     if (event.type == EVENT_KEY_DOWN && event.key == KEY_ESCAPE) {
//!         $Window$close(win);
//!     }
//!
//!     // Clear and draw
//!     $Window$clear(win, 0x000000FF);  // Black with full alpha
//!     $Window$fill_rect(win, 100, 100, 50, 50, 0xFF0000FF);  // Red rectangle
//!     $Window$present(win);
//!
//!     // Frame timing
//!     $Window$delay(16);  // ~60 FPS
//! }
//!
//! // Cleanup
//! $Window$destroy(win);
//! ```
//!
//! ## Window Management
//! - `$Window$create` - Create a window (initializes SDL automatically)
//! - `$Window$destroy` - Destroy a window
//! - `$Window$is_open` - Check if window is still open
//! - `$Window$close` - Mark window for closing
//! - `$Window$set_title` / `$Window$get_size` - Window properties
//!
//! ## Drawing (Color as RGBA u32: 0xRRGGBBAA)
//! - `$Window$clear` - Clear with color
//! - `$Window$set_color` - Set draw color for subsequent operations
//! - `$Window$draw_pixel` / `$Window$draw_line` - Basic primitives
//! - `$Window$draw_rect` / `$Window$fill_rect` - Rectangles
//! - `$Window$present` - Present frame
//!
//! ## Events
//! - `$Window$poll_event` - Get next event (non-blocking)
//! - `$Window$wait_event` - Wait for event (blocking)

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

use zrtl::{zrtl_plugin, StringPtr, string_as_str, string_new};

// ============================================================================
// Thread-Local State
// ============================================================================

struct WindowState {
    #[allow(dead_code)]
    context: Sdl,
    event_pump: EventPump,
    next_id: u64,
    windows: HashMap<u64, WindowData>,
}

struct WindowData {
    canvas: Canvas<Window>,
    is_open: bool,
}

thread_local! {
    static STATE: RefCell<Option<WindowState>> = const { RefCell::new(None) };
}

/// Initialize the windowing system if not already initialized
fn ensure_init() -> bool {
    STATE.with(|state| {
        let mut state_ref = state.borrow_mut();
        if state_ref.is_some() {
            return true;
        }

        match sdl2::init() {
            Ok(context) => {
                match context.event_pump() {
                    Ok(event_pump) => {
                        *state_ref = Some(WindowState {
                            context,
                            event_pump,
                            next_id: 1,
                            windows: HashMap::new(),
                        });
                        true
                    }
                    Err(_) => false,
                }
            }
            Err(_) => false,
        }
    })
}

// ============================================================================
// Event Types
// ============================================================================

pub const EVENT_NONE: i32 = 0;
pub const EVENT_QUIT: i32 = 1;
pub const EVENT_KEY_DOWN: i32 = 2;
pub const EVENT_KEY_UP: i32 = 3;
pub const EVENT_MOUSE_MOVE: i32 = 4;
pub const EVENT_MOUSE_DOWN: i32 = 5;
pub const EVENT_MOUSE_UP: i32 = 6;
pub const EVENT_MOUSE_WHEEL: i32 = 7;
pub const EVENT_RESIZE: i32 = 8;
pub const EVENT_CLOSE: i32 = 9;

/// Unified event struct for all event types
#[repr(C)]
#[derive(Clone, Copy, Default)]
pub struct WindowEvent {
    /// Event type constant
    pub event_type: i32,
    /// Window handle this event belongs to
    pub window_id: u64,
    /// Key scancode (for key events)
    pub key: i32,
    /// Key repeat flag
    pub key_repeat: i32,
    /// Mouse X position
    pub mouse_x: i32,
    /// Mouse Y position
    pub mouse_y: i32,
    /// Mouse button (1=left, 2=middle, 3=right)
    pub mouse_button: i32,
    /// Mouse wheel delta X
    pub wheel_x: i32,
    /// Mouse wheel delta Y
    pub wheel_y: i32,
    /// New window width (for resize)
    pub width: i32,
    /// New window height (for resize)
    pub height: i32,
}

/// Window size result
#[repr(C)]
#[derive(Clone, Copy, Default)]
pub struct WindowSize {
    pub width: i32,
    pub height: i32,
}

// ============================================================================
// Key Constants
// ============================================================================

pub const KEY_A: i32 = 4;
pub const KEY_B: i32 = 5;
pub const KEY_C: i32 = 6;
pub const KEY_D: i32 = 7;
pub const KEY_E: i32 = 8;
pub const KEY_F: i32 = 9;
pub const KEY_G: i32 = 10;
pub const KEY_H: i32 = 11;
pub const KEY_I: i32 = 12;
pub const KEY_J: i32 = 13;
pub const KEY_K: i32 = 14;
pub const KEY_L: i32 = 15;
pub const KEY_M: i32 = 16;
pub const KEY_N: i32 = 17;
pub const KEY_O: i32 = 18;
pub const KEY_P: i32 = 19;
pub const KEY_Q: i32 = 20;
pub const KEY_R: i32 = 21;
pub const KEY_S: i32 = 22;
pub const KEY_T: i32 = 23;
pub const KEY_U: i32 = 24;
pub const KEY_V: i32 = 25;
pub const KEY_W: i32 = 26;
pub const KEY_X: i32 = 27;
pub const KEY_Y: i32 = 28;
pub const KEY_Z: i32 = 29;
pub const KEY_1: i32 = 30;
pub const KEY_2: i32 = 31;
pub const KEY_3: i32 = 32;
pub const KEY_4: i32 = 33;
pub const KEY_5: i32 = 34;
pub const KEY_6: i32 = 35;
pub const KEY_7: i32 = 36;
pub const KEY_8: i32 = 37;
pub const KEY_9: i32 = 38;
pub const KEY_0: i32 = 39;
pub const KEY_RETURN: i32 = 40;
pub const KEY_ESCAPE: i32 = 41;
pub const KEY_BACKSPACE: i32 = 42;
pub const KEY_TAB: i32 = 43;
pub const KEY_SPACE: i32 = 44;
pub const KEY_UP: i32 = 82;
pub const KEY_DOWN: i32 = 81;
pub const KEY_LEFT: i32 = 80;
pub const KEY_RIGHT: i32 = 79;

// ============================================================================
// Window Management
// ============================================================================

/// Create a new window
/// Returns window handle, or 0 on error
#[no_mangle]
pub extern "C" fn window_create(title: StringPtr, width: i32, height: i32) -> u64 {
    if !ensure_init() {
        return 0;
    }

    let title_str = match unsafe { string_as_str(title) } {
        Some(s) => s,
        None => "Window",
    };

    STATE.with(|state| {
        let mut state_ref = state.borrow_mut();
        let state = match state_ref.as_mut() {
            Some(s) => s,
            None => return 0,
        };

        let video = match state.context.video() {
            Ok(v) => v,
            Err(_) => return 0,
        };

        let window = video.window(title_str, width as u32, height as u32)
            .position_centered()
            .build();

        let window = match window {
            Ok(w) => w,
            Err(_) => return 0,
        };

        let canvas = match window.into_canvas().software().build() {
            Ok(c) => c,
            Err(_) => return 0,
        };

        let id = state.next_id;
        state.next_id += 1;
        state.windows.insert(id, WindowData {
            canvas,
            is_open: true,
        });
        id
    })
}

/// Create a window at specific position
#[no_mangle]
pub extern "C" fn window_create_at(title: StringPtr, x: i32, y: i32, width: i32, height: i32) -> u64 {
    if !ensure_init() {
        return 0;
    }

    let title_str = match unsafe { string_as_str(title) } {
        Some(s) => s,
        None => "Window",
    };

    STATE.with(|state| {
        let mut state_ref = state.borrow_mut();
        let state = match state_ref.as_mut() {
            Some(s) => s,
            None => return 0,
        };

        let video = match state.context.video() {
            Ok(v) => v,
            Err(_) => return 0,
        };

        let window = video.window(title_str, width as u32, height as u32)
            .position(x, y)
            .build();

        let window = match window {
            Ok(w) => w,
            Err(_) => return 0,
        };

        let canvas = match window.into_canvas().software().build() {
            Ok(c) => c,
            Err(_) => return 0,
        };

        let id = state.next_id;
        state.next_id += 1;
        state.windows.insert(id, WindowData {
            canvas,
            is_open: true,
        });
        id
    })
}

/// Destroy a window
#[no_mangle]
pub extern "C" fn window_destroy(handle: u64) {
    STATE.with(|state| {
        if let Some(s) = state.borrow_mut().as_mut() {
            s.windows.remove(&handle);
        }
    });
}

/// Check if window is open
#[no_mangle]
pub extern "C" fn window_is_open(handle: u64) -> i32 {
    STATE.with(|state| {
        if let Some(s) = state.borrow().as_ref() {
            if let Some(win) = s.windows.get(&handle) {
                return if win.is_open { 1 } else { 0 };
            }
        }
        0
    })
}

/// Close a window (marks it for closing, doesn't destroy)
#[no_mangle]
pub extern "C" fn window_close(handle: u64) {
    STATE.with(|state| {
        if let Some(s) = state.borrow_mut().as_mut() {
            if let Some(win) = s.windows.get_mut(&handle) {
                win.is_open = false;
            }
        }
    });
}

/// Set window title
#[no_mangle]
pub extern "C" fn window_set_title(handle: u64, title: StringPtr) -> i32 {
    let title_str = match unsafe { string_as_str(title) } {
        Some(s) => s,
        None => return 0,
    };

    STATE.with(|state| {
        if let Some(s) = state.borrow_mut().as_mut() {
            if let Some(win) = s.windows.get_mut(&handle) {
                win.canvas.window_mut().set_title(title_str).ok();
                return 1;
            }
        }
        0
    })
}

/// Get window title
#[no_mangle]
pub extern "C" fn window_get_title(handle: u64) -> StringPtr {
    STATE.with(|state| {
        if let Some(s) = state.borrow().as_ref() {
            if let Some(win) = s.windows.get(&handle) {
                let title = win.canvas.window().title();
                return string_new(title);
            }
        }
        string_new("")
    })
}

/// Get window size
#[no_mangle]
pub extern "C" fn window_get_size(handle: u64) -> WindowSize {
    STATE.with(|state| {
        if let Some(s) = state.borrow().as_ref() {
            if let Some(win) = s.windows.get(&handle) {
                let (w, h) = win.canvas.window().size();
                return WindowSize { width: w as i32, height: h as i32 };
            }
        }
        WindowSize::default()
    })
}

/// Show window
#[no_mangle]
pub extern "C" fn window_show(handle: u64) {
    STATE.with(|state| {
        if let Some(s) = state.borrow_mut().as_mut() {
            if let Some(win) = s.windows.get_mut(&handle) {
                win.canvas.window_mut().show();
            }
        }
    });
}

/// Hide window
#[no_mangle]
pub extern "C" fn window_hide(handle: u64) {
    STATE.with(|state| {
        if let Some(s) = state.borrow_mut().as_mut() {
            if let Some(win) = s.windows.get_mut(&handle) {
                win.canvas.window_mut().hide();
            }
        }
    });
}

// ============================================================================
// Drawing Functions
// ============================================================================

/// Convert RGBA u32 (0xRRGGBBAA) to SDL Color
fn rgba_to_color(rgba: u32) -> Color {
    let r = ((rgba >> 24) & 0xFF) as u8;
    let g = ((rgba >> 16) & 0xFF) as u8;
    let b = ((rgba >> 8) & 0xFF) as u8;
    let a = (rgba & 0xFF) as u8;
    Color::RGBA(r, g, b, a)
}

/// Clear window with color (RGBA: 0xRRGGBBAA)
#[no_mangle]
pub extern "C" fn window_clear(handle: u64, color: u32) {
    STATE.with(|state| {
        if let Some(s) = state.borrow_mut().as_mut() {
            if let Some(win) = s.windows.get_mut(&handle) {
                win.canvas.set_draw_color(rgba_to_color(color));
                win.canvas.clear();
            }
        }
    });
}

/// Set draw color for subsequent draw operations (RGBA: 0xRRGGBBAA)
#[no_mangle]
pub extern "C" fn window_set_color(handle: u64, color: u32) {
    STATE.with(|state| {
        if let Some(s) = state.borrow_mut().as_mut() {
            if let Some(win) = s.windows.get_mut(&handle) {
                win.canvas.set_draw_color(rgba_to_color(color));
            }
        }
    });
}

/// Present the frame (flip buffers)
#[no_mangle]
pub extern "C" fn window_present(handle: u64) {
    STATE.with(|state| {
        if let Some(s) = state.borrow_mut().as_mut() {
            if let Some(win) = s.windows.get_mut(&handle) {
                win.canvas.present();
            }
        }
    });
}

/// Draw a single pixel
#[no_mangle]
pub extern "C" fn window_draw_pixel(handle: u64, x: i32, y: i32) -> i32 {
    STATE.with(|state| {
        if let Some(s) = state.borrow_mut().as_mut() {
            if let Some(win) = s.windows.get_mut(&handle) {
                return win.canvas.draw_point((x, y)).map(|_| 1).unwrap_or(0);
            }
        }
        0
    })
}

/// Draw a line
#[no_mangle]
pub extern "C" fn window_draw_line(handle: u64, x1: i32, y1: i32, x2: i32, y2: i32) -> i32 {
    STATE.with(|state| {
        if let Some(s) = state.borrow_mut().as_mut() {
            if let Some(win) = s.windows.get_mut(&handle) {
                return win.canvas.draw_line((x1, y1), (x2, y2)).map(|_| 1).unwrap_or(0);
            }
        }
        0
    })
}

/// Draw a rectangle outline
#[no_mangle]
pub extern "C" fn window_draw_rect(handle: u64, x: i32, y: i32, w: i32, h: i32) -> i32 {
    STATE.with(|state| {
        if let Some(s) = state.borrow_mut().as_mut() {
            if let Some(win) = s.windows.get_mut(&handle) {
                return win.canvas.draw_rect(Rect::new(x, y, w as u32, h as u32)).map(|_| 1).unwrap_or(0);
            }
        }
        0
    })
}

/// Fill a rectangle
#[no_mangle]
pub extern "C" fn window_fill_rect(handle: u64, x: i32, y: i32, w: i32, h: i32) -> i32 {
    STATE.with(|state| {
        if let Some(s) = state.borrow_mut().as_mut() {
            if let Some(win) = s.windows.get_mut(&handle) {
                return win.canvas.fill_rect(Rect::new(x, y, w as u32, h as u32)).map(|_| 1).unwrap_or(0);
            }
        }
        0
    })
}

/// Draw a colored pixel (sets color and draws)
#[no_mangle]
pub extern "C" fn window_draw_pixel_color(handle: u64, x: i32, y: i32, color: u32) -> i32 {
    STATE.with(|state| {
        if let Some(s) = state.borrow_mut().as_mut() {
            if let Some(win) = s.windows.get_mut(&handle) {
                win.canvas.set_draw_color(rgba_to_color(color));
                return win.canvas.draw_point((x, y)).map(|_| 1).unwrap_or(0);
            }
        }
        0
    })
}

/// Fill a colored rectangle (sets color and fills)
#[no_mangle]
pub extern "C" fn window_fill_rect_color(handle: u64, x: i32, y: i32, w: i32, h: i32, color: u32) -> i32 {
    STATE.with(|state| {
        if let Some(s) = state.borrow_mut().as_mut() {
            if let Some(win) = s.windows.get_mut(&handle) {
                win.canvas.set_draw_color(rgba_to_color(color));
                return win.canvas.fill_rect(Rect::new(x, y, w as u32, h as u32)).map(|_| 1).unwrap_or(0);
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
/// This is the primary way to render a zrtl_paint canvas to a window:
/// ```text
/// let canvas = $Paint$canvas_create(800, 600);
/// // ... draw on canvas ...
/// let buffer = $Paint$canvas_get_buffer(canvas);
/// $Window$blit(win, buffer.data, buffer.width, buffer.height, buffer.pitch, 0, 0);
/// $Window$present(win);
/// ```
///
/// Parameters:
/// - handle: Window handle
/// - data: Pointer to RGBA pixel data
/// - width, height: Source image dimensions
/// - pitch: Source row stride in bytes (usually width * 4)
/// - dst_x, dst_y: Destination position in window
///
/// Returns 1 on success, 0 on error
#[no_mangle]
pub extern "C" fn window_blit(
    handle: u64,
    data: *const u8,
    width: i32,
    height: i32,
    pitch: i32,
    dst_x: i32,
    dst_y: i32,
) -> i32 {
    if data.is_null() || width <= 0 || height <= 0 {
        return 0;
    }

    STATE.with(|state| {
        if let Some(s) = state.borrow_mut().as_mut() {
            if let Some(win) = s.windows.get_mut(&handle) {
                let texture_creator = win.canvas.texture_creator();

                let mut texture = match texture_creator.create_texture_streaming(
                    sdl2::pixels::PixelFormatEnum::RGBA32,
                    width as u32,
                    height as u32,
                ) {
                    Ok(t) => t,
                    Err(_) => return 0,
                };

                let src_slice = unsafe {
                    std::slice::from_raw_parts(data, (height * pitch) as usize)
                };

                if texture.update(None, src_slice, pitch as usize).is_err() {
                    return 0;
                }

                let dst_rect = Rect::new(dst_x, dst_y, width as u32, height as u32);
                if win.canvas.copy(&texture, None, Some(dst_rect)).is_err() {
                    return 0;
                }

                return 1;
            }
        }
        0
    })
}

/// Blit RGBA pixel data to window, scaled to fit destination
///
/// Parameters:
/// - handle: Window handle
/// - data: Pointer to RGBA pixel data
/// - src_w, src_h: Source image dimensions
/// - pitch: Source row stride in bytes
/// - dst_x, dst_y, dst_w, dst_h: Destination rectangle
///
/// Returns 1 on success, 0 on error
#[no_mangle]
pub extern "C" fn window_blit_scaled(
    handle: u64,
    data: *const u8,
    src_w: i32,
    src_h: i32,
    pitch: i32,
    dst_x: i32,
    dst_y: i32,
    dst_w: i32,
    dst_h: i32,
) -> i32 {
    if data.is_null() || src_w <= 0 || src_h <= 0 {
        return 0;
    }

    STATE.with(|state| {
        if let Some(s) = state.borrow_mut().as_mut() {
            if let Some(win) = s.windows.get_mut(&handle) {
                let texture_creator = win.canvas.texture_creator();

                let mut texture = match texture_creator.create_texture_streaming(
                    sdl2::pixels::PixelFormatEnum::RGBA32,
                    src_w as u32,
                    src_h as u32,
                ) {
                    Ok(t) => t,
                    Err(_) => return 0,
                };

                let src_slice = unsafe {
                    std::slice::from_raw_parts(data, (src_h * pitch) as usize)
                };

                if texture.update(None, src_slice, pitch as usize).is_err() {
                    return 0;
                }

                let dst_rect = Rect::new(dst_x, dst_y, dst_w as u32, dst_h as u32);
                if win.canvas.copy(&texture, None, Some(dst_rect)).is_err() {
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

fn convert_event(event: Event, windows: &mut HashMap<u64, WindowData>) -> WindowEvent {
    let mut data = WindowEvent::default();

    match event {
        Event::Quit { .. } => {
            data.event_type = EVENT_QUIT;
            // Mark all windows as closed
            for win in windows.values_mut() {
                win.is_open = false;
            }
        }
        Event::KeyDown { scancode, repeat, window_id, .. } => {
            data.event_type = EVENT_KEY_DOWN;
            data.window_id = window_id as u64;
            data.key = scancode.map(|s| s as i32).unwrap_or(0);
            data.key_repeat = if repeat { 1 } else { 0 };
        }
        Event::KeyUp { scancode, window_id, .. } => {
            data.event_type = EVENT_KEY_UP;
            data.window_id = window_id as u64;
            data.key = scancode.map(|s| s as i32).unwrap_or(0);
        }
        Event::MouseMotion { x, y, window_id, .. } => {
            data.event_type = EVENT_MOUSE_MOVE;
            data.window_id = window_id as u64;
            data.mouse_x = x;
            data.mouse_y = y;
        }
        Event::MouseButtonDown { x, y, mouse_btn, window_id, .. } => {
            data.event_type = EVENT_MOUSE_DOWN;
            data.window_id = window_id as u64;
            data.mouse_x = x;
            data.mouse_y = y;
            data.mouse_button = mouse_btn as i32;
        }
        Event::MouseButtonUp { x, y, mouse_btn, window_id, .. } => {
            data.event_type = EVENT_MOUSE_UP;
            data.window_id = window_id as u64;
            data.mouse_x = x;
            data.mouse_y = y;
            data.mouse_button = mouse_btn as i32;
        }
        Event::MouseWheel { x, y, window_id, .. } => {
            data.event_type = EVENT_MOUSE_WHEEL;
            data.window_id = window_id as u64;
            data.wheel_x = x;
            data.wheel_y = y;
        }
        Event::Window { win_event, window_id, .. } => {
            match win_event {
                sdl2::event::WindowEvent::Resized(w, h) |
                sdl2::event::WindowEvent::SizeChanged(w, h) => {
                    data.event_type = EVENT_RESIZE;
                    data.window_id = window_id as u64;
                    data.width = w;
                    data.height = h;
                }
                sdl2::event::WindowEvent::Close => {
                    data.event_type = EVENT_CLOSE;
                    data.window_id = window_id as u64;
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
pub extern "C" fn window_poll_event() -> WindowEvent {
    STATE.with(|state| {
        if let Some(s) = state.borrow_mut().as_mut() {
            match s.event_pump.poll_event() {
                Some(event) => convert_event(event, &mut s.windows),
                None => WindowEvent::default(),
            }
        } else {
            WindowEvent::default()
        }
    })
}

/// Wait for an event (blocking)
#[no_mangle]
pub extern "C" fn window_wait_event() -> WindowEvent {
    STATE.with(|state| {
        if let Some(s) = state.borrow_mut().as_mut() {
            let event = s.event_pump.wait_event();
            convert_event(event, &mut s.windows)
        } else {
            WindowEvent::default()
        }
    })
}

/// Wait for event with timeout (milliseconds)
#[no_mangle]
pub extern "C" fn window_wait_event_timeout(timeout_ms: u32) -> WindowEvent {
    STATE.with(|state| {
        if let Some(s) = state.borrow_mut().as_mut() {
            match s.event_pump.wait_event_timeout(timeout_ms) {
                Some(event) => convert_event(event, &mut s.windows),
                None => WindowEvent::default(),
            }
        } else {
            WindowEvent::default()
        }
    })
}

// ============================================================================
// Timing
// ============================================================================

/// Delay for specified milliseconds
#[no_mangle]
pub extern "C" fn window_delay(ms: u32) {
    std::thread::sleep(std::time::Duration::from_millis(ms as u64));
}

/// Get ticks since initialization (milliseconds)
#[no_mangle]
pub extern "C" fn window_get_ticks() -> u32 {
    unsafe { sdl2::sys::SDL_GetTicks() }
}

// ============================================================================
// Version
// ============================================================================

/// Get version string
#[no_mangle]
pub extern "C" fn window_version() -> *const c_char {
    static VERSION: &[u8] = b"0.1.0\0";
    VERSION.as_ptr() as *const c_char
}

// ============================================================================
// Plugin Export
// ============================================================================

zrtl_plugin! {
    name: "zrtl_window",
    symbols: [
        // Window Management
        ("$Window$create", window_create),
        ("$Window$create_at", window_create_at),
        ("$Window$destroy", window_destroy),
        ("$Window$is_open", window_is_open),
        ("$Window$close", window_close),
        ("$Window$set_title", window_set_title),
        ("$Window$get_title", window_get_title),
        ("$Window$get_size", window_get_size),
        ("$Window$show", window_show),
        ("$Window$hide", window_hide),

        // Drawing
        ("$Window$clear", window_clear),
        ("$Window$set_color", window_set_color),
        ("$Window$present", window_present),
        ("$Window$draw_pixel", window_draw_pixel),
        ("$Window$draw_line", window_draw_line),
        ("$Window$draw_rect", window_draw_rect),
        ("$Window$fill_rect", window_fill_rect),
        ("$Window$draw_pixel_color", window_draw_pixel_color),
        ("$Window$fill_rect_color", window_fill_rect_color),

        // Buffer Blitting (for zrtl_paint integration)
        ("$Window$blit", window_blit),
        ("$Window$blit_scaled", window_blit_scaled),

        // Events
        ("$Window$poll_event", window_poll_event),
        ("$Window$wait_event", window_wait_event),
        ("$Window$wait_event_timeout", window_wait_event_timeout),

        // Timing
        ("$Window$delay", window_delay),
        ("$Window$get_ticks", window_get_ticks),

        // Version
        ("$Window$version", window_version),
    ]
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::CStr;

    #[test]
    fn test_version() {
        let version = window_version();
        assert!(!version.is_null());
        let v = unsafe { CStr::from_ptr(version) };
        assert_eq!(v.to_str().unwrap(), "0.1.0");
    }

    #[test]
    fn test_event_default() {
        let event = WindowEvent::default();
        assert_eq!(event.event_type, EVENT_NONE);
        assert_eq!(event.key, 0);
        assert_eq!(event.mouse_x, 0);
    }

    #[test]
    fn test_rgba_to_color() {
        let color = rgba_to_color(0xFF00FF80);
        assert_eq!(color.r, 255);
        assert_eq!(color.g, 0);
        assert_eq!(color.b, 255);
        assert_eq!(color.a, 128);
    }

    #[test]
    fn test_delay() {
        use std::time::Instant;
        let start = Instant::now();
        window_delay(10);
        let elapsed = start.elapsed();
        assert!(elapsed.as_millis() >= 10);
    }

    #[test]
    fn test_window_size_default() {
        let size = WindowSize::default();
        assert_eq!(size.width, 0);
        assert_eq!(size.height, 0);
    }
}
