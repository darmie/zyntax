# zrtl_window

High-level cross-platform windowing API for Zyntax, built on SDL2.

This plugin provides a simplified interface for creating windows, handling events,
and drawing basic graphics. It automatically initializes SDL2 when needed.

## Quick Start

```zig
// Create a window (SDL is initialized automatically)
let win = $Window$create("My App", 800, 600);

// Main loop
while ($Window$is_open(win)) {
    // Poll events
    let event = $Window$poll_event();
    if (event.event_type == EVENT_KEY_DOWN && event.key == KEY_ESCAPE) {
        $Window$close(win);
    }

    // Clear screen (black with full alpha)
    $Window$clear(win, 0x000000FF);

    // Draw a red rectangle
    $Window$fill_rect_color(win, 100, 100, 50, 50, 0xFF0000FF);

    // Present frame
    $Window$present(win);

    // Target ~60 FPS
    $Window$delay(16);
}

// Cleanup
$Window$destroy(win);
```

## Functions

### Window Management

- `$Window$create(title: String, width: i32, height: i32) -> u64` - Create centered window
- `$Window$create_at(title: String, x: i32, y: i32, width: i32, height: i32) -> u64` - Create at position
- `$Window$destroy(handle: u64)` - Destroy window
- `$Window$is_open(handle: u64) -> i32` - Check if window is still open
- `$Window$close(handle: u64)` - Mark window for closing
- `$Window$set_title(handle: u64, title: String) -> i32` - Set title
- `$Window$get_title(handle: u64) -> String` - Get title
- `$Window$get_size(handle: u64) -> WindowSize` - Get size
- `$Window$show(handle: u64)` / `$Window$hide(handle: u64)` - Show/hide

### Drawing

Colors are specified as RGBA u32 values: `0xRRGGBBAA`
- Red: `0xFF0000FF`
- Green: `0x00FF00FF`
- Blue: `0x0000FFFF`
- White: `0xFFFFFFFF`
- Black: `0x000000FF`

Functions:

- `$Window$clear(handle: u64, color: u32)` - Clear with color
- `$Window$set_color(handle: u64, color: u32)` - Set draw color
- `$Window$present(handle: u64)` - Present frame
- `$Window$draw_pixel(handle: u64, x: i32, y: i32) -> i32` - Draw pixel
- `$Window$draw_line(handle: u64, x1: i32, y1: i32, x2: i32, y2: i32) -> i32` - Draw line
- `$Window$draw_rect(handle: u64, x: i32, y: i32, w: i32, h: i32) -> i32` - Draw rectangle outline
- `$Window$fill_rect(handle: u64, x: i32, y: i32, w: i32, h: i32) -> i32` - Fill rectangle
- `$Window$draw_pixel_color(handle: u64, x: i32, y: i32, color: u32) -> i32` - Draw colored pixel
- `$Window$fill_rect_color(handle: u64, x: i32, y: i32, w: i32, h: i32, color: u32) -> i32` - Fill colored rect

### Events

- `$Window$poll_event() -> WindowEvent` - Poll for events (non-blocking)
- `$Window$wait_event() -> WindowEvent` - Wait for event (blocking)
- `$Window$wait_event_timeout(timeout_ms: u32) -> WindowEvent` - Wait with timeout

### Timing

- `$Window$delay(ms: u32)` - Sleep for milliseconds
- `$Window$get_ticks() -> u32` - Get milliseconds since init

### Version

- `$Window$version() -> *c_char` - Get plugin version

## Event Types

| Constant | Value | Description |
|----------|-------|-------------|
| EVENT_NONE | 0 | No event |
| EVENT_QUIT | 1 | Application quit |
| EVENT_KEY_DOWN | 2 | Key pressed |
| EVENT_KEY_UP | 3 | Key released |
| EVENT_MOUSE_MOVE | 4 | Mouse moved |
| EVENT_MOUSE_DOWN | 5 | Mouse button pressed |
| EVENT_MOUSE_UP | 6 | Mouse button released |
| EVENT_MOUSE_WHEEL | 7 | Mouse wheel scrolled |
| EVENT_RESIZE | 8 | Window resized |
| EVENT_CLOSE | 9 | Window close requested |

## WindowEvent Structure

```c
struct WindowEvent {
    event_type: i32,    // Event type constant
    window_id: u64,     // Window handle
    key: i32,           // Key scancode
    key_repeat: i32,    // 1 if repeat, 0 otherwise
    mouse_x: i32,       // Mouse X position
    mouse_y: i32,       // Mouse Y position
    mouse_button: i32,  // Mouse button (1=left, 2=middle, 3=right)
    wheel_x: i32,       // Wheel delta X
    wheel_y: i32,       // Wheel delta Y
    width: i32,         // New width (resize)
    height: i32,        // New height (resize)
}
```

## Key Constants

Letters: `KEY_A` (4) through `KEY_Z` (29)
Numbers: `KEY_0` (39), `KEY_1` (30) through `KEY_9` (38)
Special: `KEY_RETURN` (40), `KEY_ESCAPE` (41), `KEY_BACKSPACE` (42),
         `KEY_TAB` (43), `KEY_SPACE` (44)
Arrows: `KEY_UP` (82), `KEY_DOWN` (81), `KEY_LEFT` (80), `KEY_RIGHT` (79)

## Dependencies

- `zrtl` (core ZRTL types)
- `sdl2` crate with `bundled` feature
