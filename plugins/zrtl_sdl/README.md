# zrtl_sdl

Low-level SDL2 bindings for Zyntax providing direct access to SDL2's windowing,
rendering, and event handling capabilities.

## Architecture

SDL2 types are NOT thread-safe (they use Rc and raw pointers internally). This plugin
uses thread-local storage to ensure safety. **All SDL operations MUST be called from
the same thread** (typically the main thread).

For a simpler, higher-level API, consider using `zrtl_window` instead.

## Quick Start

```
// Initialize SDL
$Sdl$init();

// Create a centered window
let win = $Sdl$window_create_centered("My Game", 800, 600);

// Main loop
let running = true;
while (running) {
    // Poll events
    let event = $Sdl$poll_event();
    if (event.event_type == EVENT_QUIT) {
        running = false;
    }

    // Clear screen (black)
    $Sdl$set_draw_color(win, 0, 0, 0, 255);
    $Sdl$clear(win);

    // Draw a red rectangle
    $Sdl$set_draw_color(win, 255, 0, 0, 255);
    $Sdl$fill_rect(win, 100, 100, 50, 50);

    // Present frame
    $Sdl$present(win);

    // Target ~60 FPS
    $Sdl$delay(16);
}

// Cleanup
$Sdl$window_destroy(win);
$Sdl$quit();
```

## Functions

### Initialization
- `$Sdl$init() -> i32` - Initialize SDL (returns 1 on success, 0 on error)
- `$Sdl$quit()` - Quit SDL and cleanup all resources
- `$Sdl$is_init() -> i32` - Check if SDL is initialized

### Window Management
- `$Sdl$window_create(title: String, x: i32, y: i32, width: u32, height: u32) -> u64` - Create window
- `$Sdl$window_create_centered(title: String, width: u32, height: u32) -> u64` - Create centered window
- `$Sdl$window_destroy(handle: u64)` - Destroy window
- `$Sdl$window_set_title(handle: u64, title: String) -> i32` - Set window title
- `$Sdl$window_get_size(handle: u64) -> SdlWindowSize` - Get window size
- `$Sdl$window_show(handle: u64)` / `$Sdl$window_hide(handle: u64)` - Show/hide window

### Rendering
- `$Sdl$set_draw_color(handle: u64, r: u8, g: u8, b: u8, a: u8)` - Set RGBA draw color
- `$Sdl$clear(handle: u64)` - Clear canvas with current color
- `$Sdl$present(handle: u64)` - Present rendered frame
- `$Sdl$draw_point(handle: u64, x: i32, y: i32) -> i32` - Draw point
- `$Sdl$draw_line(handle: u64, x1: i32, y1: i32, x2: i32, y2: i32) -> i32` - Draw line
- `$Sdl$draw_rect(handle: u64, x: i32, y: i32, w: u32, h: u32) -> i32` - Draw rectangle outline
- `$Sdl$fill_rect(handle: u64, x: i32, y: i32, w: u32, h: u32) -> i32` - Fill rectangle
- `$Sdl$draw_rects(handle: u64, rects: *Rect, count: usize) -> i32` - Draw multiple rectangles
- `$Sdl$fill_rects(handle: u64, rects: *Rect, count: usize) -> i32` - Fill multiple rectangles

### Events
- `$Sdl$poll_event() -> SdlEvent` - Poll for events (non-blocking)
- `$Sdl$wait_event() -> SdlEvent` - Wait for events (blocking)
- `$Sdl$wait_event_timeout(timeout_ms: u32) -> SdlEvent` - Wait with timeout

### Timing
- `$Sdl$get_ticks() -> u32` - Get milliseconds since SDL init
- `$Sdl$delay(ms: u32)` - Sleep for milliseconds

### Version
- `$Sdl$version() -> *c_char` - Get plugin version

## Event Types

| Constant | Value | Description |
|----------|-------|-------------|
| EVENT_NONE | 0 | No event |
| EVENT_QUIT | 1 | Application quit requested |
| EVENT_KEY_DOWN | 2 | Key pressed |
| EVENT_KEY_UP | 3 | Key released |
| EVENT_MOUSE_MOTION | 4 | Mouse moved |
| EVENT_MOUSE_BUTTON_DOWN | 5 | Mouse button pressed |
| EVENT_MOUSE_BUTTON_UP | 6 | Mouse button released |
| EVENT_MOUSE_WHEEL | 7 | Mouse wheel scrolled |
| EVENT_WINDOW_RESIZED | 8 | Window resized |
| EVENT_WINDOW_CLOSE | 9 | Window close requested |

## SdlEvent Structure

```
struct SdlEvent {
    event_type: u32,      // Event type constant
    window_id: u32,       // SDL window ID
    key_scancode: i32,    // Key scancode (for key events)
    key_repeat: i32,      // 1 if key repeat, 0 otherwise
    mouse_x: i32,         // Mouse X position
    mouse_y: i32,         // Mouse Y position
    mouse_button: u8,     // Mouse button (1=left, 2=middle, 3=right)
    mouse_clicks: u8,     // Number of clicks
    wheel_x: i32,         // Wheel delta X
    wheel_y: i32,         // Wheel delta Y
    window_width: i32,    // New window width (resize)
    window_height: i32,   // New window height (resize)
}
```

## Key Constants (Scancodes)

Letters: `SCANCODE_A` (4) through `SCANCODE_Z` (29)
Numbers: `SCANCODE_0` (39), `SCANCODE_1` (30) through `SCANCODE_9` (38)
Special: `SCANCODE_RETURN` (40), `SCANCODE_ESCAPE` (41), `SCANCODE_BACKSPACE` (42),
         `SCANCODE_TAB` (43), `SCANCODE_SPACE` (44)
Arrows: `SCANCODE_UP` (82), `SCANCODE_DOWN` (81), `SCANCODE_LEFT` (80), `SCANCODE_RIGHT` (79)

## Mouse Button Constants

- `MOUSE_BUTTON_LEFT` = 1
- `MOUSE_BUTTON_MIDDLE` = 2
- `MOUSE_BUTTON_RIGHT` = 3

## Dependencies

- `zrtl` (core ZRTL types)
- `sdl2` crate with `bundled` feature (SDL2 compiled from source)
