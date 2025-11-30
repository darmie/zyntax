# zrtl_paint

2D software rasterization for creative programming, charts, and general graphics.

## Rendering to Window

The paint plugin can render to zrtl_window or zrtl_sdl by exposing its pixel buffer:

```zig
// Create paint canvas and window with same dimensions
let canvas = $Paint$canvas_create(800, 600);
let win = $Window$create("Paint Demo", 800, 600);

// Draw on the canvas
let white = $Paint$rgb(255, 255, 255);
let red = $Paint$rgb(255, 0, 0);
$Paint$canvas_clear(canvas, white);
$Paint$fill_circle(canvas, 400, 300, 100, red);

// Blit canvas to window
let buffer = $Paint$canvas_get_buffer(canvas);
$Window$blit(win, buffer.data, buffer.width, buffer.height, buffer.pitch, 0, 0);
$Window$present(win);

// Main loop
while ($Window$is_open(win)) {
    let event = $Window$poll_event();
    // ... handle events, redraw, blit ...
}

$Paint$canvas_free(canvas);
$Window$destroy(win);
```

## Functions

### Colors

- `$Paint$rgba(r: u8, g: u8, b: u8, a: u8) -> Color` - Create RGBA color
- `$Paint$rgb(r: u8, g: u8, b: u8) -> Color` - Create RGB color (alpha=255)
- `$Paint$hex(hex: u32) -> Color` - Create color from hex (0xRRGGBB or 0xRRGGBBAA)

### Canvas Operations

- `$Paint$canvas_create(width: u32, height: u32) -> Handle` - Create canvas
- `$Paint$canvas_free(handle: Handle)` - Free canvas
- `$Paint$canvas_width(handle: Handle) -> u32` - Get width
- `$Paint$canvas_height(handle: Handle) -> u32` - Get height
- `$Paint$canvas_clear(handle: Handle, color: Color)` - Clear with color
- `$Paint$canvas_get_pixels(handle: Handle) -> Array[u8]` - Get raw RGBA pixels (copies data)
- `$Paint$canvas_save_png(handle: Handle, path: String) -> i32` - Save to PNG
- `$Paint$canvas_encode_png(handle: Handle) -> Array[u8]` - Encode to PNG bytes

### Buffer Access (for SDL/Window rendering)

- `$Paint$canvas_get_buffer(handle: Handle) -> CanvasBuffer` - Get buffer info struct
- `$Paint$canvas_data_ptr(handle: Handle) -> *u8` - Get raw pointer to pixels
- `$Paint$canvas_data_ptr_mut(handle: Handle) -> *mut u8` - Get mutable pointer
- `$Paint$canvas_data_len(handle: Handle) -> u64` - Get buffer length in bytes
- `$Paint$canvas_pitch(handle: Handle) -> u32` - Get row stride (bytes per row)

The `CanvasBuffer` struct contains:

```c
struct CanvasBuffer {
    data: *const u8,  // Pointer to RGBA pixel data
    len: u64,         // Total bytes
    width: u32,       // Width in pixels
    height: u32,      // Height in pixels
    pitch: u32,       // Bytes per row (width * 4)
}
```

### Stroke Settings

- `$Paint$set_stroke_width(handle: Handle, width: f32)` - Set stroke width
- `$Paint$set_line_cap(handle: Handle, cap: i32)` - Set line cap style
- `$Paint$set_line_join(handle: Handle, join: i32)` - Set line join style

### Transform

- `$Paint$transform_reset(handle: Handle)` - Reset to identity
- `$Paint$transform_translate(handle: Handle, x: f32, y: f32)` - Translate
- `$Paint$transform_rotate(handle: Handle, angle: f32)` - Rotate (radians)
- `$Paint$transform_scale(handle: Handle, sx: f32, sy: f32)` - Scale

### Drawing Primitives

- `$Paint$fill_rect(handle: Handle, x: f32, y: f32, w: f32, h: f32, color: Color)` - Fill rectangle
- `$Paint$stroke_rect(handle: Handle, x: f32, y: f32, w: f32, h: f32, color: Color)` - Stroke rectangle
- `$Paint$fill_circle(handle: Handle, cx: f32, cy: f32, r: f32, color: Color)` - Fill circle
- `$Paint$stroke_circle(handle: Handle, cx: f32, cy: f32, r: f32, color: Color)` - Stroke circle
- `$Paint$fill_ellipse(handle: Handle, cx: f32, cy: f32, rx: f32, ry: f32, color: Color)` - Fill ellipse
- `$Paint$stroke_ellipse(handle: Handle, cx: f32, cy: f32, rx: f32, ry: f32, color: Color)` - Stroke ellipse
- `$Paint$fill_rounded_rect(...)` - Fill rounded rectangle
- `$Paint$stroke_rounded_rect(...)` - Stroke rounded rectangle
- `$Paint$draw_line(handle: Handle, x1: f32, y1: f32, x2: f32, y2: f32, color: Color)` - Draw line

### Path Operations

- `$Paint$path_create() -> Handle` - Create new path
- `$Paint$path_free(handle: Handle)` - Free path
- `$Paint$path_move_to(handle: Handle, x: f32, y: f32)` - Move to point
- `$Paint$path_line_to(handle: Handle, x: f32, y: f32)` - Line to point
- `$Paint$path_quad_to(handle: Handle, cx: f32, cy: f32, x: f32, y: f32)` - Quadratic bezier
- `$Paint$path_cubic_to(handle: Handle, c1x: f32, c1y: f32, c2x: f32, c2y: f32, x: f32, y: f32)` - Cubic bezier
- `$Paint$path_close(handle: Handle)` - Close path
- `$Paint$path_fill(canvas: Handle, path: Handle, color: Color)` - Fill path
- `$Paint$path_stroke(canvas: Handle, path: Handle, color: Color)` - Stroke path

## Constants

### Line Cap

- `LINE_CAP_BUTT = 0`
- `LINE_CAP_ROUND = 1`
- `LINE_CAP_SQUARE = 2`

### Line Join

- `LINE_JOIN_MITER = 0`
- `LINE_JOIN_ROUND = 1`
- `LINE_JOIN_BEVEL = 2`

## Example

```zig
// Create a simple drawing
let canvas = $Paint$canvas_create(400, 400);
let white = $Paint$rgb(255, 255, 255);
let red = $Paint$rgb(255, 0, 0);
let blue = $Paint$rgb(0, 0, 255);

// Clear background
$Paint$canvas_clear(canvas, white);

// Draw shapes
$Paint$fill_rect(canvas, 50, 50, 100, 100, red);
$Paint$fill_circle(canvas, 200, 200, 50, blue);

// Save to file
$Paint$canvas_save_png(canvas, "drawing.png");
$Paint$canvas_free(canvas);
```

## Dependencies

- `tiny-skia` crate (0.11)
