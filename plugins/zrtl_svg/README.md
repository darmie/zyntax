# zrtl_svg

SVG parsing and rendering using resvg/usvg.

## Functions

### Parsing
- `$Svg$parse(data: String) -> Handle` - Parse SVG from string
- `$Svg$parse_file(path: String) -> Handle` - Parse SVG from file
- `$Svg$free(handle: Handle)` - Free SVG

### Information
- `$Svg$get_width(handle: Handle) -> f32` - Get SVG width
- `$Svg$get_height(handle: Handle) -> f32` - Get SVG height

### Rendering
- `$Svg$render(handle: Handle) -> RenderResult` - Render at natural size
- `$Svg$render_scaled(handle: Handle, scale: f32) -> RenderResult` - Render with scale

### Pixmap Operations
- `$Svg$pixmap_free(handle: Handle)` - Free pixmap
- `$Svg$pixmap_width(handle: Handle) -> u32` - Get pixmap width
- `$Svg$pixmap_height(handle: Handle) -> u32` - Get pixmap height
- `$Svg$pixmap_data(handle: Handle) -> *u8` - Get raw RGBA pixel data pointer
- `$Svg$pixmap_data_len(handle: Handle) -> u64` - Get pixel data length
- `$Svg$pixmap_save_png(handle: Handle, path: String) -> i32` - Save to PNG file
- `$Svg$pixmap_encode_png(handle: Handle) -> Array[u8]` - Encode to PNG bytes

### Version
- `$Svg$version() -> *c_char` - Get plugin version

## RenderResult Structure

```
struct RenderResult {
    pixmap_handle: u64,  // Handle to rendered pixmap (0 on error)
    width: u32,          // Rendered width in pixels
    height: u32,         // Rendered height in pixels
}
```

## Example

```
// Parse and render an SVG
let svg = $Svg$parse("<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"100\" height=\"100\"><circle cx=\"50\" cy=\"50\" r=\"40\" fill=\"blue\"/></svg>");

// Get dimensions
let width = $Svg$get_width(svg);
let height = $Svg$get_height(svg);

// Render at 2x scale
let result = $Svg$render_scaled(svg, 2.0);

// Save as PNG
if result.pixmap_handle != 0 {
    $Svg$pixmap_save_png(result.pixmap_handle, "output.png");
    $Svg$pixmap_free(result.pixmap_handle);
}

$Svg$free(svg);
```

## Notes

- SVG data is stored as strings and re-parsed when needed
- Rendered pixmaps are stored in RGBA format
- The plugin handles memory management for both SVG data and rendered pixmaps

## Dependencies

- `resvg` crate (0.37)
- `usvg` crate (0.37)
- `tiny-skia` crate (0.11)
