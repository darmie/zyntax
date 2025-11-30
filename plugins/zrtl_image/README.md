# zrtl_image

Image encoding and decoding for common formats.

## Supported Formats

- PNG, JPEG, GIF, WebP, BMP, ICO, TIFF

## Functions

### Loading/Saving
- `$Image$load(path: String) -> Handle` - Load image from file
- `$Image$load_bytes(data: Array[u8], format: i32) -> Handle` - Load from byte array
- `$Image$save(handle: Handle, path: String, format: i32) -> i32` - Save image
- `$Image$encode(handle: Handle, format: i32) -> Array[u8]` - Encode to bytes
- `$Image$free(handle: Handle)` - Free image

### Image Info
- `$Image$width(handle: Handle) -> u32` - Get width
- `$Image$height(handle: Handle) -> u32` - Get height
- `$Image$get_pixel(handle: Handle, x: u32, y: u32) -> Color` - Get pixel
- `$Image$set_pixel(handle: Handle, x: u32, y: u32, color: Color)` - Set pixel

### Creation
- `$Image$create(width: u32, height: u32) -> Handle` - Create blank RGBA image
- `$Image$clone(handle: Handle) -> Handle` - Clone image

### Manipulation
- `$Image$resize(handle: Handle, width: u32, height: u32) -> Handle` - Resize
- `$Image$crop(handle: Handle, x: u32, y: u32, w: u32, h: u32) -> Handle` - Crop
- `$Image$rotate90(handle: Handle) -> Handle` - Rotate 90 degrees
- `$Image$rotate180(handle: Handle) -> Handle` - Rotate 180 degrees
- `$Image$rotate270(handle: Handle) -> Handle` - Rotate 270 degrees
- `$Image$flip_horizontal(handle: Handle) -> Handle` - Flip horizontally
- `$Image$flip_vertical(handle: Handle) -> Handle` - Flip vertically
- `$Image$grayscale(handle: Handle) -> Handle` - Convert to grayscale
- `$Image$blur(handle: Handle, sigma: f32) -> Handle` - Gaussian blur
- `$Image$brighten(handle: Handle, amount: i32) -> Handle` - Adjust brightness
- `$Image$contrast(handle: Handle, amount: f32) -> Handle` - Adjust contrast
- `$Image$invert(handle: Handle) -> Handle` - Invert colors

## Format Constants

- `FORMAT_PNG = 0`
- `FORMAT_JPEG = 1`
- `FORMAT_GIF = 2`
- `FORMAT_WEBP = 3`
- `FORMAT_BMP = 4`
- `FORMAT_ICO = 5`
- `FORMAT_TIFF = 6`

## Example

```
// Load, resize, and save an image
let img = $Image$load("input.png");
let resized = $Image$resize(img, 800, 600);
$Image$save(resized, "output.jpg", FORMAT_JPEG);
$Image$free(resized);
$Image$free(img);
```

## Dependencies

- `image` crate (0.24)
