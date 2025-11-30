# zrtl_http

HTTP client for Zyntax-based languages.

## Overview

Provides HTTP client functionality with simple request functions, a request builder API for complex requests, and async support.

## Exported Symbols

### Simple Requests

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Http$get` | `(StringPtr) -> StringPtr` | GET request, returns body |
| `$Http$get_timeout` | `(StringPtr, u64) -> StringPtr` | GET with timeout (ms) |
| `$Http$post` | `(StringPtr, StringPtr) -> StringPtr` | POST with text body |
| `$Http$post_json` | `(StringPtr, StringPtr) -> StringPtr` | POST with JSON body |
| `$Http$post_form` | `(StringPtr, StringPtr) -> StringPtr` | POST with form data |

### Other HTTP Methods

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Http$put` | `(StringPtr, StringPtr) -> StringPtr` | PUT request |
| `$Http$patch` | `(StringPtr, StringPtr) -> StringPtr` | PATCH request |
| `$Http$delete` | `(StringPtr) -> StringPtr` | DELETE request |
| `$Http$head` | `(StringPtr) -> i32` | HEAD request (returns status) |

### With Headers

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Http$get_with_header` | `(StringPtr, StringPtr, StringPtr) -> StringPtr` | GET with custom header |
| `$Http$get_with_bearer` | `(StringPtr, StringPtr) -> StringPtr` | GET with Bearer token |
| `$Http$post_json_with_bearer` | `(StringPtr, StringPtr, StringPtr) -> StringPtr` | POST JSON with Bearer |

### Response Info

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Http$status` | `(StringPtr) -> i32` | Get URL status code (HEAD request) |
| `$Http$is_ok` | `(StringPtr) -> i32` | Check if URL returns 2xx |

### URL Encoding

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Http$url_encode` | `(StringPtr) -> StringPtr` | URL-encode a string |
| `$Http$url_decode` | `(StringPtr) -> StringPtr` | URL-decode a string |

### Request Builder API

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Http$request_new` | `(StringPtr, StringPtr) -> i64` | Create request (method, url) |
| `$Http$request_header` | `(i64, StringPtr, StringPtr) -> i32` | Add header |
| `$Http$request_body` | `(i64, StringPtr) -> i32` | Set body |
| `$Http$request_timeout` | `(i64, u64) -> i32` | Set timeout (ms) |
| `$Http$request_send` | `(i64) -> i64` | Send request (returns response handle) |
| `$Http$request_free` | `(i64) -> ()` | Free request (if not sent) |

### Response API

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Http$response_body` | `(i64) -> StringPtr` | Get response body |
| `$Http$response_status` | `(i64) -> i32` | Get status code |
| `$Http$response_header` | `(i64, StringPtr) -> StringPtr` | Get response header |
| `$Http$response_free` | `(i64) -> ()` | Free response handle |

### Async Operations

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Http$get_async` | `(StringPtr) -> *ZrtlPromise` | Async GET |
| `$Http$post_async` | `(StringPtr, StringPtr) -> *ZrtlPromise` | Async POST |
| `$Http$post_json_async` | `(StringPtr, StringPtr) -> *ZrtlPromise` | Async POST JSON |
| `$Http$request_send_async` | `(i64) -> *ZrtlPromise` | Async send builder |
| `$Http$promise_free` | `(*ZrtlPromise) -> ()` | Free promise |

## Usage Example

### Simple Requests

```zig
// GET request
const body = $Http$get("https://api.example.com/data");
$IO$println(body);
string_free(body);

// POST JSON
const response = $Http$post_json(
    "https://api.example.com/users",
    "{\"name\": \"Alice\"}"
);

// With Bearer token
const auth_response = $Http$get_with_bearer(
    "https://api.example.com/protected",
    "my-jwt-token"
);

// Check if URL is accessible
if ($Http$is_ok("https://example.com") == 1) {
    // Site is up
}
```

### Request Builder (Multiple Headers)

```zig
// Create request
const req = $Http$request_new("POST", "https://api.example.com/data");

// Add headers
$Http$request_header(req, "Content-Type", "application/json");
$Http$request_header(req, "Authorization", "Bearer my-token");
$Http$request_header(req, "X-Custom-Header", "value");

// Set body and timeout
$Http$request_body(req, "{\"key\": \"value\"}");
$Http$request_timeout(req, 30000);  // 30 seconds

// Send request
const resp = $Http$request_send(req);
if (resp != 0) {
    const status = $Http$response_status(resp);
    const body = $Http$response_body(resp);
    const content_type = $Http$response_header(resp, "content-type");

    $IO$println_i64(status);
    $IO$println(body);

    string_free(body);
    string_free(content_type);
    $Http$response_free(resp);
}
```

### Async Requests

```zig
// Start async GET
const promise = $Http$get_async("https://api.example.com/slow-endpoint");

// Do other work while waiting...

// Poll until ready
while (true) {
    const result = promise_poll(promise);
    if (result.is_ready()) {
        // Result is response handle
        const resp_handle = result.value();
        const body = $Http$response_body(resp_handle);
        $IO$println(body);
        string_free(body);
        $Http$response_free(resp_handle);
        break;
    } else if (result.is_failed()) {
        // Request failed
        break;
    }
    // Still pending - continue other work
}

$Http$promise_free(promise);
```

### URL Encoding

```zig
const query = $Http$url_encode("hello world");  // "hello%20world"
const decoded = $Http$url_decode("hello%20world");  // "hello world"

const url = $String$concat("https://api.example.com?q=", query);
const response = $Http$get(url);
```

## Return Values

- Simple requests return body string (empty on error)
- `$Http$request_send` returns response handle (0 on error)
- Status code functions return -1 on error

## Error Handling

Simple request functions return empty strings on error. For more control, use the request builder API which separates success/failure via the response handle (0 = error).

## Async Polling

Async operations return `ZrtlPromise` pointers. Poll result:
- Ready: Value is the response handle
- Failed: Request failed
- Pending: Still in progress

## Memory Management

- String results must be freed with `string_free`
- Response handles must be freed with `$Http$response_free`
- Request handles are consumed by `$Http$request_send` (or free with `$Http$request_free`)
- Promises must be freed with `$Http$promise_free`

## Dependencies

- `zrtl` - Core ZRTL SDK
- `ureq` - HTTP client
- `lazy_static` - Handle storage
