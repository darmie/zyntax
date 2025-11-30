# zrtl_websocket

WebSocket client for Zyntax-based languages.

## Overview

Provides WebSocket client functionality for real-time bidirectional communication with both synchronous and asynchronous APIs.

## Message Type Constants

| Constant | Value | Description |
|----------|-------|-------------|
| `MSG_TYPE_NONE` | 0 | Error/None |
| `MSG_TYPE_TEXT` | 1 | Text message |
| `MSG_TYPE_BINARY` | 2 | Binary message |
| `MSG_TYPE_CLOSE` | 3 | Connection closed |
| `MSG_TYPE_PING` | 4 | Ping (handled automatically) |
| `MSG_TYPE_PONG` | 5 | Pong |

## Exported Symbols

### Connection

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$WebSocket$connect` | `(StringPtr) -> i64` | Connect to server (0 on error) |
| `$WebSocket$close` | `(i64) -> ()` | Close connection |
| `$WebSocket$is_connected` | `(i64) -> i32` | Check if connected |

### Send Messages

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$WebSocket$send` | `(i64, StringPtr) -> i32` | Send text (1=ok, 0=error) |
| `$WebSocket$send_binary` | `(i64, ArrayPtr) -> i32` | Send binary |
| `$WebSocket$ping` | `(i64) -> i32` | Send ping |

### Receive Messages

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$WebSocket$recv` | `(i64) -> StringPtr` | Receive text (blocking) |
| `$WebSocket$recv_binary` | `(i64) -> ArrayPtr` | Receive binary (blocking) |
| `$WebSocket$recv_type` | `(i64) -> i32` | Receive and get message type |

### Async Operations

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$WebSocket$connect_async` | `(StringPtr) -> *ZrtlPromise` | Async connect |
| `$WebSocket$recv_async` | `(i64) -> *ZrtlPromise` | Async receive |
| `$WebSocket$send_async` | `(i64, StringPtr) -> *ZrtlPromise` | Async send |
| `$WebSocket$async_recv_result` | `(*ZrtlPromise) -> StringPtr` | Get recv result |
| `$WebSocket$promise_free` | `(*ZrtlPromise) -> ()` | Free promise |

## Usage Example

### Basic Echo Client

```zig
// Connect to WebSocket server
const ws = $WebSocket$connect("ws://echo.websocket.org");
if (ws == 0) {
    $IO$eprintln("Failed to connect");
    return;
}

// Send a message
if ($WebSocket$send(ws, "Hello, WebSocket!") == 1) {
    $IO$println("Message sent");
}

// Receive response (blocking)
const response = $WebSocket$recv(ws);
$IO$print("Received: ");
$IO$println(response);
string_free(response);

// Close connection
$WebSocket$close(ws);
```

### Real-time Chat

```zig
const ws = $WebSocket$connect("wss://chat.example.com/room/123");

// Send message
$WebSocket$send(ws, "{\"type\":\"join\",\"user\":\"Alice\"}");

// Receive loop
while ($WebSocket$is_connected(ws) == 1) {
    const msg = $WebSocket$recv(ws);
    if (msg != null && string_length(msg) > 0) {
        $IO$println(msg);
        string_free(msg);
    } else {
        // Empty message means connection closed
        break;
    }
}

$WebSocket$close(ws);
```

### Binary Messages

```zig
const ws = $WebSocket$connect("ws://binary.example.com");

// Send binary data
const data = array_new<u8>(4);
array_push(data, 0x01);
array_push(data, 0x02);
array_push(data, 0x03);
array_push(data, 0x04);
$WebSocket$send_binary(ws, data);
array_free(data);

// Receive binary
const received = $WebSocket$recv_binary(ws);
// Process binary data...
array_free(received);

$WebSocket$close(ws);
```

### Async WebSocket

```zig
// Async connect
const connect_promise = $WebSocket$connect_async("wss://api.example.com");

// Poll until connected
while (true) {
    const result = promise_poll(connect_promise);
    if (result.is_ready()) {
        const ws = result.value();  // WebSocket handle
        break;
    } else if (result.is_failed()) {
        $IO$eprintln("Connection failed");
        break;
    }
    // Do other work while waiting...
}
$WebSocket$promise_free(connect_promise);

// Async receive (non-blocking)
const recv_promise = $WebSocket$recv_async(ws);

// Poll for messages
while (true) {
    const result = promise_poll(recv_promise);
    if (result.is_ready()) {
        const msg = $WebSocket$async_recv_result(recv_promise);
        $IO$println(msg);
        // Don't free msg - it's owned by the promise
        break;
    } else if (result.is_failed()) {
        break;
    }
    // Non-blocking - can do other work
}

$WebSocket$promise_free(recv_promise);
$WebSocket$close(ws);
```

### Message Types

```zig
const ws = $WebSocket$connect("ws://example.com");

while (true) {
    const msg_type = $WebSocket$recv_type(ws);

    switch (msg_type) {
        MSG_TYPE_TEXT => {
            const text = $WebSocket$recv(ws);
            // Handle text
            string_free(text);
        },
        MSG_TYPE_BINARY => {
            const data = $WebSocket$recv_binary(ws);
            // Handle binary
            array_free(data);
        },
        MSG_TYPE_CLOSE => {
            $IO$println("Connection closed by server");
            break;
        },
        MSG_TYPE_NONE => {
            // Error or timeout
            break;
        },
        else => {}
    }
}
```

## URL Schemes

| Scheme | Description |
|--------|-------------|
| `ws://` | Unencrypted WebSocket |
| `wss://` | TLS-encrypted WebSocket (recommended) |

## Ping/Pong

- Pings from server are automatically answered with pongs
- Use `$WebSocket$ping()` to send a ping to the server
- Useful for keeping connections alive through proxies

## Return Values

- `$WebSocket$connect`: Handle (0 on error)
- `$WebSocket$send` / `$WebSocket$send_binary`: 1 on success, 0 on error
- `$WebSocket$recv`: Empty string on close/error

## Error Handling

- `$WebSocket$recv` returns empty string on close/error
- `$WebSocket$is_connected` returns 0 when connection is lost
- Check return values from send functions

## Memory Management

- Text messages from `$WebSocket$recv` must be freed
- Binary messages from `$WebSocket$recv_binary` must be freed
- Promises must be freed with `$WebSocket$promise_free`
- Results from `$WebSocket$async_recv_result` are freed when promise is freed

## Dependencies

- `zrtl` - Core ZRTL SDK
- `tungstenite` - WebSocket protocol
- `lazy_static` - Handle storage
