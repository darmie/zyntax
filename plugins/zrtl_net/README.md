# zrtl_net

TCP and UDP networking for Zyntax-based languages.

## Overview

Provides low-level TCP client/server and UDP socket functionality with both synchronous and asynchronous APIs.

## Exported Symbols

### TCP Client

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Net$tcp_connect` | `(StringPtr) -> u64` | Connect to TCP server (0 on error) |
| `$Net$tcp_connect_timeout` | `(StringPtr, u64) -> u64` | Connect with timeout (ms) |
| `$Net$tcp_read` | `(u64, u32) -> ArrayPtr` | Read up to N bytes |
| `$Net$tcp_write` | `(u64, ArrayPtr) -> i64` | Write bytes (-1 on error) |
| `$Net$tcp_write_string` | `(u64, StringPtr) -> i64` | Write string (-1 on error) |
| `$Net$tcp_close` | `(u64) -> ()` | Close connection |
| `$Net$tcp_set_read_timeout` | `(u64, u64) -> i32` | Set read timeout (0=none) |
| `$Net$tcp_set_write_timeout` | `(u64, u64) -> i32` | Set write timeout (0=none) |

### TCP Server

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Net$tcp_listen` | `(StringPtr) -> u64` | Create listener (0 on error) |
| `$Net$tcp_accept` | `(u64) -> u64` | Accept connection (blocking) |
| `$Net$tcp_listener_close` | `(u64) -> ()` | Close listener |

### UDP

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Net$udp_bind` | `(StringPtr) -> u64` | Create UDP socket (0 on error) |
| `$Net$udp_send_to` | `(u64, StringPtr, ArrayPtr) -> i64` | Send to address |
| `$Net$udp_recv` | `(u64, u32) -> ArrayPtr` | Receive up to N bytes |
| `$Net$udp_close` | `(u64) -> ()` | Close socket |
| `$Net$udp_set_nonblocking` | `(u64, i32) -> i32` | Set non-blocking mode |

### Async Operations

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Net$tcp_connect_async` | `(StringPtr) -> *ZrtlPromise` | Async connect |
| `$Net$tcp_accept_async` | `(u64) -> *ZrtlPromise` | Async accept |
| `$Net$tcp_read_async` | `(u64, u32) -> *ZrtlPromise` | Async read |
| `$Net$async_read_result` | `(*ZrtlPromise) -> ArrayPtr` | Get read result |
| `$Net$promise_free` | `(*ZrtlPromise) -> ()` | Free promise |

## Usage Example

### TCP Client

```zig
// Connect to server
const sock = $Net$tcp_connect("127.0.0.1:8080");
if (sock == 0) {
    // Connection failed
    return;
}

// Set timeout
$Net$tcp_set_read_timeout(sock, 5000);  // 5 second timeout

// Send request
const request = "GET / HTTP/1.0\r\nHost: localhost\r\n\r\n";
$Net$tcp_write_string(sock, request);

// Read response
const response = $Net$tcp_read(sock, 4096);
if (response != null) {
    // Process response bytes
    array_free(response);
}

// Close connection
$Net$tcp_close(sock);
```

### TCP Server

```zig
// Create listener
const listener = $Net$tcp_listen("0.0.0.0:8080");
if (listener == 0) {
    // Bind failed
    return;
}

// Accept loop
while (true) {
    const client = $Net$tcp_accept(listener);  // Blocks
    if (client != 0) {
        // Handle client
        const data = $Net$tcp_read(client, 1024);
        // ... process data ...

        $Net$tcp_write_string(client, "HTTP/1.0 200 OK\r\n\r\nHello");
        $Net$tcp_close(client);
    }
}

$Net$tcp_listener_close(listener);
```

### UDP

```zig
// Create socket bound to port
const sock = $Net$udp_bind("0.0.0.0:9000");
if (sock == 0) {
    return;
}

// Send packet
const data = array_new<u8>(5);
array_push(data, 'h');
array_push(data, 'e');
array_push(data, 'l');
array_push(data, 'l');
array_push(data, 'o');
$Net$udp_send_to(sock, "127.0.0.1:9001", data);

// Receive packet
const received = $Net$udp_recv(sock, 1024);
if (received != null) {
    // Process packet
    array_free(received);
}

$Net$udp_close(sock);
```

### Async TCP

```zig
// Async connect
const connect_promise = $Net$tcp_connect_async("127.0.0.1:8080");

// Poll until ready
while (true) {
    const result = promise_poll(connect_promise);
    if (result.is_ready()) {
        const sock = result.value();
        // Connected!
        break;
    } else if (result.is_failed()) {
        // Connection failed
        break;
    }
    // Pending - do other work...
}

// Async read
const read_promise = $Net$tcp_read_async(sock, 4096);
// ... poll read_promise ...

// Get read result when ready
const data = $Net$async_read_result(read_promise);

// Cleanup
$Net$promise_free(connect_promise);
$Net$promise_free(read_promise);
```

## Address Format

Addresses use standard `host:port` format:
- `"127.0.0.1:8080"` - IPv4
- `"[::1]:8080"` - IPv6
- `"localhost:8080"` - Hostname
- `"0.0.0.0:8080"` - All interfaces (for listening)

## Return Values

- Handle functions return 0 on error
- Read/write functions return -1 on error
- Read functions return null array on error

## Async Polling

Async operations return `ZrtlPromise` pointers. Poll with the promise's poll function:
- `PollResult::Pending` (0) - Not ready yet
- `PollResult::Ready(value)` - Completed, value is handle/count
- `PollResult::Failed(error)` - Error occurred

## Memory Management

- Array results must be freed with `array_free`
- Promises must be freed with `$Net$promise_free`
- Connections should be closed with `tcp_close` / `udp_close`

## Dependencies

- `zrtl` - Core ZRTL SDK
- `lazy_static` - Handle storage
