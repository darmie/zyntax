//! ZRTL WebSocket Plugin
//!
//! Provides WebSocket client functionality for real-time bidirectional communication.
//!
//! ## Connection
//! - `$WebSocket$connect` - Connect to a WebSocket server
//! - `$WebSocket$close` - Close connection
//! - `$WebSocket$is_connected` - Check if connected
//!
//! ## Messaging
//! - `$WebSocket$send` - Send text message
//! - `$WebSocket$send_binary` - Send binary message
//! - `$WebSocket$recv` - Receive message (blocking)
//! - `$WebSocket$try_recv` - Try to receive message (non-blocking)
//!
//! ## Async Operations
//! - `$WebSocket$connect_async` - Connect asynchronously (returns ZrtlPromise)
//! - `$WebSocket$recv_async` - Receive message asynchronously (returns ZrtlPromise)
//! - `$WebSocket$send_async` - Send message asynchronously (returns ZrtlPromise)

use std::collections::HashMap;
use std::sync::Mutex;
use std::sync::atomic::{AtomicI64, Ordering};
use std::net::TcpStream;
use tungstenite::{WebSocket, Message, connect, stream::MaybeTlsStream};
use zrtl::{
    zrtl_plugin, StringPtr, ArrayPtr, string_new, string_as_str, array_new, array_push, array_as_slice,
    ZrtlPromise, PollResult, StateMachineHeader,
};

// ============================================================================
// Handle Management
// ============================================================================

static NEXT_WS_ID: AtomicI64 = AtomicI64::new(1);

struct WsConnection {
    socket: WebSocket<MaybeTlsStream<TcpStream>>,
    connected: bool,
}

lazy_static::lazy_static! {
    static ref CONNECTIONS: Mutex<HashMap<i64, WsConnection>> = Mutex::new(HashMap::new());
}

// ============================================================================
// Connection Management
// ============================================================================

/// Connect to a WebSocket server
/// Returns handle on success, 0 on error
#[no_mangle]
pub extern "C" fn ws_connect(url: StringPtr) -> i64 {
    let url_str = match unsafe { string_as_str(url) } {
        Some(s) => s,
        None => return 0,
    };

    // tungstenite's connect accepts &str directly
    let (socket, _response) = match connect(url_str) {
        Ok(result) => result,
        Err(_) => return 0,
    };

    let id = NEXT_WS_ID.fetch_add(1, Ordering::SeqCst);
    if let Ok(mut conns) = CONNECTIONS.lock() {
        conns.insert(id, WsConnection { socket, connected: true });
        id
    } else {
        0
    }
}

/// Close WebSocket connection
#[no_mangle]
pub extern "C" fn ws_close(handle: i64) {
    if let Ok(mut conns) = CONNECTIONS.lock() {
        if let Some(mut conn) = conns.remove(&handle) {
            let _ = conn.socket.close(None);
        }
    }
}

/// Check if connection is still open
#[no_mangle]
pub extern "C" fn ws_is_connected(handle: i64) -> i32 {
    if let Ok(conns) = CONNECTIONS.lock() {
        if let Some(conn) = conns.get(&handle) {
            return conn.connected as i32;
        }
    }
    0
}

// ============================================================================
// Send Messages
// ============================================================================

/// Send a text message
/// Returns 1 on success, 0 on error
#[no_mangle]
pub extern "C" fn ws_send(handle: i64, message: StringPtr) -> i32 {
    let msg_str = match unsafe { string_as_str(message) } {
        Some(s) => s,
        None => return 0,
    };

    if let Ok(mut conns) = CONNECTIONS.lock() {
        if let Some(conn) = conns.get_mut(&handle) {
            if conn.connected {
                match conn.socket.send(Message::Text(msg_str.to_string())) {
                    Ok(_) => return 1,
                    Err(_) => {
                        conn.connected = false;
                        return 0;
                    }
                }
            }
        }
    }
    0
}

/// Send a binary message
/// Returns 1 on success, 0 on error
#[no_mangle]
pub extern "C" fn ws_send_binary(handle: i64, data: ArrayPtr) -> i32 {
    let bytes = if data.is_null() {
        vec![]
    } else {
        unsafe { array_as_slice::<u8>(data).to_vec() }
    };

    if let Ok(mut conns) = CONNECTIONS.lock() {
        if let Some(conn) = conns.get_mut(&handle) {
            if conn.connected {
                match conn.socket.send(Message::Binary(bytes)) {
                    Ok(_) => return 1,
                    Err(_) => {
                        conn.connected = false;
                        return 0;
                    }
                }
            }
        }
    }
    0
}

/// Send a ping
/// Returns 1 on success, 0 on error
#[no_mangle]
pub extern "C" fn ws_ping(handle: i64) -> i32 {
    if let Ok(mut conns) = CONNECTIONS.lock() {
        if let Some(conn) = conns.get_mut(&handle) {
            if conn.connected {
                match conn.socket.send(Message::Ping(vec![])) {
                    Ok(_) => return 1,
                    Err(_) => {
                        conn.connected = false;
                        return 0;
                    }
                }
            }
        }
    }
    0
}

// ============================================================================
// Receive Messages
// ============================================================================

/// Receive a text message (blocking)
/// Returns the message, or empty string on error/close
#[no_mangle]
pub extern "C" fn ws_recv(handle: i64) -> StringPtr {
    if let Ok(mut conns) = CONNECTIONS.lock() {
        if let Some(conn) = conns.get_mut(&handle) {
            if conn.connected {
                loop {
                    match conn.socket.read() {
                        Ok(Message::Text(text)) => {
                            return string_new(&text);
                        }
                        Ok(Message::Binary(data)) => {
                            // Try to interpret as UTF-8
                            match String::from_utf8(data) {
                                Ok(text) => return string_new(&text),
                                Err(e) => {
                                    // Return hex representation for binary
                                    let hex: String = e.into_bytes().iter().map(|b| format!("{:02x}", b)).collect();
                                    return string_new(&hex);
                                }
                            }
                        }
                        Ok(Message::Ping(data)) => {
                            // Respond to ping with pong
                            let _ = conn.socket.send(Message::Pong(data));
                            continue;
                        }
                        Ok(Message::Pong(_)) => {
                            // Ignore pongs
                            continue;
                        }
                        Ok(Message::Close(_)) => {
                            conn.connected = false;
                            return string_new("");
                        }
                        Ok(Message::Frame(_)) => {
                            continue;
                        }
                        Err(_) => {
                            conn.connected = false;
                            return string_new("");
                        }
                    }
                }
            }
        }
    }
    string_new("")
}

/// Receive a binary message (blocking)
/// Returns the data, or empty array on error/close
#[no_mangle]
pub extern "C" fn ws_recv_binary(handle: i64) -> ArrayPtr {
    if let Ok(mut conns) = CONNECTIONS.lock() {
        if let Some(conn) = conns.get_mut(&handle) {
            if conn.connected {
                loop {
                    match conn.socket.read() {
                        Ok(Message::Binary(data)) => {
                            let mut arr = array_new::<u8>(data.len());
                            for byte in data {
                                arr = unsafe { array_push(arr, byte) };
                            }
                            return arr;
                        }
                        Ok(Message::Text(text)) => {
                            let data = text.into_bytes();
                            let mut arr = array_new::<u8>(data.len());
                            for byte in data {
                                arr = unsafe { array_push(arr, byte) };
                            }
                            return arr;
                        }
                        Ok(Message::Ping(data)) => {
                            let _ = conn.socket.send(Message::Pong(data));
                            continue;
                        }
                        Ok(Message::Pong(_)) => {
                            continue;
                        }
                        Ok(Message::Close(_)) => {
                            conn.connected = false;
                            return array_new::<u8>(0);
                        }
                        Ok(Message::Frame(_)) => {
                            continue;
                        }
                        Err(_) => {
                            conn.connected = false;
                            return array_new::<u8>(0);
                        }
                    }
                }
            }
        }
    }
    array_new::<u8>(0)
}

// ============================================================================
// Message Type Constants
// ============================================================================

/// Message type: Text
pub const MSG_TYPE_TEXT: i32 = 1;
/// Message type: Binary
pub const MSG_TYPE_BINARY: i32 = 2;
/// Message type: Close
pub const MSG_TYPE_CLOSE: i32 = 3;
/// Message type: Ping
pub const MSG_TYPE_PING: i32 = 4;
/// Message type: Pong
pub const MSG_TYPE_PONG: i32 = 5;
/// Message type: Error/None
pub const MSG_TYPE_NONE: i32 = 0;

/// Receive and get message type (blocking)
/// Sets the message content in the provided callback
/// Returns message type constant
#[no_mangle]
pub extern "C" fn ws_recv_type(handle: i64) -> i32 {
    if let Ok(mut conns) = CONNECTIONS.lock() {
        if let Some(conn) = conns.get_mut(&handle) {
            if conn.connected {
                match conn.socket.read() {
                    Ok(Message::Text(_)) => return MSG_TYPE_TEXT,
                    Ok(Message::Binary(_)) => return MSG_TYPE_BINARY,
                    Ok(Message::Close(_)) => {
                        conn.connected = false;
                        return MSG_TYPE_CLOSE;
                    }
                    Ok(Message::Ping(data)) => {
                        let _ = conn.socket.send(Message::Pong(data));
                        return MSG_TYPE_PING;
                    }
                    Ok(Message::Pong(_)) => return MSG_TYPE_PONG,
                    Ok(Message::Frame(_)) => return MSG_TYPE_NONE,
                    Err(_) => {
                        conn.connected = false;
                        return MSG_TYPE_NONE;
                    }
                }
            }
        }
    }
    MSG_TYPE_NONE
}

// ============================================================================
// Plugin Export
// ============================================================================

zrtl_plugin! {
    name: "zrtl_websocket",
    symbols: [
        // Connection
        ("$WebSocket$connect", ws_connect),
        ("$WebSocket$close", ws_close),
        ("$WebSocket$is_connected", ws_is_connected),

        // Send
        ("$WebSocket$send", ws_send),
        ("$WebSocket$send_binary", ws_send_binary),
        ("$WebSocket$ping", ws_ping),

        // Receive
        ("$WebSocket$recv", ws_recv),
        ("$WebSocket$recv_binary", ws_recv_binary),
        ("$WebSocket$recv_type", ws_recv_type),

        // Async Operations
        ("$WebSocket$connect_async", ws_connect_async),
        ("$WebSocket$recv_async", ws_recv_async),
        ("$WebSocket$send_async", ws_send_async),
        ("$WebSocket$async_recv_result", ws_async_recv_result),
        ("$WebSocket$promise_free", ws_promise_free),
    ]
}

// ============================================================================
// Async Operations
// ============================================================================

/// Promise type tags for proper cleanup
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq)]
enum WsPromiseType {
    Connect = 1,
    Recv = 2,
    Send = 3,
}

/// State for async WebSocket connect
#[repr(C)]
struct ConnectState {
    header: StateMachineHeader,
    promise_type: WsPromiseType,
    url: String,
    result: i64,
}

/// Poll function for async connect
unsafe extern "C" fn connect_poll(state: *mut u8) -> i64 {
    let sm = &mut *(state as *mut ConnectState);

    // If already completed, return result
    if sm.header.async_state().is_finished() {
        return if sm.result > 0 {
            PollResult::Ready(sm.result).to_abi()
        } else {
            PollResult::Failed(-1).to_abi()
        };
    }

    // Try to connect
    match connect(&sm.url) {
        Ok((socket, _response)) => {
            let id = NEXT_WS_ID.fetch_add(1, Ordering::SeqCst);
            if let Ok(mut conns) = CONNECTIONS.lock() {
                conns.insert(id, WsConnection { socket, connected: true });
                sm.result = id;
                sm.header.set_completed();
                PollResult::Ready(id).to_abi()
            } else {
                sm.header.set_failed();
                PollResult::Failed(-2).to_abi()
            }
        }
        Err(_) => {
            sm.header.set_failed();
            PollResult::Failed(-1).to_abi()
        }
    }
}

/// Connect to WebSocket server asynchronously
/// Returns a ZrtlPromise pointer
#[no_mangle]
pub extern "C" fn ws_connect_async(url: StringPtr) -> *const ZrtlPromise {
    let url_str = match unsafe { string_as_str(url) } {
        Some(s) => s.to_string(),
        None => {
            // Return a failed promise
            let state = Box::new(ConnectState {
                header: StateMachineHeader::new(),
                promise_type: WsPromiseType::Connect,
                url: String::new(),
                result: 0,
            });
            let state_ptr = Box::into_raw(state) as *mut u8;
            // Mark as failed immediately
            unsafe { (*(state_ptr as *mut ConnectState)).header.set_failed(); }

            let promise = Box::new(unsafe { ZrtlPromise::new(state_ptr, connect_poll) });
            return Box::into_raw(promise);
        }
    };

    let state = Box::new(ConnectState {
        header: StateMachineHeader::new(),
        promise_type: WsPromiseType::Connect,
        url: url_str,
        result: 0,
    });
    let state_ptr = Box::into_raw(state) as *mut u8;

    let promise = Box::new(unsafe { ZrtlPromise::new(state_ptr, connect_poll) });
    Box::into_raw(promise)
}

/// State for async WebSocket receive
#[repr(C)]
struct RecvState {
    header: StateMachineHeader,
    promise_type: WsPromiseType,
    handle: i64,
    result_ptr: StringPtr,
}

/// Poll function for async receive - non-blocking check
unsafe extern "C" fn recv_poll(state: *mut u8) -> i64 {
    let sm = &mut *(state as *mut RecvState);

    // If already completed, return result
    if sm.header.async_state().is_finished() {
        return if !sm.result_ptr.is_null() {
            // Return handle value (we store it elsewhere, just signal ready)
            PollResult::Ready(sm.handle).to_abi()
        } else {
            PollResult::Failed(-1).to_abi()
        };
    }

    // Try non-blocking receive
    if let Ok(mut conns) = CONNECTIONS.lock() {
        if let Some(conn) = conns.get_mut(&sm.handle) {
            if !conn.connected {
                sm.header.set_failed();
                return PollResult::Failed(-1).to_abi();
            }

            // Set socket to non-blocking mode for poll check
            if let MaybeTlsStream::Plain(tcp) = conn.socket.get_mut() {
                let _ = tcp.set_nonblocking(true);
            }

            match conn.socket.read() {
                Ok(Message::Text(text)) => {
                    sm.result_ptr = string_new(&text);
                    sm.header.set_completed();

                    // Restore blocking mode
                    if let MaybeTlsStream::Plain(tcp) = conn.socket.get_mut() {
                        let _ = tcp.set_nonblocking(false);
                    }
                    return PollResult::Ready(sm.handle).to_abi();
                }
                Ok(Message::Binary(data)) => {
                    match String::from_utf8(data) {
                        Ok(text) => {
                            sm.result_ptr = string_new(&text);
                        }
                        Err(e) => {
                            let hex: String = e.into_bytes().iter().map(|b| format!("{:02x}", b)).collect();
                            sm.result_ptr = string_new(&hex);
                        }
                    }
                    sm.header.set_completed();

                    if let MaybeTlsStream::Plain(tcp) = conn.socket.get_mut() {
                        let _ = tcp.set_nonblocking(false);
                    }
                    return PollResult::Ready(sm.handle).to_abi();
                }
                Ok(Message::Ping(data)) => {
                    let _ = conn.socket.send(Message::Pong(data));
                    // Keep pending, not the message we want
                    return PollResult::Pending.to_abi();
                }
                Ok(Message::Pong(_)) => {
                    return PollResult::Pending.to_abi();
                }
                Ok(Message::Close(_)) => {
                    conn.connected = false;
                    sm.header.set_failed();
                    return PollResult::Failed(-2).to_abi();
                }
                Ok(Message::Frame(_)) => {
                    return PollResult::Pending.to_abi();
                }
                Err(tungstenite::Error::Io(ref e)) if e.kind() == std::io::ErrorKind::WouldBlock => {
                    // Not ready yet
                    return PollResult::Pending.to_abi();
                }
                Err(_) => {
                    conn.connected = false;
                    sm.header.set_failed();
                    return PollResult::Failed(-1).to_abi();
                }
            }
        }
    }

    sm.header.set_failed();
    PollResult::Failed(-3).to_abi()
}

/// Receive from WebSocket asynchronously
/// Returns a ZrtlPromise pointer. Poll result gives handle, use ws_async_recv_result to get string.
#[no_mangle]
pub extern "C" fn ws_recv_async(handle: i64) -> *const ZrtlPromise {
    let state = Box::new(RecvState {
        header: StateMachineHeader::new(),
        promise_type: WsPromiseType::Recv,
        handle,
        result_ptr: std::ptr::null_mut(),
    });
    let state_ptr = Box::into_raw(state) as *mut u8;

    let promise = Box::new(unsafe { ZrtlPromise::new(state_ptr, recv_poll) });
    Box::into_raw(promise)
}

/// Get the received message from a completed recv promise
/// The promise pointer should be from ws_recv_async after it completed
#[no_mangle]
pub extern "C" fn ws_async_recv_result(promise: *const ZrtlPromise) -> StringPtr {
    if promise.is_null() {
        return string_new("");
    }

    unsafe {
        let promise_ref = &*promise;
        let state = promise_ref.state_machine as *mut RecvState;
        if state.is_null() {
            return string_new("");
        }
        (*state).result_ptr
    }
}

/// State for async WebSocket send
#[repr(C)]
struct SendState {
    header: StateMachineHeader,
    promise_type: WsPromiseType,
    handle: i64,
    message: String,
}

/// Poll function for async send
unsafe extern "C" fn send_poll(state: *mut u8) -> i64 {
    let sm = &mut *(state as *mut SendState);

    if sm.header.async_state().is_finished() {
        return PollResult::Ready(1).to_abi();
    }

    if let Ok(mut conns) = CONNECTIONS.lock() {
        if let Some(conn) = conns.get_mut(&sm.handle) {
            if !conn.connected {
                sm.header.set_failed();
                return PollResult::Failed(-1).to_abi();
            }

            match conn.socket.send(Message::Text(sm.message.clone())) {
                Ok(_) => {
                    sm.header.set_completed();
                    PollResult::Ready(1).to_abi()
                }
                Err(_) => {
                    conn.connected = false;
                    sm.header.set_failed();
                    PollResult::Failed(-1).to_abi()
                }
            }
        } else {
            sm.header.set_failed();
            PollResult::Failed(-2).to_abi()
        }
    } else {
        sm.header.set_failed();
        PollResult::Failed(-3).to_abi()
    }
}

/// Send message asynchronously
/// Returns a ZrtlPromise pointer
#[no_mangle]
pub extern "C" fn ws_send_async(handle: i64, message: StringPtr) -> *const ZrtlPromise {
    let msg_str = unsafe { string_as_str(message) }.unwrap_or("").to_string();

    let state = Box::new(SendState {
        header: StateMachineHeader::new(),
        promise_type: WsPromiseType::Send,
        handle,
        message: msg_str,
    });
    let state_ptr = Box::into_raw(state) as *mut u8;

    let promise = Box::new(unsafe { ZrtlPromise::new(state_ptr, send_poll) });
    Box::into_raw(promise)
}

/// Free a promise after use (for memory cleanup)
///
/// This properly deallocates the state machine based on the promise type tag
/// stored in the state. The type tag is at a fixed offset after the header.
#[no_mangle]
pub extern "C" fn ws_promise_free(promise: *mut ZrtlPromise) {
    if promise.is_null() {
        return;
    }

    unsafe {
        let promise_box = Box::from_raw(promise);
        if !promise_box.state_machine.is_null() {
            // Read the promise type tag (it's right after the header in all state types)
            // ConnectState, RecvState, SendState all have: header, promise_type, ...
            let type_ptr = promise_box.state_machine.add(std::mem::size_of::<StateMachineHeader>());
            let promise_type = *(type_ptr as *const WsPromiseType);

            // Free the appropriate state type
            match promise_type {
                WsPromiseType::Connect => {
                    let _ = Box::from_raw(promise_box.state_machine as *mut ConnectState);
                }
                WsPromiseType::Recv => {
                    let state = Box::from_raw(promise_box.state_machine as *mut RecvState);
                    // Also free the result string if it was allocated
                    if !state.result_ptr.is_null() {
                        zrtl::string_free(state.result_ptr);
                    }
                }
                WsPromiseType::Send => {
                    let _ = Box::from_raw(promise_box.state_machine as *mut SendState);
                }
            }
        }
        // promise_box is dropped here, freeing the ZrtlPromise struct
    }
}

// Note: WebSocket tests require a running server, so we keep them minimal
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_invalid_url() {
        let handle = ws_connect(string_new("not a valid url"));
        assert_eq!(handle, 0);
    }

    #[test]
    fn test_connection_refused() {
        // This will fail to connect but shouldn't crash
        let handle = ws_connect(string_new("ws://127.0.0.1:1"));
        assert_eq!(handle, 0);
    }

    #[test]
    fn test_is_connected_invalid_handle() {
        assert_eq!(ws_is_connected(999), 0);
    }

    #[test]
    fn test_send_invalid_handle() {
        assert_eq!(ws_send(999, string_new("test")), 0);
    }

    #[test]
    fn test_async_connect_invalid_url() {
        // Test that async connect with invalid URL returns a promise
        let url = string_new("not a valid url");
        let promise = ws_connect_async(url);
        assert!(!promise.is_null());

        // Poll it - should fail
        unsafe {
            let promise_mut = promise as *mut ZrtlPromise;
            let result = (*promise_mut).poll();
            assert!(result.is_failed());
        }
    }
}
