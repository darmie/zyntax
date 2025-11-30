//! ZRTL HTTP Plugin
//!
//! Provides HTTP client functionality for making web requests.
//!
//! ## Simple Requests
//! - `$Http$get` - GET request, returns body as string
//! - `$Http$post` - POST with body
//! - `$Http$post_json` - POST with JSON body
//!
//! ## Request Builder (for multiple headers)
//! - `$Http$request_new` - Create a request builder
//! - `$Http$request_header` - Add header to request
//! - `$Http$request_body` - Set request body
//! - `$Http$request_send` - Send the request
//!
//! ## Response Handling
//! - `$Http$response_body` - Get response body
//! - `$Http$response_status` - Get response status code
//! - `$Http$response_header` - Get response header
//!
//! ## Async Operations
//! - `$Http$get_async` - Async GET request (returns ZrtlPromise)
//! - `$Http$post_async` - Async POST request (returns ZrtlPromise)
//! - `$Http$request_send_async` - Send request builder asynchronously

use std::collections::HashMap;
use std::sync::Mutex;
use std::sync::atomic::{AtomicI64, Ordering};
use std::thread;
use zrtl::{zrtl_plugin, StringPtr, string_new, string_as_str, ZrtlPromise, PollResult, StateMachineHeader};

// ============================================================================
// Request Builder State
// ============================================================================

struct RequestBuilder {
    method: String,
    url: String,
    headers: Vec<(String, String)>,
    body: Option<String>,
    timeout_ms: Option<u64>,
}

struct ResponseData {
    status: u16,
    body: String,
    headers: HashMap<String, String>,
}

static NEXT_REQUEST_ID: AtomicI64 = AtomicI64::new(1);
static NEXT_RESPONSE_ID: AtomicI64 = AtomicI64::new(1);

lazy_static::lazy_static! {
    static ref REQUESTS: Mutex<HashMap<i64, RequestBuilder>> = Mutex::new(HashMap::new());
    static ref RESPONSES: Mutex<HashMap<i64, ResponseData>> = Mutex::new(HashMap::new());
}

// ============================================================================
// Simple GET
// ============================================================================

/// Simple GET request, returns response body
#[no_mangle]
pub extern "C" fn http_get(url: StringPtr) -> StringPtr {
    let url_str = match unsafe { string_as_str(url) } {
        Some(s) => s,
        None => return string_new(""),
    };

    match ureq::get(url_str).call() {
        Ok(response) => {
            match response.into_string() {
                Ok(body) => string_new(&body),
                Err(_) => string_new(""),
            }
        }
        Err(_) => string_new(""),
    }
}

/// GET request with timeout (milliseconds)
#[no_mangle]
pub extern "C" fn http_get_timeout(url: StringPtr, timeout_ms: u64) -> StringPtr {
    let url_str = match unsafe { string_as_str(url) } {
        Some(s) => s,
        None => return string_new(""),
    };

    let agent = ureq::AgentBuilder::new()
        .timeout(std::time::Duration::from_millis(timeout_ms))
        .build();

    match agent.get(url_str).call() {
        Ok(response) => {
            match response.into_string() {
                Ok(body) => string_new(&body),
                Err(_) => string_new(""),
            }
        }
        Err(_) => string_new(""),
    }
}

// ============================================================================
// POST Requests
// ============================================================================

/// POST request with string body
#[no_mangle]
pub extern "C" fn http_post(url: StringPtr, body: StringPtr) -> StringPtr {
    let url_str = match unsafe { string_as_str(url) } {
        Some(s) => s,
        None => return string_new(""),
    };
    let body_str = unsafe { string_as_str(body) }.unwrap_or("");

    match ureq::post(url_str)
        .set("Content-Type", "text/plain")
        .send_string(body_str)
    {
        Ok(response) => {
            match response.into_string() {
                Ok(body) => string_new(&body),
                Err(_) => string_new(""),
            }
        }
        Err(_) => string_new(""),
    }
}

/// POST request with JSON body (automatically sets Content-Type)
#[no_mangle]
pub extern "C" fn http_post_json(url: StringPtr, json_body: StringPtr) -> StringPtr {
    let url_str = match unsafe { string_as_str(url) } {
        Some(s) => s,
        None => return string_new(""),
    };
    let body_str = unsafe { string_as_str(json_body) }.unwrap_or("{}");

    match ureq::post(url_str)
        .set("Content-Type", "application/json")
        .send_string(body_str)
    {
        Ok(response) => {
            match response.into_string() {
                Ok(body) => string_new(&body),
                Err(_) => string_new(""),
            }
        }
        Err(_) => string_new(""),
    }
}

/// POST form data (URL-encoded)
#[no_mangle]
pub extern "C" fn http_post_form(url: StringPtr, form_data: StringPtr) -> StringPtr {
    let url_str = match unsafe { string_as_str(url) } {
        Some(s) => s,
        None => return string_new(""),
    };
    let form_str = unsafe { string_as_str(form_data) }.unwrap_or("");

    match ureq::post(url_str)
        .set("Content-Type", "application/x-www-form-urlencoded")
        .send_string(form_str)
    {
        Ok(response) => {
            match response.into_string() {
                Ok(body) => string_new(&body),
                Err(_) => string_new(""),
            }
        }
        Err(_) => string_new(""),
    }
}

// ============================================================================
// Other HTTP Methods
// ============================================================================

/// PUT request with body
#[no_mangle]
pub extern "C" fn http_put(url: StringPtr, body: StringPtr) -> StringPtr {
    let url_str = match unsafe { string_as_str(url) } {
        Some(s) => s,
        None => return string_new(""),
    };
    let body_str = unsafe { string_as_str(body) }.unwrap_or("");

    match ureq::put(url_str)
        .set("Content-Type", "application/json")
        .send_string(body_str)
    {
        Ok(response) => {
            match response.into_string() {
                Ok(body) => string_new(&body),
                Err(_) => string_new(""),
            }
        }
        Err(_) => string_new(""),
    }
}

/// PATCH request with body
#[no_mangle]
pub extern "C" fn http_patch(url: StringPtr, body: StringPtr) -> StringPtr {
    let url_str = match unsafe { string_as_str(url) } {
        Some(s) => s,
        None => return string_new(""),
    };
    let body_str = unsafe { string_as_str(body) }.unwrap_or("");

    match ureq::request("PATCH", url_str)
        .set("Content-Type", "application/json")
        .send_string(body_str)
    {
        Ok(response) => {
            match response.into_string() {
                Ok(body) => string_new(&body),
                Err(_) => string_new(""),
            }
        }
        Err(_) => string_new(""),
    }
}

/// DELETE request
#[no_mangle]
pub extern "C" fn http_delete(url: StringPtr) -> StringPtr {
    let url_str = match unsafe { string_as_str(url) } {
        Some(s) => s,
        None => return string_new(""),
    };

    match ureq::delete(url_str).call() {
        Ok(response) => {
            match response.into_string() {
                Ok(body) => string_new(&body),
                Err(_) => string_new(""),
            }
        }
        Err(_) => string_new(""),
    }
}

/// HEAD request, returns status code
#[no_mangle]
pub extern "C" fn http_head(url: StringPtr) -> i32 {
    let url_str = match unsafe { string_as_str(url) } {
        Some(s) => s,
        None => return -1,
    };

    match ureq::head(url_str).call() {
        Ok(response) => response.status() as i32,
        Err(ureq::Error::Status(code, _)) => code as i32,
        Err(_) => -1,
    }
}

// ============================================================================
// Request with Headers
// ============================================================================

/// GET with custom header
#[no_mangle]
pub extern "C" fn http_get_with_header(url: StringPtr, header_name: StringPtr, header_value: StringPtr) -> StringPtr {
    let url_str = match unsafe { string_as_str(url) } {
        Some(s) => s,
        None => return string_new(""),
    };
    let name = unsafe { string_as_str(header_name) }.unwrap_or("");
    let value = unsafe { string_as_str(header_value) }.unwrap_or("");

    match ureq::get(url_str)
        .set(name, value)
        .call()
    {
        Ok(response) => {
            match response.into_string() {
                Ok(body) => string_new(&body),
                Err(_) => string_new(""),
            }
        }
        Err(_) => string_new(""),
    }
}

/// GET with Authorization Bearer token
#[no_mangle]
pub extern "C" fn http_get_with_bearer(url: StringPtr, token: StringPtr) -> StringPtr {
    let url_str = match unsafe { string_as_str(url) } {
        Some(s) => s,
        None => return string_new(""),
    };
    let token_str = unsafe { string_as_str(token) }.unwrap_or("");

    match ureq::get(url_str)
        .set("Authorization", &format!("Bearer {}", token_str))
        .call()
    {
        Ok(response) => {
            match response.into_string() {
                Ok(body) => string_new(&body),
                Err(_) => string_new(""),
            }
        }
        Err(_) => string_new(""),
    }
}

/// POST JSON with Authorization Bearer token
#[no_mangle]
pub extern "C" fn http_post_json_with_bearer(url: StringPtr, json_body: StringPtr, token: StringPtr) -> StringPtr {
    let url_str = match unsafe { string_as_str(url) } {
        Some(s) => s,
        None => return string_new(""),
    };
    let body_str = unsafe { string_as_str(json_body) }.unwrap_or("{}");
    let token_str = unsafe { string_as_str(token) }.unwrap_or("");

    match ureq::post(url_str)
        .set("Content-Type", "application/json")
        .set("Authorization", &format!("Bearer {}", token_str))
        .send_string(body_str)
    {
        Ok(response) => {
            match response.into_string() {
                Ok(body) => string_new(&body),
                Err(_) => string_new(""),
            }
        }
        Err(_) => string_new(""),
    }
}

// ============================================================================
// Response Info
// ============================================================================

/// Get status code from a URL (makes HEAD request)
#[no_mangle]
pub extern "C" fn http_status(url: StringPtr) -> i32 {
    http_head(url)
}

/// Check if URL is reachable (returns 1 if 2xx status)
#[no_mangle]
pub extern "C" fn http_is_ok(url: StringPtr) -> i32 {
    let status = http_head(url);
    (status >= 200 && status < 300) as i32
}

// ============================================================================
// URL Encoding
// ============================================================================

/// URL-encode a string
#[no_mangle]
pub extern "C" fn http_url_encode(s: StringPtr) -> StringPtr {
    let s_str = match unsafe { string_as_str(s) } {
        Some(s) => s,
        None => return string_new(""),
    };

    let mut encoded = String::new();
    for byte in s_str.bytes() {
        match byte {
            b'A'..=b'Z' | b'a'..=b'z' | b'0'..=b'9' | b'-' | b'_' | b'.' | b'~' => {
                encoded.push(byte as char);
            }
            _ => {
                encoded.push_str(&format!("%{:02X}", byte));
            }
        }
    }
    string_new(&encoded)
}

/// URL-decode a string
#[no_mangle]
pub extern "C" fn http_url_decode(s: StringPtr) -> StringPtr {
    let s_str = match unsafe { string_as_str(s) } {
        Some(s) => s,
        None => return string_new(""),
    };

    let mut decoded = Vec::new();
    let mut bytes = s_str.bytes().peekable();

    while let Some(byte) = bytes.next() {
        if byte == b'%' {
            let mut hex = String::new();
            if let Some(&h1) = bytes.peek() {
                hex.push(h1 as char);
                bytes.next();
            }
            if let Some(&h2) = bytes.peek() {
                hex.push(h2 as char);
                bytes.next();
            }
            if let Ok(val) = u8::from_str_radix(&hex, 16) {
                decoded.push(val);
            }
        } else if byte == b'+' {
            decoded.push(b' ');
        } else {
            decoded.push(byte);
        }
    }

    match String::from_utf8(decoded) {
        Ok(s) => string_new(&s),
        Err(_) => string_new(""),
    }
}

// ============================================================================
// Request Builder API
// ============================================================================

/// Create a new request builder with method and URL
/// Returns request handle, or 0 on error
#[no_mangle]
pub extern "C" fn http_request_new(method: StringPtr, url: StringPtr) -> i64 {
    let method_str = match unsafe { string_as_str(method) } {
        Some(s) => s.to_uppercase(),
        None => return 0,
    };
    let url_str = match unsafe { string_as_str(url) } {
        Some(s) => s.to_string(),
        None => return 0,
    };

    let id = NEXT_REQUEST_ID.fetch_add(1, Ordering::SeqCst);
    let builder = RequestBuilder {
        method: method_str,
        url: url_str,
        headers: Vec::new(),
        body: None,
        timeout_ms: None,
    };

    if let Ok(mut requests) = REQUESTS.lock() {
        requests.insert(id, builder);
        id
    } else {
        0
    }
}

/// Add a header to the request builder
/// Returns 1 on success, 0 on error
#[no_mangle]
pub extern "C" fn http_request_header(handle: i64, name: StringPtr, value: StringPtr) -> i32 {
    let name_str = match unsafe { string_as_str(name) } {
        Some(s) => s.to_string(),
        None => return 0,
    };
    let value_str = match unsafe { string_as_str(value) } {
        Some(s) => s.to_string(),
        None => return 0,
    };

    if let Ok(mut requests) = REQUESTS.lock() {
        if let Some(builder) = requests.get_mut(&handle) {
            builder.headers.push((name_str, value_str));
            return 1;
        }
    }
    0
}

/// Set the request body
/// Returns 1 on success, 0 on error
#[no_mangle]
pub extern "C" fn http_request_body(handle: i64, body: StringPtr) -> i32 {
    let body_str = match unsafe { string_as_str(body) } {
        Some(s) => s.to_string(),
        None => return 0,
    };

    if let Ok(mut requests) = REQUESTS.lock() {
        if let Some(builder) = requests.get_mut(&handle) {
            builder.body = Some(body_str);
            return 1;
        }
    }
    0
}

/// Set request timeout in milliseconds
/// Returns 1 on success, 0 on error
#[no_mangle]
pub extern "C" fn http_request_timeout(handle: i64, timeout_ms: u64) -> i32 {
    if let Ok(mut requests) = REQUESTS.lock() {
        if let Some(builder) = requests.get_mut(&handle) {
            builder.timeout_ms = Some(timeout_ms);
            return 1;
        }
    }
    0
}

/// Send the request and return a response handle
/// Returns response handle, or 0 on error
#[no_mangle]
pub extern "C" fn http_request_send(handle: i64) -> i64 {
    let builder = {
        let mut requests = match REQUESTS.lock() {
            Ok(r) => r,
            Err(_) => return 0,
        };
        match requests.remove(&handle) {
            Some(b) => b,
            None => return 0,
        }
    };

    // Build the agent with optional timeout
    let agent = if let Some(timeout) = builder.timeout_ms {
        ureq::AgentBuilder::new()
            .timeout(std::time::Duration::from_millis(timeout))
            .build()
    } else {
        ureq::Agent::new()
    };

    // Build the request
    let mut request = agent.request(&builder.method, &builder.url);

    // Add all headers
    for (name, value) in &builder.headers {
        request = request.set(name, value);
    }

    // Send with or without body
    let result = if let Some(body) = &builder.body {
        request.send_string(body)
    } else {
        request.call()
    };

    match result {
        Ok(response) => {
            let status = response.status();

            // Collect headers
            let mut headers = HashMap::new();
            for name in response.headers_names() {
                if let Some(value) = response.header(&name) {
                    headers.insert(name.to_lowercase(), value.to_string());
                }
            }

            // Get body
            let body = response.into_string().unwrap_or_default();

            let response_id = NEXT_RESPONSE_ID.fetch_add(1, Ordering::SeqCst);
            let response_data = ResponseData {
                status,
                body,
                headers,
            };

            if let Ok(mut responses) = RESPONSES.lock() {
                responses.insert(response_id, response_data);
                response_id
            } else {
                0
            }
        }
        Err(ureq::Error::Status(code, response)) => {
            // Still create response for error status codes
            let mut headers = HashMap::new();
            for name in response.headers_names() {
                if let Some(value) = response.header(&name) {
                    headers.insert(name.to_lowercase(), value.to_string());
                }
            }
            let body = response.into_string().unwrap_or_default();

            let response_id = NEXT_RESPONSE_ID.fetch_add(1, Ordering::SeqCst);
            let response_data = ResponseData {
                status: code,
                body,
                headers,
            };

            if let Ok(mut responses) = RESPONSES.lock() {
                responses.insert(response_id, response_data);
                response_id
            } else {
                0
            }
        }
        Err(_) => 0,
    }
}

/// Get the response body
#[no_mangle]
pub extern "C" fn http_response_body(handle: i64) -> StringPtr {
    if let Ok(responses) = RESPONSES.lock() {
        if let Some(response) = responses.get(&handle) {
            return string_new(&response.body);
        }
    }
    string_new("")
}

/// Get the response status code
#[no_mangle]
pub extern "C" fn http_response_status(handle: i64) -> i32 {
    if let Ok(responses) = RESPONSES.lock() {
        if let Some(response) = responses.get(&handle) {
            return response.status as i32;
        }
    }
    0
}

/// Get a response header value
#[no_mangle]
pub extern "C" fn http_response_header(handle: i64, name: StringPtr) -> StringPtr {
    let name_str = match unsafe { string_as_str(name) } {
        Some(s) => s.to_lowercase(),
        None => return string_new(""),
    };

    if let Ok(responses) = RESPONSES.lock() {
        if let Some(response) = responses.get(&handle) {
            if let Some(value) = response.headers.get(&name_str) {
                return string_new(value);
            }
        }
    }
    string_new("")
}

/// Free a response handle
#[no_mangle]
pub extern "C" fn http_response_free(handle: i64) {
    if let Ok(mut responses) = RESPONSES.lock() {
        responses.remove(&handle);
    }
}

/// Free a request handle (if not sent)
#[no_mangle]
pub extern "C" fn http_request_free(handle: i64) {
    if let Ok(mut requests) = REQUESTS.lock() {
        requests.remove(&handle);
    }
}

// ============================================================================
// Plugin Export
// ============================================================================

zrtl_plugin! {
    name: "zrtl_http",
    symbols: [
        // Simple requests
        ("$Http$get", http_get),
        ("$Http$get_timeout", http_get_timeout),
        ("$Http$post", http_post),
        ("$Http$post_json", http_post_json),
        ("$Http$post_form", http_post_form),

        // Other methods
        ("$Http$put", http_put),
        ("$Http$patch", http_patch),
        ("$Http$delete", http_delete),
        ("$Http$head", http_head),

        // With headers
        ("$Http$get_with_header", http_get_with_header),
        ("$Http$get_with_bearer", http_get_with_bearer),
        ("$Http$post_json_with_bearer", http_post_json_with_bearer),

        // Response info
        ("$Http$status", http_status),
        ("$Http$is_ok", http_is_ok),

        // URL encoding
        ("$Http$url_encode", http_url_encode),
        ("$Http$url_decode", http_url_decode),

        // Request builder API
        ("$Http$request_new", http_request_new),
        ("$Http$request_header", http_request_header),
        ("$Http$request_body", http_request_body),
        ("$Http$request_timeout", http_request_timeout),
        ("$Http$request_send", http_request_send),
        ("$Http$request_free", http_request_free),

        // Response API
        ("$Http$response_body", http_response_body),
        ("$Http$response_status", http_response_status),
        ("$Http$response_header", http_response_header),
        ("$Http$response_free", http_response_free),

        // Async API
        ("$Http$get_async", http_get_async),
        ("$Http$post_async", http_post_async),
        ("$Http$post_json_async", http_post_json_async),
        ("$Http$request_send_async", http_request_send_async),
        ("$Http$promise_free", http_promise_free),
    ]
}

// ============================================================================
// Async Operations
// ============================================================================

/// Internal: Perform HTTP request in a spawn-style manner
/// For true async, we use a thread + atomic flag pattern
struct HttpFuture {
    state: std::sync::Arc<std::sync::Mutex<HttpFutureState>>,
}

struct HttpFutureState {
    completed: bool,
    result: Option<Result<i64, i32>>,
}

impl HttpFuture {
    fn spawn_get(url: String) -> Self {
        let state = std::sync::Arc::new(std::sync::Mutex::new(HttpFutureState {
            completed: false,
            result: None,
        }));

        let state_clone = state.clone();
        thread::spawn(move || {
            let result = match ureq::get(&url).call() {
                Ok(response) => {
                    let status = response.status();
                    let mut headers = HashMap::new();
                    for name in response.headers_names() {
                        if let Some(value) = response.header(&name) {
                            headers.insert(name.to_lowercase(), value.to_string());
                        }
                    }
                    let body = response.into_string().unwrap_or_default();

                    let response_id = NEXT_RESPONSE_ID.fetch_add(1, Ordering::SeqCst);
                    let response_data = ResponseData { status, body, headers };

                    if let Ok(mut responses) = RESPONSES.lock() {
                        responses.insert(response_id, response_data);
                        Ok(response_id)
                    } else {
                        Err(-2)
                    }
                }
                Err(_) => Err(-1),
            };

            if let Ok(mut state) = state_clone.lock() {
                state.completed = true;
                state.result = Some(result);
            }
        });

        HttpFuture { state }
    }

    fn spawn_post(url: String, body: String, content_type: String) -> Self {
        let state = std::sync::Arc::new(std::sync::Mutex::new(HttpFutureState {
            completed: false,
            result: None,
        }));

        let state_clone = state.clone();
        thread::spawn(move || {
            let result = match ureq::post(&url)
                .set("Content-Type", &content_type)
                .send_string(&body)
            {
                Ok(response) => {
                    let status = response.status();
                    let mut headers = HashMap::new();
                    for name in response.headers_names() {
                        if let Some(value) = response.header(&name) {
                            headers.insert(name.to_lowercase(), value.to_string());
                        }
                    }
                    let body = response.into_string().unwrap_or_default();

                    let response_id = NEXT_RESPONSE_ID.fetch_add(1, Ordering::SeqCst);
                    let response_data = ResponseData { status, body, headers };

                    if let Ok(mut responses) = RESPONSES.lock() {
                        responses.insert(response_id, response_data);
                        Ok(response_id)
                    } else {
                        Err(-2)
                    }
                }
                Err(_) => Err(-1),
            };

            if let Ok(mut state) = state_clone.lock() {
                state.completed = true;
                state.result = Some(result);
            }
        });

        HttpFuture { state }
    }

    fn poll(&self) -> PollResult {
        if let Ok(state) = self.state.lock() {
            if state.completed {
                match state.result {
                    Some(Ok(v)) => PollResult::Ready(v),
                    Some(Err(e)) => PollResult::Failed(e),
                    None => PollResult::Failed(-3),
                }
            } else {
                PollResult::Pending
            }
        } else {
            PollResult::Failed(-4)
        }
    }
}

// Storage for active HTTP futures
lazy_static::lazy_static! {
    static ref HTTP_FUTURES: Mutex<HashMap<i64, HttpFuture>> = Mutex::new(HashMap::new());
}

static NEXT_FUTURE_ID: AtomicI64 = AtomicI64::new(1);

/// State for async promise
#[repr(C)]
struct HttpPromiseState {
    header: StateMachineHeader,
    future_id: i64,
}

/// Poll function for async HTTP
unsafe extern "C" fn http_async_poll(state: *mut u8) -> i64 {
    let sm = &mut *(state as *mut HttpPromiseState);

    if sm.header.async_state().is_finished() {
        return PollResult::Failed(-1).to_abi();
    }

    if let Ok(futures) = HTTP_FUTURES.lock() {
        if let Some(future) = futures.get(&sm.future_id) {
            let result = future.poll();
            match result {
                PollResult::Ready(v) => {
                    sm.header.set_completed();
                    return PollResult::Ready(v).to_abi();
                }
                PollResult::Failed(e) => {
                    sm.header.set_failed();
                    return PollResult::Failed(e).to_abi();
                }
                PollResult::Pending => {
                    return PollResult::Pending.to_abi();
                }
            }
        }
    }

    sm.header.set_failed();
    PollResult::Failed(-5).to_abi()
}

/// Async GET request
/// Returns a ZrtlPromise pointer. Poll until ready, result is response handle.
#[no_mangle]
pub extern "C" fn http_get_async(url: StringPtr) -> *const ZrtlPromise {
    let url_str = match unsafe { string_as_str(url) } {
        Some(s) => s.to_string(),
        None => {
            // Return a failed promise
            let state = Box::new(HttpPromiseState {
                header: StateMachineHeader::new(),
                future_id: 0,
            });
            let state_ptr = Box::into_raw(state) as *mut u8;
            unsafe { (*(state_ptr as *mut HttpPromiseState)).header.set_failed(); }
            let promise = Box::new(unsafe { ZrtlPromise::new(state_ptr, http_async_poll) });
            return Box::into_raw(promise);
        }
    };

    // Spawn the HTTP request
    let future = HttpFuture::spawn_get(url_str);
    let future_id = NEXT_FUTURE_ID.fetch_add(1, Ordering::SeqCst);

    if let Ok(mut futures) = HTTP_FUTURES.lock() {
        futures.insert(future_id, future);
    }

    let state = Box::new(HttpPromiseState {
        header: StateMachineHeader::new(),
        future_id,
    });
    let state_ptr = Box::into_raw(state) as *mut u8;

    let promise = Box::new(unsafe { ZrtlPromise::new(state_ptr, http_async_poll) });
    Box::into_raw(promise)
}

/// Async POST request with body
/// Returns a ZrtlPromise pointer. Poll until ready, result is response handle.
#[no_mangle]
pub extern "C" fn http_post_async(url: StringPtr, body: StringPtr) -> *const ZrtlPromise {
    let url_str = match unsafe { string_as_str(url) } {
        Some(s) => s.to_string(),
        None => {
            let state = Box::new(HttpPromiseState {
                header: StateMachineHeader::new(),
                future_id: 0,
            });
            let state_ptr = Box::into_raw(state) as *mut u8;
            unsafe { (*(state_ptr as *mut HttpPromiseState)).header.set_failed(); }
            let promise = Box::new(unsafe { ZrtlPromise::new(state_ptr, http_async_poll) });
            return Box::into_raw(promise);
        }
    };
    let body_str = unsafe { string_as_str(body) }.unwrap_or("").to_string();

    let future = HttpFuture::spawn_post(url_str, body_str, "text/plain".to_string());
    let future_id = NEXT_FUTURE_ID.fetch_add(1, Ordering::SeqCst);

    if let Ok(mut futures) = HTTP_FUTURES.lock() {
        futures.insert(future_id, future);
    }

    let state = Box::new(HttpPromiseState {
        header: StateMachineHeader::new(),
        future_id,
    });
    let state_ptr = Box::into_raw(state) as *mut u8;

    let promise = Box::new(unsafe { ZrtlPromise::new(state_ptr, http_async_poll) });
    Box::into_raw(promise)
}

/// Async POST JSON request
/// Returns a ZrtlPromise pointer.
#[no_mangle]
pub extern "C" fn http_post_json_async(url: StringPtr, json_body: StringPtr) -> *const ZrtlPromise {
    let url_str = match unsafe { string_as_str(url) } {
        Some(s) => s.to_string(),
        None => {
            let state = Box::new(HttpPromiseState {
                header: StateMachineHeader::new(),
                future_id: 0,
            });
            let state_ptr = Box::into_raw(state) as *mut u8;
            unsafe { (*(state_ptr as *mut HttpPromiseState)).header.set_failed(); }
            let promise = Box::new(unsafe { ZrtlPromise::new(state_ptr, http_async_poll) });
            return Box::into_raw(promise);
        }
    };
    let body_str = unsafe { string_as_str(json_body) }.unwrap_or("{}").to_string();

    let future = HttpFuture::spawn_post(url_str, body_str, "application/json".to_string());
    let future_id = NEXT_FUTURE_ID.fetch_add(1, Ordering::SeqCst);

    if let Ok(mut futures) = HTTP_FUTURES.lock() {
        futures.insert(future_id, future);
    }

    let state = Box::new(HttpPromiseState {
        header: StateMachineHeader::new(),
        future_id,
    });
    let state_ptr = Box::into_raw(state) as *mut u8;

    let promise = Box::new(unsafe { ZrtlPromise::new(state_ptr, http_async_poll) });
    Box::into_raw(promise)
}

/// Send a request builder asynchronously
/// Returns a ZrtlPromise pointer. Poll until ready, result is response handle.
#[no_mangle]
pub extern "C" fn http_request_send_async(handle: i64) -> *const ZrtlPromise {
    let builder = {
        let mut requests = match REQUESTS.lock() {
            Ok(r) => r,
            Err(_) => {
                let state = Box::new(HttpPromiseState {
                    header: StateMachineHeader::new(),
                    future_id: 0,
                });
                let state_ptr = Box::into_raw(state) as *mut u8;
                unsafe { (*(state_ptr as *mut HttpPromiseState)).header.set_failed(); }
                let promise = Box::new(unsafe { ZrtlPromise::new(state_ptr, http_async_poll) });
                return Box::into_raw(promise);
            }
        };
        match requests.remove(&handle) {
            Some(b) => b,
            None => {
                let state = Box::new(HttpPromiseState {
                    header: StateMachineHeader::new(),
                    future_id: 0,
                });
                let state_ptr = Box::into_raw(state) as *mut u8;
                unsafe { (*(state_ptr as *mut HttpPromiseState)).header.set_failed(); }
                let promise = Box::new(unsafe { ZrtlPromise::new(state_ptr, http_async_poll) });
                return Box::into_raw(promise);
            }
        }
    };

    // Spawn in thread
    let state = std::sync::Arc::new(std::sync::Mutex::new(HttpFutureState {
        completed: false,
        result: None,
    }));

    let state_clone = state.clone();
    thread::spawn(move || {
        let agent = if let Some(timeout) = builder.timeout_ms {
            ureq::AgentBuilder::new()
                .timeout(std::time::Duration::from_millis(timeout))
                .build()
        } else {
            ureq::Agent::new()
        };

        let mut request = agent.request(&builder.method, &builder.url);
        for (name, value) in &builder.headers {
            request = request.set(name, value);
        }

        let result = if let Some(body) = &builder.body {
            request.send_string(body)
        } else {
            request.call()
        };

        let http_result = match result {
            Ok(response) => {
                let status = response.status();
                let mut headers = HashMap::new();
                for name in response.headers_names() {
                    if let Some(value) = response.header(&name) {
                        headers.insert(name.to_lowercase(), value.to_string());
                    }
                }
                let body = response.into_string().unwrap_or_default();

                let response_id = NEXT_RESPONSE_ID.fetch_add(1, Ordering::SeqCst);
                let response_data = ResponseData { status, body, headers };

                if let Ok(mut responses) = RESPONSES.lock() {
                    responses.insert(response_id, response_data);
                    Ok(response_id)
                } else {
                    Err(-2)
                }
            }
            Err(ureq::Error::Status(code, response)) => {
                let mut headers = HashMap::new();
                for name in response.headers_names() {
                    if let Some(value) = response.header(&name) {
                        headers.insert(name.to_lowercase(), value.to_string());
                    }
                }
                let body = response.into_string().unwrap_or_default();

                let response_id = NEXT_RESPONSE_ID.fetch_add(1, Ordering::SeqCst);
                let response_data = ResponseData { status: code, body, headers };

                if let Ok(mut responses) = RESPONSES.lock() {
                    responses.insert(response_id, response_data);
                    Ok(response_id)
                } else {
                    Err(-2)
                }
            }
            Err(_) => Err(-1),
        };

        if let Ok(mut state) = state_clone.lock() {
            state.completed = true;
            state.result = Some(http_result);
        }
    });

    let future = HttpFuture { state };
    let future_id = NEXT_FUTURE_ID.fetch_add(1, Ordering::SeqCst);

    if let Ok(mut futures) = HTTP_FUTURES.lock() {
        futures.insert(future_id, future);
    }

    let promise_state = Box::new(HttpPromiseState {
        header: StateMachineHeader::new(),
        future_id,
    });
    let state_ptr = Box::into_raw(promise_state) as *mut u8;

    let promise = Box::new(unsafe { ZrtlPromise::new(state_ptr, http_async_poll) });
    Box::into_raw(promise)
}

/// Free an HTTP promise after use
#[no_mangle]
pub extern "C" fn http_promise_free(promise: *mut ZrtlPromise) {
    if promise.is_null() {
        return;
    }

    unsafe {
        let promise_box = Box::from_raw(promise);
        let state = promise_box.state_machine as *mut HttpPromiseState;
        if !state.is_null() {
            let state_box = Box::from_raw(state);
            // Clean up the future
            if let Ok(mut futures) = HTTP_FUTURES.lock() {
                futures.remove(&state_box.future_id);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_url_encode() {
        let input = string_new("hello world");
        let encoded = http_url_encode(input);
        let encoded_str = unsafe { string_as_str(encoded) }.unwrap();
        assert_eq!(encoded_str, "hello%20world");
    }

    #[test]
    fn test_url_decode() {
        let input = string_new("hello%20world");
        let decoded = http_url_decode(input);
        let decoded_str = unsafe { string_as_str(decoded) }.unwrap();
        assert_eq!(decoded_str, "hello world");
    }

    #[test]
    fn test_url_encode_special() {
        let input = string_new("a=1&b=2");
        let encoded = http_url_encode(input);
        let encoded_str = unsafe { string_as_str(encoded) }.unwrap();
        assert_eq!(encoded_str, "a%3D1%26b%3D2");
    }

    #[test]
    fn test_async_get_invalid_url() {
        // Test that async get with null URL returns a failed promise
        let promise = http_get_async(std::ptr::null_mut());
        assert!(!promise.is_null());

        unsafe {
            let promise_mut = promise as *mut ZrtlPromise;
            let result = (*promise_mut).poll();
            assert!(result.is_failed());
        }
    }

    // Note: Network tests would require mocking or a real server
    // These are commented out to avoid test failures in CI
    // #[test]
    // fn test_http_get() {
    //     let url = string_new("https://httpbin.org/get");
    //     let response = http_get(url);
    //     let response_str = unsafe { string_as_str(response) }.unwrap();
    //     assert!(response_str.contains("httpbin"));
    // }
}
