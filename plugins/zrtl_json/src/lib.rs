//! ZRTL JSON Plugin
//!
//! Provides JSON parsing and serialization for Zyntax-based languages.
//!
//! ## JSON Value Representation
//!
//! JSON values are represented as opaque handles (u64). Use the accessor
//! functions to work with them:
//!
//! - `$Json$parse` - Parse JSON string to handle
//! - `$Json$stringify` - Convert handle to JSON string
//! - `$Json$get_type` - Get value type (null=0, bool=1, number=2, string=3, array=4, object=5)
//! - `$Json$get_*` - Get typed values
//! - `$Json$set_*` - Set values in objects/arrays
//! - `$Json$free` - Free JSON handle

use std::collections::HashMap;
use std::sync::Mutex;
use zrtl::{zrtl_plugin, StringPtr, ArrayPtr, string_new, string_as_str, array_new, array_push};
use serde_json::{Value, Map, Number};

// ============================================================================
// Handle Management
// ============================================================================

static JSON_HANDLES: Mutex<Option<JsonHandleStore>> = Mutex::new(None);

struct JsonHandleStore {
    values: HashMap<u64, Value>,
    next_id: u64,
}

impl JsonHandleStore {
    fn new() -> Self {
        Self {
            values: HashMap::new(),
            next_id: 1,
        }
    }

    fn insert(&mut self, value: Value) -> u64 {
        let id = self.next_id;
        self.next_id += 1;
        self.values.insert(id, value);
        id
    }

    fn get(&self, id: u64) -> Option<&Value> {
        self.values.get(&id)
    }

    fn get_mut(&mut self, id: u64) -> Option<&mut Value> {
        self.values.get_mut(&id)
    }

    fn remove(&mut self, id: u64) -> Option<Value> {
        self.values.remove(&id)
    }
}

fn with_store<F, R>(f: F) -> R
where
    F: FnOnce(&mut JsonHandleStore) -> R,
{
    let mut guard = JSON_HANDLES.lock().unwrap();
    if guard.is_none() {
        *guard = Some(JsonHandleStore::new());
    }
    f(guard.as_mut().unwrap())
}

// ============================================================================
// Type Constants
// ============================================================================

const JSON_TYPE_NULL: i32 = 0;
const JSON_TYPE_BOOL: i32 = 1;
const JSON_TYPE_NUMBER: i32 = 2;
const JSON_TYPE_STRING: i32 = 3;
const JSON_TYPE_ARRAY: i32 = 4;
const JSON_TYPE_OBJECT: i32 = 5;

// ============================================================================
// Parsing & Stringifying
// ============================================================================

/// Parse JSON string, returns handle (0 on error)
#[no_mangle]
pub extern "C" fn json_parse(s: StringPtr) -> u64 {
    let s_str = match unsafe { string_as_str(s) } {
        Some(s) => s,
        None => return 0,
    };

    match serde_json::from_str::<Value>(s_str) {
        Ok(value) => with_store(|store| store.insert(value)),
        Err(_) => 0,
    }
}

/// Convert JSON value to string
#[no_mangle]
pub extern "C" fn json_stringify(handle: u64) -> StringPtr {
    with_store(|store| {
        match store.get(handle) {
            Some(value) => string_new(&value.to_string()),
            None => string_new("null"),
        }
    })
}

/// Convert JSON value to pretty-printed string
#[no_mangle]
pub extern "C" fn json_stringify_pretty(handle: u64) -> StringPtr {
    with_store(|store| {
        match store.get(handle) {
            Some(value) => {
                match serde_json::to_string_pretty(value) {
                    Ok(s) => string_new(&s),
                    Err(_) => string_new("null"),
                }
            }
            None => string_new("null"),
        }
    })
}

/// Free a JSON handle
#[no_mangle]
pub extern "C" fn json_free(handle: u64) {
    with_store(|store| {
        store.remove(handle);
    });
}

// ============================================================================
// Type Checking
// ============================================================================

/// Get JSON value type
#[no_mangle]
pub extern "C" fn json_get_type(handle: u64) -> i32 {
    with_store(|store| {
        match store.get(handle) {
            Some(Value::Null) => JSON_TYPE_NULL,
            Some(Value::Bool(_)) => JSON_TYPE_BOOL,
            Some(Value::Number(_)) => JSON_TYPE_NUMBER,
            Some(Value::String(_)) => JSON_TYPE_STRING,
            Some(Value::Array(_)) => JSON_TYPE_ARRAY,
            Some(Value::Object(_)) => JSON_TYPE_OBJECT,
            None => JSON_TYPE_NULL,
        }
    })
}

/// Check if value is null
#[no_mangle]
pub extern "C" fn json_is_null(handle: u64) -> i32 {
    (json_get_type(handle) == JSON_TYPE_NULL) as i32
}

/// Check if value is boolean
#[no_mangle]
pub extern "C" fn json_is_bool(handle: u64) -> i32 {
    (json_get_type(handle) == JSON_TYPE_BOOL) as i32
}

/// Check if value is number
#[no_mangle]
pub extern "C" fn json_is_number(handle: u64) -> i32 {
    (json_get_type(handle) == JSON_TYPE_NUMBER) as i32
}

/// Check if value is string
#[no_mangle]
pub extern "C" fn json_is_string(handle: u64) -> i32 {
    (json_get_type(handle) == JSON_TYPE_STRING) as i32
}

/// Check if value is array
#[no_mangle]
pub extern "C" fn json_is_array(handle: u64) -> i32 {
    (json_get_type(handle) == JSON_TYPE_ARRAY) as i32
}

/// Check if value is object
#[no_mangle]
pub extern "C" fn json_is_object(handle: u64) -> i32 {
    (json_get_type(handle) == JSON_TYPE_OBJECT) as i32
}

// ============================================================================
// Value Getters
// ============================================================================

/// Get boolean value (returns 0 if not a bool)
#[no_mangle]
pub extern "C" fn json_get_bool(handle: u64) -> i32 {
    with_store(|store| {
        match store.get(handle) {
            Some(Value::Bool(b)) => *b as i32,
            _ => 0,
        }
    })
}

/// Get integer value (returns 0 if not a number)
#[no_mangle]
pub extern "C" fn json_get_int(handle: u64) -> i64 {
    with_store(|store| {
        match store.get(handle) {
            Some(Value::Number(n)) => n.as_i64().unwrap_or(0),
            _ => 0,
        }
    })
}

/// Get float value (returns 0.0 if not a number)
#[no_mangle]
pub extern "C" fn json_get_float(handle: u64) -> f64 {
    with_store(|store| {
        match store.get(handle) {
            Some(Value::Number(n)) => n.as_f64().unwrap_or(0.0),
            _ => 0.0,
        }
    })
}

/// Get string value (returns empty string if not a string)
#[no_mangle]
pub extern "C" fn json_get_string(handle: u64) -> StringPtr {
    with_store(|store| {
        match store.get(handle) {
            Some(Value::String(s)) => string_new(s),
            _ => string_new(""),
        }
    })
}

// ============================================================================
// Object Access
// ============================================================================

/// Get value from object by key (returns new handle, 0 if not found)
#[no_mangle]
pub extern "C" fn json_get(handle: u64, key: StringPtr) -> u64 {
    let key_str = match unsafe { string_as_str(key) } {
        Some(s) => s,
        None => return 0,
    };

    with_store(|store| {
        match store.get(handle) {
            Some(Value::Object(obj)) => {
                match obj.get(key_str) {
                    Some(value) => store.insert(value.clone()),
                    None => 0,
                }
            }
            _ => 0,
        }
    })
}

/// Check if object has key
#[no_mangle]
pub extern "C" fn json_has(handle: u64, key: StringPtr) -> i32 {
    let key_str = match unsafe { string_as_str(key) } {
        Some(s) => s,
        None => return 0,
    };

    with_store(|store| {
        match store.get(handle) {
            Some(Value::Object(obj)) => obj.contains_key(key_str) as i32,
            _ => 0,
        }
    })
}

/// Get all keys from object
#[no_mangle]
pub extern "C" fn json_keys(handle: u64) -> ArrayPtr {
    with_store(|store| {
        match store.get(handle) {
            Some(Value::Object(obj)) => {
                let arr = array_new::<StringPtr>(obj.len());
                for key in obj.keys() {
                    let key_ptr = string_new(key);
                    unsafe { array_push(arr, key_ptr); }
                }
                arr
            }
            _ => array_new::<StringPtr>(0),
        }
    })
}

/// Set value in object
#[no_mangle]
pub extern "C" fn json_set(handle: u64, key: StringPtr, value_handle: u64) {
    let key_str = match unsafe { string_as_str(key) } {
        Some(s) => s.to_string(),
        None => return,
    };

    with_store(|store| {
        // Get the value to set (clone it)
        let value = match store.get(value_handle) {
            Some(v) => v.clone(),
            None => Value::Null,
        };

        // Set it in the object
        if let Some(Value::Object(obj)) = store.get_mut(handle) {
            obj.insert(key_str, value);
        }
    });
}

/// Remove key from object
#[no_mangle]
pub extern "C" fn json_remove(handle: u64, key: StringPtr) {
    let key_str = match unsafe { string_as_str(key) } {
        Some(s) => s,
        None => return,
    };

    with_store(|store| {
        if let Some(Value::Object(obj)) = store.get_mut(handle) {
            obj.remove(key_str);
        }
    });
}

// ============================================================================
// Array Access
// ============================================================================

/// Get array length
#[no_mangle]
pub extern "C" fn json_array_length(handle: u64) -> i64 {
    with_store(|store| {
        match store.get(handle) {
            Some(Value::Array(arr)) => arr.len() as i64,
            _ => 0,
        }
    })
}

/// Get value from array by index (returns new handle, 0 if out of bounds)
#[no_mangle]
pub extern "C" fn json_array_get(handle: u64, index: i64) -> u64 {
    with_store(|store| {
        match store.get(handle) {
            Some(Value::Array(arr)) => {
                match arr.get(index as usize) {
                    Some(value) => store.insert(value.clone()),
                    None => 0,
                }
            }
            _ => 0,
        }
    })
}

/// Set value in array at index
#[no_mangle]
pub extern "C" fn json_array_set(handle: u64, index: i64, value_handle: u64) {
    with_store(|store| {
        let value = match store.get(value_handle) {
            Some(v) => v.clone(),
            None => Value::Null,
        };

        if let Some(Value::Array(arr)) = store.get_mut(handle) {
            let idx = index as usize;
            if idx < arr.len() {
                arr[idx] = value;
            }
        }
    });
}

/// Push value to array
#[no_mangle]
pub extern "C" fn json_array_push(handle: u64, value_handle: u64) {
    with_store(|store| {
        let value = match store.get(value_handle) {
            Some(v) => v.clone(),
            None => Value::Null,
        };

        if let Some(Value::Array(arr)) = store.get_mut(handle) {
            arr.push(value);
        }
    });
}

/// Pop value from array (returns new handle)
#[no_mangle]
pub extern "C" fn json_array_pop(handle: u64) -> u64 {
    with_store(|store| {
        if let Some(Value::Array(arr)) = store.get_mut(handle) {
            match arr.pop() {
                Some(value) => store.insert(value),
                None => 0,
            }
        } else {
            0
        }
    })
}

// ============================================================================
// Value Constructors
// ============================================================================

/// Create null value
#[no_mangle]
pub extern "C" fn json_null() -> u64 {
    with_store(|store| store.insert(Value::Null))
}

/// Create boolean value
#[no_mangle]
pub extern "C" fn json_bool(value: i32) -> u64 {
    with_store(|store| store.insert(Value::Bool(value != 0)))
}

/// Create integer value
#[no_mangle]
pub extern "C" fn json_int(value: i64) -> u64 {
    with_store(|store| store.insert(Value::Number(Number::from(value))))
}

/// Create float value
#[no_mangle]
pub extern "C" fn json_float(value: f64) -> u64 {
    with_store(|store| {
        match Number::from_f64(value) {
            Some(n) => store.insert(Value::Number(n)),
            None => store.insert(Value::Null), // NaN or Infinity
        }
    })
}

/// Create string value
#[no_mangle]
pub extern "C" fn json_string(s: StringPtr) -> u64 {
    let s_str = unsafe { string_as_str(s) }.unwrap_or("");
    with_store(|store| store.insert(Value::String(s_str.to_string())))
}

/// Create empty array
#[no_mangle]
pub extern "C" fn json_array() -> u64 {
    with_store(|store| store.insert(Value::Array(Vec::new())))
}

/// Create empty object
#[no_mangle]
pub extern "C" fn json_object() -> u64 {
    with_store(|store| store.insert(Value::Object(Map::new())))
}

// ============================================================================
// Path Access (JSONPath-like)
// ============================================================================

/// Get value by dot-separated path (e.g., "foo.bar.0.baz")
#[no_mangle]
pub extern "C" fn json_path_get(handle: u64, path: StringPtr) -> u64 {
    let path_str = match unsafe { string_as_str(path) } {
        Some(s) => s,
        None => return 0,
    };

    with_store(|store| {
        let mut current = match store.get(handle) {
            Some(v) => v.clone(),
            None => return 0,
        };

        for part in path_str.split('.') {
            if part.is_empty() {
                continue;
            }

            current = match &current {
                Value::Object(obj) => {
                    match obj.get(part) {
                        Some(v) => v.clone(),
                        None => return 0,
                    }
                }
                Value::Array(arr) => {
                    match part.parse::<usize>() {
                        Ok(idx) => {
                            match arr.get(idx) {
                                Some(v) => v.clone(),
                                None => return 0,
                            }
                        }
                        Err(_) => return 0,
                    }
                }
                _ => return 0,
            };
        }

        store.insert(current)
    })
}

// ============================================================================
// Plugin Export
// ============================================================================

zrtl_plugin! {
    name: "zrtl_json",
    symbols: [
        // Parse/Stringify
        ("$Json$parse", json_parse),
        ("$Json$stringify", json_stringify),
        ("$Json$stringify_pretty", json_stringify_pretty),
        ("$Json$free", json_free),

        // Type checking
        ("$Json$get_type", json_get_type),
        ("$Json$is_null", json_is_null),
        ("$Json$is_bool", json_is_bool),
        ("$Json$is_number", json_is_number),
        ("$Json$is_string", json_is_string),
        ("$Json$is_array", json_is_array),
        ("$Json$is_object", json_is_object),

        // Value getters
        ("$Json$get_bool", json_get_bool),
        ("$Json$get_int", json_get_int),
        ("$Json$get_float", json_get_float),
        ("$Json$get_string", json_get_string),

        // Object access
        ("$Json$get", json_get),
        ("$Json$has", json_has),
        ("$Json$keys", json_keys),
        ("$Json$set", json_set),
        ("$Json$remove", json_remove),

        // Array access
        ("$Json$array_length", json_array_length),
        ("$Json$array_get", json_array_get),
        ("$Json$array_set", json_array_set),
        ("$Json$array_push", json_array_push),
        ("$Json$array_pop", json_array_pop),

        // Constructors
        ("$Json$null", json_null),
        ("$Json$bool", json_bool),
        ("$Json$int", json_int),
        ("$Json$float", json_float),
        ("$Json$string", json_string),
        ("$Json$array", json_array),
        ("$Json$object", json_object),

        // Path access
        ("$Json$path_get", json_path_get),
    ]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_stringify() {
        let json_str = string_new(r#"{"name": "test", "value": 42}"#);
        let handle = json_parse(json_str);
        assert!(handle != 0);

        let result = json_stringify(handle);
        let result_str = unsafe { string_as_str(result) }.unwrap();
        assert!(result_str.contains("test"));
        assert!(result_str.contains("42"));

        json_free(handle);
    }

    #[test]
    fn test_type_checking() {
        let null_h = json_null();
        assert_eq!(json_is_null(null_h), 1);

        let bool_h = json_bool(1);
        assert_eq!(json_is_bool(bool_h), 1);

        let num_h = json_int(42);
        assert_eq!(json_is_number(num_h), 1);

        let str_h = json_string(string_new("hello"));
        assert_eq!(json_is_string(str_h), 1);

        let arr_h = json_array();
        assert_eq!(json_is_array(arr_h), 1);

        let obj_h = json_object();
        assert_eq!(json_is_object(obj_h), 1);
    }

    #[test]
    fn test_object_access() {
        let obj_h = json_object();
        let key = string_new("name");
        let value = json_string(string_new("test"));

        json_set(obj_h, key, value);
        assert_eq!(json_has(obj_h, key), 1);

        let got = json_get(obj_h, key);
        assert!(got != 0);
        let got_str = json_get_string(got);
        assert_eq!(unsafe { string_as_str(got_str) }, Some("test"));
    }

    #[test]
    fn test_array_access() {
        let arr_h = json_array();
        let val1 = json_int(1);
        let val2 = json_int(2);

        json_array_push(arr_h, val1);
        json_array_push(arr_h, val2);

        assert_eq!(json_array_length(arr_h), 2);

        let got = json_array_get(arr_h, 0);
        assert_eq!(json_get_int(got), 1);
    }

    #[test]
    fn test_path_get() {
        let json_str = string_new(r#"{"users": [{"name": "Alice"}, {"name": "Bob"}]}"#);
        let handle = json_parse(json_str);

        let path = string_new("users.1.name");
        let name_h = json_path_get(handle, path);
        let name = json_get_string(name_h);
        assert_eq!(unsafe { string_as_str(name) }, Some("Bob"));
    }
}
