//! ZRTL Regex Plugin
//!
//! Provides regular expression matching and replacement for Zyntax-based languages.
//!
//! ## Pattern Handles
//!
//! Compiled regex patterns are represented as opaque handles (u64) for efficiency.
//! Use `$Regex$compile` to create a pattern, and `$Regex$free` when done.
//!
//! ## Quick Functions
//!
//! For one-off operations, use the quick functions that compile and discard:
//! - `$Regex$is_match` - Check if pattern matches
//! - `$Regex$find` - Find first match
//! - `$Regex$replace` - Replace first match
//! - `$Regex$replace_all` - Replace all matches

use std::collections::HashMap;
use std::sync::Mutex;
use zrtl::{zrtl_plugin, StringPtr, ArrayPtr, string_new, string_as_str, array_new, array_push};
use regex::Regex;

// ============================================================================
// Handle Management
// ============================================================================

static REGEX_HANDLES: Mutex<Option<RegexHandleStore>> = Mutex::new(None);

struct RegexHandleStore {
    patterns: HashMap<u64, Regex>,
    next_id: u64,
}

impl RegexHandleStore {
    fn new() -> Self {
        Self {
            patterns: HashMap::new(),
            next_id: 1,
        }
    }

    fn insert(&mut self, pattern: Regex) -> u64 {
        let id = self.next_id;
        self.next_id += 1;
        self.patterns.insert(id, pattern);
        id
    }

    fn get(&self, id: u64) -> Option<&Regex> {
        self.patterns.get(&id)
    }

    fn remove(&mut self, id: u64) -> Option<Regex> {
        self.patterns.remove(&id)
    }
}

fn with_store<F, R>(f: F) -> R
where
    F: FnOnce(&mut RegexHandleStore) -> R,
{
    let mut guard = REGEX_HANDLES.lock().unwrap();
    if guard.is_none() {
        *guard = Some(RegexHandleStore::new());
    }
    f(guard.as_mut().unwrap())
}

// ============================================================================
// Pattern Compilation
// ============================================================================

/// Compile a regex pattern, returns handle (0 on error)
#[no_mangle]
pub extern "C" fn regex_compile(pattern: StringPtr) -> u64 {
    let pattern_str = match unsafe { string_as_str(pattern) } {
        Some(s) => s,
        None => return 0,
    };

    match Regex::new(pattern_str) {
        Ok(re) => with_store(|store| store.insert(re)),
        Err(_) => 0,
    }
}

/// Compile a case-insensitive regex pattern
#[no_mangle]
pub extern "C" fn regex_compile_ignorecase(pattern: StringPtr) -> u64 {
    let pattern_str = match unsafe { string_as_str(pattern) } {
        Some(s) => s,
        None => return 0,
    };

    match Regex::new(&format!("(?i){}", pattern_str)) {
        Ok(re) => with_store(|store| store.insert(re)),
        Err(_) => 0,
    }
}

/// Free a compiled regex pattern
#[no_mangle]
pub extern "C" fn regex_free(handle: u64) {
    with_store(|store| {
        store.remove(handle);
    });
}

// ============================================================================
// Matching (with compiled pattern)
// ============================================================================

/// Check if pattern matches string (using compiled pattern)
#[no_mangle]
pub extern "C" fn regex_matches(handle: u64, text: StringPtr) -> i32 {
    let text_str = match unsafe { string_as_str(text) } {
        Some(s) => s,
        None => return 0,
    };

    with_store(|store| {
        match store.get(handle) {
            Some(re) => re.is_match(text_str) as i32,
            None => 0,
        }
    })
}

/// Find first match, returns start position (-1 if not found)
#[no_mangle]
pub extern "C" fn regex_find_start(handle: u64, text: StringPtr) -> i64 {
    let text_str = match unsafe { string_as_str(text) } {
        Some(s) => s,
        None => return -1,
    };

    with_store(|store| {
        match store.get(handle) {
            Some(re) => {
                match re.find(text_str) {
                    Some(m) => m.start() as i64,
                    None => -1,
                }
            }
            None => -1,
        }
    })
}

/// Find first match, returns the matched string
#[no_mangle]
pub extern "C" fn regex_find_match(handle: u64, text: StringPtr) -> StringPtr {
    let text_str = match unsafe { string_as_str(text) } {
        Some(s) => s,
        None => return string_new(""),
    };

    with_store(|store| {
        match store.get(handle) {
            Some(re) => {
                match re.find(text_str) {
                    Some(m) => string_new(m.as_str()),
                    None => string_new(""),
                }
            }
            None => string_new(""),
        }
    })
}

/// Find all matches, returns array of strings
#[no_mangle]
pub extern "C" fn regex_find_all(handle: u64, text: StringPtr) -> ArrayPtr {
    let text_str = match unsafe { string_as_str(text) } {
        Some(s) => s,
        None => return array_new::<StringPtr>(0),
    };

    with_store(|store| {
        match store.get(handle) {
            Some(re) => {
                let matches: Vec<_> = re.find_iter(text_str).collect();
                let arr = array_new::<StringPtr>(matches.len());
                for m in matches {
                    let s = string_new(m.as_str());
                    unsafe { array_push(arr, s); }
                }
                arr
            }
            None => array_new::<StringPtr>(0),
        }
    })
}

/// Count number of matches
#[no_mangle]
pub extern "C" fn regex_count(handle: u64, text: StringPtr) -> i64 {
    let text_str = match unsafe { string_as_str(text) } {
        Some(s) => s,
        None => return 0,
    };

    with_store(|store| {
        match store.get(handle) {
            Some(re) => re.find_iter(text_str).count() as i64,
            None => 0,
        }
    })
}

// ============================================================================
// Replacement (with compiled pattern)
// ============================================================================

/// Replace first match
#[no_mangle]
pub extern "C" fn regex_replace_first(handle: u64, text: StringPtr, replacement: StringPtr) -> StringPtr {
    let text_str = match unsafe { string_as_str(text) } {
        Some(s) => s,
        None => return string_new(""),
    };
    let repl_str = unsafe { string_as_str(replacement) }.unwrap_or("");

    with_store(|store| {
        match store.get(handle) {
            Some(re) => string_new(&re.replace(text_str, repl_str)),
            None => string_new(text_str),
        }
    })
}

/// Replace all matches
#[no_mangle]
pub extern "C" fn regex_replace_all_compiled(handle: u64, text: StringPtr, replacement: StringPtr) -> StringPtr {
    let text_str = match unsafe { string_as_str(text) } {
        Some(s) => s,
        None => return string_new(""),
    };
    let repl_str = unsafe { string_as_str(replacement) }.unwrap_or("");

    with_store(|store| {
        match store.get(handle) {
            Some(re) => string_new(&re.replace_all(text_str, repl_str)),
            None => string_new(text_str),
        }
    })
}

// ============================================================================
// Capture Groups
// ============================================================================

/// Get capture group from first match (0 = full match, 1+ = groups)
#[no_mangle]
pub extern "C" fn regex_capture(handle: u64, text: StringPtr, group: i32) -> StringPtr {
    let text_str = match unsafe { string_as_str(text) } {
        Some(s) => s,
        None => return string_new(""),
    };

    with_store(|store| {
        match store.get(handle) {
            Some(re) => {
                match re.captures(text_str) {
                    Some(caps) => {
                        match caps.get(group as usize) {
                            Some(m) => string_new(m.as_str()),
                            None => string_new(""),
                        }
                    }
                    None => string_new(""),
                }
            }
            None => string_new(""),
        }
    })
}

/// Get all capture groups from first match as array
#[no_mangle]
pub extern "C" fn regex_captures(handle: u64, text: StringPtr) -> ArrayPtr {
    let text_str = match unsafe { string_as_str(text) } {
        Some(s) => s,
        None => return array_new::<StringPtr>(0),
    };

    with_store(|store| {
        match store.get(handle) {
            Some(re) => {
                match re.captures(text_str) {
                    Some(caps) => {
                        let arr = array_new::<StringPtr>(caps.len());
                        for i in 0..caps.len() {
                            let s = match caps.get(i) {
                                Some(m) => string_new(m.as_str()),
                                None => string_new(""),
                            };
                            unsafe { array_push(arr, s); }
                        }
                        arr
                    }
                    None => array_new::<StringPtr>(0),
                }
            }
            None => array_new::<StringPtr>(0),
        }
    })
}

// ============================================================================
// Split
// ============================================================================

/// Split string by regex pattern
#[no_mangle]
pub extern "C" fn regex_split(handle: u64, text: StringPtr) -> ArrayPtr {
    let text_str = match unsafe { string_as_str(text) } {
        Some(s) => s,
        None => return array_new::<StringPtr>(0),
    };

    with_store(|store| {
        match store.get(handle) {
            Some(re) => {
                let parts: Vec<_> = re.split(text_str).collect();
                let arr = array_new::<StringPtr>(parts.len());
                for part in parts {
                    let s = string_new(part);
                    unsafe { array_push(arr, s); }
                }
                arr
            }
            None => array_new::<StringPtr>(0),
        }
    })
}

/// Split string by regex pattern with limit
#[no_mangle]
pub extern "C" fn regex_splitn(handle: u64, text: StringPtr, limit: i32) -> ArrayPtr {
    let text_str = match unsafe { string_as_str(text) } {
        Some(s) => s,
        None => return array_new::<StringPtr>(0),
    };

    with_store(|store| {
        match store.get(handle) {
            Some(re) => {
                let parts: Vec<_> = re.splitn(text_str, limit as usize).collect();
                let arr = array_new::<StringPtr>(parts.len());
                for part in parts {
                    let s = string_new(part);
                    unsafe { array_push(arr, s); }
                }
                arr
            }
            None => array_new::<StringPtr>(0),
        }
    })
}

// ============================================================================
// Quick Functions (compile-and-discard)
// ============================================================================

/// Quick check if pattern matches (compiles pattern each call)
#[no_mangle]
pub extern "C" fn regex_is_match(pattern: StringPtr, text: StringPtr) -> i32 {
    let pattern_str = match unsafe { string_as_str(pattern) } {
        Some(s) => s,
        None => return 0,
    };
    let text_str = match unsafe { string_as_str(text) } {
        Some(s) => s,
        None => return 0,
    };

    match Regex::new(pattern_str) {
        Ok(re) => re.is_match(text_str) as i32,
        Err(_) => 0,
    }
}

/// Quick find first match
#[no_mangle]
pub extern "C" fn regex_find(pattern: StringPtr, text: StringPtr) -> StringPtr {
    let pattern_str = match unsafe { string_as_str(pattern) } {
        Some(s) => s,
        None => return string_new(""),
    };
    let text_str = match unsafe { string_as_str(text) } {
        Some(s) => s,
        None => return string_new(""),
    };

    match Regex::new(pattern_str) {
        Ok(re) => {
            match re.find(text_str) {
                Some(m) => string_new(m.as_str()),
                None => string_new(""),
            }
        }
        Err(_) => string_new(""),
    }
}

/// Quick replace first match
#[no_mangle]
pub extern "C" fn regex_replace(pattern: StringPtr, text: StringPtr, replacement: StringPtr) -> StringPtr {
    let pattern_str = match unsafe { string_as_str(pattern) } {
        Some(s) => s,
        None => return string_new(""),
    };
    let text_str = match unsafe { string_as_str(text) } {
        Some(s) => s,
        None => return string_new(""),
    };
    let repl_str = unsafe { string_as_str(replacement) }.unwrap_or("");

    match Regex::new(pattern_str) {
        Ok(re) => string_new(&re.replace(text_str, repl_str)),
        Err(_) => string_new(text_str),
    }
}

/// Quick replace all matches
#[no_mangle]
pub extern "C" fn regex_replace_all(pattern: StringPtr, text: StringPtr, replacement: StringPtr) -> StringPtr {
    let pattern_str = match unsafe { string_as_str(pattern) } {
        Some(s) => s,
        None => return string_new(""),
    };
    let text_str = match unsafe { string_as_str(text) } {
        Some(s) => s,
        None => return string_new(""),
    };
    let repl_str = unsafe { string_as_str(replacement) }.unwrap_or("");

    match Regex::new(pattern_str) {
        Ok(re) => string_new(&re.replace_all(text_str, repl_str)),
        Err(_) => string_new(text_str),
    }
}

// ============================================================================
// Utility
// ============================================================================

/// Escape special regex characters in a string
#[no_mangle]
pub extern "C" fn regex_escape(text: StringPtr) -> StringPtr {
    let text_str = match unsafe { string_as_str(text) } {
        Some(s) => s,
        None => return string_new(""),
    };
    string_new(&regex::escape(text_str))
}

// ============================================================================
// Plugin Export
// ============================================================================

zrtl_plugin! {
    name: "zrtl_regex",
    symbols: [
        // Compilation
        ("$Regex$compile", regex_compile),
        ("$Regex$compile_ignorecase", regex_compile_ignorecase),
        ("$Regex$free", regex_free),

        // Matching (compiled)
        ("$Regex$matches", regex_matches),
        ("$Regex$find_start", regex_find_start),
        ("$Regex$find_match", regex_find_match),
        ("$Regex$find_all", regex_find_all),
        ("$Regex$count", regex_count),

        // Replacement (compiled)
        ("$Regex$replace_first", regex_replace_first),
        ("$Regex$replace_all_compiled", regex_replace_all_compiled),

        // Capture groups
        ("$Regex$capture", regex_capture),
        ("$Regex$captures", regex_captures),

        // Split
        ("$Regex$split", regex_split),
        ("$Regex$splitn", regex_splitn),

        // Quick functions
        ("$Regex$is_match", regex_is_match),
        ("$Regex$find", regex_find),
        ("$Regex$replace", regex_replace),
        ("$Regex$replace_all", regex_replace_all),

        // Utility
        ("$Regex$escape", regex_escape),
    ]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compile_match() {
        let pattern = string_new(r"\d+");
        let handle = regex_compile(pattern);
        assert!(handle != 0);

        let text = string_new("hello 123 world");
        assert_eq!(regex_matches(handle, text), 1);

        let no_match = string_new("hello world");
        assert_eq!(regex_matches(handle, no_match), 0);

        regex_free(handle);
    }

    #[test]
    fn test_find() {
        let pattern = string_new(r"\d+");
        let handle = regex_compile(pattern);

        let text = string_new("abc 123 def 456");
        let found = regex_find_match(handle, text);
        assert_eq!(unsafe { string_as_str(found) }, Some("123"));

        assert_eq!(regex_find_start(handle, text), 4);
    }

    #[test]
    fn test_find_all() {
        let pattern = string_new(r"\d+");
        let handle = regex_compile(pattern);

        let text = string_new("a1b2c3");
        let arr = regex_find_all(handle, text);

        // Should have 3 matches: "1", "2", "3"
        assert_eq!(regex_count(handle, text), 3);
    }

    #[test]
    fn test_replace() {
        let pattern = string_new(r"\d+");
        let text = string_new("a1b2c3");
        let repl = string_new("X");

        let result = regex_replace_all(pattern, text, repl);
        assert_eq!(unsafe { string_as_str(result) }, Some("aXbXcX"));
    }

    #[test]
    fn test_captures() {
        let pattern = string_new(r"(\w+)@(\w+)\.(\w+)");
        let handle = regex_compile(pattern);

        let text = string_new("Contact: user@example.com");

        let group1 = regex_capture(handle, text, 1);
        assert_eq!(unsafe { string_as_str(group1) }, Some("user"));

        let group2 = regex_capture(handle, text, 2);
        assert_eq!(unsafe { string_as_str(group2) }, Some("example"));
    }

    #[test]
    fn test_split() {
        let pattern = string_new(r"\s+");
        let handle = regex_compile(pattern);

        let text = string_new("hello   world  foo");
        let arr = regex_split(handle, text);
        // Should be ["hello", "world", "foo"]
        assert!(arr as usize != 0);
    }

    #[test]
    fn test_escape() {
        let text = string_new("hello.world*");
        let escaped = regex_escape(text);
        assert_eq!(unsafe { string_as_str(escaped) }, Some(r"hello\.world\*"));
    }

    #[test]
    fn test_quick_match() {
        let pattern = string_new(r"\d+");
        let text = string_new("abc 123");
        assert_eq!(regex_is_match(pattern, text), 1);
    }
}
