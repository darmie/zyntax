//! ZRTL Process Plugin
//!
//! Provides subprocess execution and management for Zyntax-based languages.
//!
//! ## Quick Execution
//!
//! - `$Process$run` - Run command and wait for completion
//! - `$Process$run_capture` - Run and capture output
//! - `$Process$shell` - Run shell command
//!
//! ## Process Handles
//!
//! For advanced control, spawn processes and manage them:
//! - `$Process$spawn` - Start a new process
//! - `$Process$wait` - Wait for process to finish
//! - `$Process$kill` - Kill a running process
//! - `$Process$read_stdout` - Read from stdout
//! - `$Process$write_stdin` - Write to stdin

use std::collections::HashMap;
use std::io::{Read, Write};
use std::process::{Child, Command, Stdio, Output};
use std::sync::Mutex;
use zrtl::{zrtl_plugin, StringPtr, ArrayPtr, string_new, string_as_str, array_new, array_push};

// ============================================================================
// Handle Management
// ============================================================================

static PROCESS_HANDLES: Mutex<Option<ProcessHandleStore>> = Mutex::new(None);

struct ProcessHandleStore {
    processes: HashMap<u64, Child>,
    next_id: u64,
}

impl ProcessHandleStore {
    fn new() -> Self {
        Self {
            processes: HashMap::new(),
            next_id: 1,
        }
    }

    fn insert(&mut self, child: Child) -> u64 {
        let id = self.next_id;
        self.next_id += 1;
        self.processes.insert(id, child);
        id
    }

    fn get_mut(&mut self, id: u64) -> Option<&mut Child> {
        self.processes.get_mut(&id)
    }

    fn remove(&mut self, id: u64) -> Option<Child> {
        self.processes.remove(&id)
    }
}

fn with_store<F, R>(f: F) -> R
where
    F: FnOnce(&mut ProcessHandleStore) -> R,
{
    let mut guard = PROCESS_HANDLES.lock().unwrap();
    if guard.is_none() {
        *guard = Some(ProcessHandleStore::new());
    }
    f(guard.as_mut().unwrap())
}

// ============================================================================
// Quick Execution
// ============================================================================

/// Run a command and wait for it to complete
/// Returns exit code (or -1 on error)
#[no_mangle]
pub extern "C" fn process_run(program: StringPtr, args: ArrayPtr) -> i32 {
    let program_str = match unsafe { string_as_str(program) } {
        Some(s) => s,
        None => return -1,
    };

    let mut cmd = Command::new(program_str);

    // Parse args array
    if !args.is_null() {
        let header = args as *const i32;
        let _capacity = unsafe { *header };
        let length = unsafe { *header.add(1) };
        let data = unsafe { header.add(2) } as *const StringPtr;

        for i in 0..length as usize {
            let arg_ptr = unsafe { *data.add(i) };
            if let Some(arg) = unsafe { string_as_str(arg_ptr) } {
                cmd.arg(arg);
            }
        }
    }

    match cmd.status() {
        Ok(status) => status.code().unwrap_or(-1),
        Err(_) => -1,
    }
}

/// Run a command and capture stdout
/// Returns stdout as string (empty on error)
#[no_mangle]
pub extern "C" fn process_run_capture(program: StringPtr, args: ArrayPtr) -> StringPtr {
    let program_str = match unsafe { string_as_str(program) } {
        Some(s) => s,
        None => return string_new(""),
    };

    let mut cmd = Command::new(program_str);
    cmd.stdout(Stdio::piped())
       .stderr(Stdio::piped());

    // Parse args array
    if !args.is_null() {
        let header = args as *const i32;
        let _capacity = unsafe { *header };
        let length = unsafe { *header.add(1) };
        let data = unsafe { header.add(2) } as *const StringPtr;

        for i in 0..length as usize {
            let arg_ptr = unsafe { *data.add(i) };
            if let Some(arg) = unsafe { string_as_str(arg_ptr) } {
                cmd.arg(arg);
            }
        }
    }

    match cmd.output() {
        Ok(output) => {
            string_new(&String::from_utf8_lossy(&output.stdout))
        }
        Err(_) => string_new(""),
    }
}

/// Run a shell command (uses /bin/sh -c on Unix, cmd /C on Windows)
/// Returns exit code
#[no_mangle]
pub extern "C" fn process_shell(command: StringPtr) -> i32 {
    let command_str = match unsafe { string_as_str(command) } {
        Some(s) => s,
        None => return -1,
    };

    #[cfg(windows)]
    let status = Command::new("cmd")
        .args(["/C", command_str])
        .status();

    #[cfg(not(windows))]
    let status = Command::new("/bin/sh")
        .args(["-c", command_str])
        .status();

    match status {
        Ok(s) => s.code().unwrap_or(-1),
        Err(_) => -1,
    }
}

/// Run a shell command and capture output
#[no_mangle]
pub extern "C" fn process_shell_capture(command: StringPtr) -> StringPtr {
    let command_str = match unsafe { string_as_str(command) } {
        Some(s) => s,
        None => return string_new(""),
    };

    #[cfg(windows)]
    let output = Command::new("cmd")
        .args(["/C", command_str])
        .output();

    #[cfg(not(windows))]
    let output = Command::new("/bin/sh")
        .args(["-c", command_str])
        .output();

    match output {
        Ok(out) => string_new(&String::from_utf8_lossy(&out.stdout)),
        Err(_) => string_new(""),
    }
}

// ============================================================================
// Process Spawning
// ============================================================================

/// Spawn a new process (returns handle, 0 on error)
#[no_mangle]
pub extern "C" fn process_spawn(program: StringPtr, args: ArrayPtr) -> u64 {
    let program_str = match unsafe { string_as_str(program) } {
        Some(s) => s,
        None => return 0,
    };

    let mut cmd = Command::new(program_str);
    cmd.stdin(Stdio::piped())
       .stdout(Stdio::piped())
       .stderr(Stdio::piped());

    // Parse args array
    if !args.is_null() {
        let header = args as *const i32;
        let _capacity = unsafe { *header };
        let length = unsafe { *header.add(1) };
        let data = unsafe { header.add(2) } as *const StringPtr;

        for i in 0..length as usize {
            let arg_ptr = unsafe { *data.add(i) };
            if let Some(arg) = unsafe { string_as_str(arg_ptr) } {
                cmd.arg(arg);
            }
        }
    }

    match cmd.spawn() {
        Ok(child) => with_store(|store| store.insert(child)),
        Err(_) => 0,
    }
}

/// Wait for a process to complete (returns exit code, -1 on error)
#[no_mangle]
pub extern "C" fn process_wait(handle: u64) -> i32 {
    with_store(|store| {
        match store.remove(handle) {
            Some(mut child) => {
                match child.wait() {
                    Ok(status) => status.code().unwrap_or(-1),
                    Err(_) => -1,
                }
            }
            None => -1,
        }
    })
}

/// Check if a process is still running
#[no_mangle]
pub extern "C" fn process_is_running(handle: u64) -> i32 {
    with_store(|store| {
        match store.get_mut(handle) {
            Some(child) => {
                match child.try_wait() {
                    Ok(None) => 1,  // Still running
                    Ok(Some(_)) => 0,  // Finished
                    Err(_) => 0,
                }
            }
            None => 0,
        }
    })
}

/// Kill a running process
#[no_mangle]
pub extern "C" fn process_kill(handle: u64) -> i32 {
    with_store(|store| {
        match store.get_mut(handle) {
            Some(child) => {
                match child.kill() {
                    Ok(_) => 1,
                    Err(_) => 0,
                }
            }
            None => 0,
        }
    })
}

/// Get the process ID of a spawned process
#[no_mangle]
pub extern "C" fn process_id(handle: u64) -> u32 {
    with_store(|store| {
        match store.get_mut(handle) {
            Some(child) => child.id(),
            None => 0,
        }
    })
}

// ============================================================================
// I/O
// ============================================================================

/// Read all available stdout from a spawned process
#[no_mangle]
pub extern "C" fn process_read_stdout(handle: u64) -> StringPtr {
    with_store(|store| {
        match store.get_mut(handle) {
            Some(child) => {
                if let Some(stdout) = child.stdout.as_mut() {
                    let mut buffer = Vec::new();
                    match stdout.read_to_end(&mut buffer) {
                        Ok(_) => string_new(&String::from_utf8_lossy(&buffer)),
                        Err(_) => string_new(""),
                    }
                } else {
                    string_new("")
                }
            }
            None => string_new(""),
        }
    })
}

/// Read all available stderr from a spawned process
#[no_mangle]
pub extern "C" fn process_read_stderr(handle: u64) -> StringPtr {
    with_store(|store| {
        match store.get_mut(handle) {
            Some(child) => {
                if let Some(stderr) = child.stderr.as_mut() {
                    let mut buffer = Vec::new();
                    match stderr.read_to_end(&mut buffer) {
                        Ok(_) => string_new(&String::from_utf8_lossy(&buffer)),
                        Err(_) => string_new(""),
                    }
                } else {
                    string_new("")
                }
            }
            None => string_new(""),
        }
    })
}

/// Write to stdin of a spawned process
#[no_mangle]
pub extern "C" fn process_write_stdin(handle: u64, data: StringPtr) -> i32 {
    let data_str = match unsafe { string_as_str(data) } {
        Some(s) => s,
        None => return 0,
    };

    with_store(|store| {
        match store.get_mut(handle) {
            Some(child) => {
                if let Some(stdin) = child.stdin.as_mut() {
                    match stdin.write_all(data_str.as_bytes()) {
                        Ok(_) => data_str.len() as i32,
                        Err(_) => -1,
                    }
                } else {
                    -1
                }
            }
            None => -1,
        }
    })
}

/// Close stdin of a spawned process (signals EOF)
#[no_mangle]
pub extern "C" fn process_close_stdin(handle: u64) {
    with_store(|store| {
        if let Some(child) = store.get_mut(handle) {
            child.stdin.take();
        }
    });
}

// ============================================================================
// Utility
// ============================================================================

/// Get the current process ID
#[no_mangle]
pub extern "C" fn process_current_pid() -> u32 {
    std::process::id()
}

/// Check if a command exists (searches PATH)
#[no_mangle]
pub extern "C" fn process_command_exists(program: StringPtr) -> i32 {
    let program_str = match unsafe { string_as_str(program) } {
        Some(s) => s,
        None => return 0,
    };

    // Try to spawn with --version or --help to check existence
    // A simpler approach: use which/where
    #[cfg(windows)]
    let result = Command::new("where")
        .arg(program_str)
        .output();

    #[cfg(not(windows))]
    let result = Command::new("which")
        .arg(program_str)
        .output();

    match result {
        Ok(output) => output.status.success() as i32,
        Err(_) => 0,
    }
}

// ============================================================================
// Plugin Export
// ============================================================================

zrtl_plugin! {
    name: "zrtl_process",
    symbols: [
        // Quick execution
        ("$Process$run", process_run),
        ("$Process$run_capture", process_run_capture),
        ("$Process$shell", process_shell),
        ("$Process$shell_capture", process_shell_capture),

        // Process management
        ("$Process$spawn", process_spawn),
        ("$Process$wait", process_wait),
        ("$Process$is_running", process_is_running),
        ("$Process$kill", process_kill),
        ("$Process$id", process_id),

        // I/O
        ("$Process$read_stdout", process_read_stdout),
        ("$Process$read_stderr", process_read_stderr),
        ("$Process$write_stdin", process_write_stdin),
        ("$Process$close_stdin", process_close_stdin),

        // Utility
        ("$Process$current_pid", process_current_pid),
        ("$Process$command_exists", process_command_exists),
    ]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_shell_echo() {
        let cmd = string_new("echo hello");
        let output = process_shell_capture(cmd);
        let output_str = unsafe { string_as_str(output) }.unwrap();
        assert!(output_str.contains("hello"));
    }

    #[test]
    fn test_run_capture() {
        let program = string_new("echo");
        let arr = array_new::<StringPtr>(1);
        let arg = string_new("test");
        unsafe { array_push(arr, arg); }

        let output = process_run_capture(program, arr);
        let output_str = unsafe { string_as_str(output) }.unwrap();
        assert!(output_str.contains("test"));
    }

    #[test]
    fn test_current_pid() {
        let pid = process_current_pid();
        assert!(pid > 0);
    }

    #[test]
    fn test_command_exists() {
        // echo should exist everywhere
        let cmd = string_new("echo");
        assert_eq!(process_command_exists(cmd), 1);

        // nonexistent command
        let bad = string_new("this_command_should_not_exist_12345");
        assert_eq!(process_command_exists(bad), 0);
    }
}
