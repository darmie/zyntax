//! ZRTL SQL Plugin
//!
//! Provides SQLite database operations with a handle-based API.
//!
//! ## Connection Management
//! - `$Sql$open` - Open a database (file path or ":memory:")
//! - `$Sql$close` - Close a database connection
//!
//! ## Query Execution
//! - `$Sql$execute` - Execute SQL (INSERT/UPDATE/DELETE), returns rows affected
//! - `$Sql$query` - Execute SELECT, returns result set handle
//!
//! ## Result Set Navigation
//! - `$Sql$next` - Move to next row (returns 1 if row available, 0 if done)
//! - `$Sql$get_int` - Get integer column value
//! - `$Sql$get_float` - Get float column value
//! - `$Sql$get_string` - Get string column value
//! - `$Sql$get_blob` - Get blob column value as byte array
//! - `$Sql$free_result` - Free result set

use std::collections::HashMap;
use std::sync::Mutex;
use std::sync::atomic::{AtomicI64, Ordering};
use rusqlite::{Connection, Statement, Row, types::Value};
use zrtl::{zrtl_plugin, StringPtr, ArrayPtr, string_new, string_as_str, array_new, array_push};

// ============================================================================
// Handle Management
// ============================================================================

static NEXT_DB_ID: AtomicI64 = AtomicI64::new(1);
static NEXT_STMT_ID: AtomicI64 = AtomicI64::new(1);
static NEXT_RESULT_ID: AtomicI64 = AtomicI64::new(1);

struct DbConnection {
    conn: Connection,
}

struct ResultSet {
    columns: Vec<String>,
    rows: Vec<Vec<Value>>,
    current_row: usize,
}

lazy_static::lazy_static! {
    static ref CONNECTIONS: Mutex<HashMap<i64, DbConnection>> = Mutex::new(HashMap::new());
    static ref RESULTS: Mutex<HashMap<i64, ResultSet>> = Mutex::new(HashMap::new());
}

// ============================================================================
// Connection Management
// ============================================================================

/// Open a SQLite database
/// path: file path or ":memory:" for in-memory database
/// Returns database handle, or 0 on error
#[no_mangle]
pub extern "C" fn sql_open(path: StringPtr) -> i64 {
    let path_str = match unsafe { string_as_str(path) } {
        Some(s) => s,
        None => return 0,
    };

    let conn = match Connection::open(path_str) {
        Ok(c) => c,
        Err(_) => return 0,
    };

    let id = NEXT_DB_ID.fetch_add(1, Ordering::SeqCst);
    if let Ok(mut conns) = CONNECTIONS.lock() {
        conns.insert(id, DbConnection { conn });
        id
    } else {
        0
    }
}

/// Open an in-memory database
/// Returns database handle, or 0 on error
#[no_mangle]
pub extern "C" fn sql_open_memory() -> i64 {
    let conn = match Connection::open_in_memory() {
        Ok(c) => c,
        Err(_) => return 0,
    };

    let id = NEXT_DB_ID.fetch_add(1, Ordering::SeqCst);
    if let Ok(mut conns) = CONNECTIONS.lock() {
        conns.insert(id, DbConnection { conn });
        id
    } else {
        0
    }
}

/// Close a database connection
#[no_mangle]
pub extern "C" fn sql_close(handle: i64) {
    if let Ok(mut conns) = CONNECTIONS.lock() {
        conns.remove(&handle);
    }
}

// ============================================================================
// Query Execution
// ============================================================================

/// Execute SQL statement (INSERT/UPDATE/DELETE/CREATE/etc)
/// Returns number of rows affected, or -1 on error
#[no_mangle]
pub extern "C" fn sql_execute(db: i64, sql: StringPtr) -> i64 {
    let sql_str = match unsafe { string_as_str(sql) } {
        Some(s) => s,
        None => return -1,
    };

    let conns = match CONNECTIONS.lock() {
        Ok(c) => c,
        Err(_) => return -1,
    };

    let db_conn = match conns.get(&db) {
        Some(c) => c,
        None => return -1,
    };

    match db_conn.conn.execute(sql_str, []) {
        Ok(rows) => rows as i64,
        Err(_) => -1,
    }
}

/// Execute SQL with one string parameter
#[no_mangle]
pub extern "C" fn sql_execute_1(db: i64, sql: StringPtr, p1: StringPtr) -> i64 {
    let sql_str = match unsafe { string_as_str(sql) } {
        Some(s) => s,
        None => return -1,
    };
    let p1_str = unsafe { string_as_str(p1) }.unwrap_or("");

    let conns = match CONNECTIONS.lock() {
        Ok(c) => c,
        Err(_) => return -1,
    };

    let db_conn = match conns.get(&db) {
        Some(c) => c,
        None => return -1,
    };

    match db_conn.conn.execute(sql_str, [p1_str]) {
        Ok(rows) => rows as i64,
        Err(_) => -1,
    }
}

/// Execute SQL with two string parameters
#[no_mangle]
pub extern "C" fn sql_execute_2(db: i64, sql: StringPtr, p1: StringPtr, p2: StringPtr) -> i64 {
    let sql_str = match unsafe { string_as_str(sql) } {
        Some(s) => s,
        None => return -1,
    };
    let p1_str = unsafe { string_as_str(p1) }.unwrap_or("");
    let p2_str = unsafe { string_as_str(p2) }.unwrap_or("");

    let conns = match CONNECTIONS.lock() {
        Ok(c) => c,
        Err(_) => return -1,
    };

    let db_conn = match conns.get(&db) {
        Some(c) => c,
        None => return -1,
    };

    match db_conn.conn.execute(sql_str, [p1_str, p2_str]) {
        Ok(rows) => rows as i64,
        Err(_) => -1,
    }
}

/// Execute SQL with three string parameters
#[no_mangle]
pub extern "C" fn sql_execute_3(db: i64, sql: StringPtr, p1: StringPtr, p2: StringPtr, p3: StringPtr) -> i64 {
    let sql_str = match unsafe { string_as_str(sql) } {
        Some(s) => s,
        None => return -1,
    };
    let p1_str = unsafe { string_as_str(p1) }.unwrap_or("");
    let p2_str = unsafe { string_as_str(p2) }.unwrap_or("");
    let p3_str = unsafe { string_as_str(p3) }.unwrap_or("");

    let conns = match CONNECTIONS.lock() {
        Ok(c) => c,
        Err(_) => return -1,
    };

    let db_conn = match conns.get(&db) {
        Some(c) => c,
        None => return -1,
    };

    match db_conn.conn.execute(sql_str, [p1_str, p2_str, p3_str]) {
        Ok(rows) => rows as i64,
        Err(_) => -1,
    }
}

/// Execute SELECT query
/// Returns result set handle, or 0 on error
#[no_mangle]
pub extern "C" fn sql_query(db: i64, sql: StringPtr) -> i64 {
    let sql_str = match unsafe { string_as_str(sql) } {
        Some(s) => s,
        None => return 0,
    };

    let conns = match CONNECTIONS.lock() {
        Ok(c) => c,
        Err(_) => return 0,
    };

    let db_conn = match conns.get(&db) {
        Some(c) => c,
        None => return 0,
    };

    let mut stmt = match db_conn.conn.prepare(sql_str) {
        Ok(s) => s,
        Err(_) => return 0,
    };

    // Get column names
    let columns: Vec<String> = stmt.column_names().iter().map(|s| s.to_string()).collect();
    let col_count = columns.len();

    // Collect all rows
    let mut rows: Vec<Vec<Value>> = Vec::new();
    let row_iter = match stmt.query_map([], |row| {
        let mut values = Vec::with_capacity(col_count);
        for i in 0..col_count {
            let val: Value = row.get(i).unwrap_or(Value::Null);
            values.push(val);
        }
        Ok(values)
    }) {
        Ok(iter) => iter,
        Err(_) => return 0,
    };

    for row_result in row_iter {
        if let Ok(row) = row_result {
            rows.push(row);
        }
    }

    let result_id = NEXT_RESULT_ID.fetch_add(1, Ordering::SeqCst);
    let result_set = ResultSet {
        columns,
        rows,
        current_row: 0,
    };

    if let Ok(mut results) = RESULTS.lock() {
        results.insert(result_id, result_set);
        result_id
    } else {
        0
    }
}

/// Execute SELECT with one string parameter
#[no_mangle]
pub extern "C" fn sql_query_1(db: i64, sql: StringPtr, p1: StringPtr) -> i64 {
    let sql_str = match unsafe { string_as_str(sql) } {
        Some(s) => s,
        None => return 0,
    };
    let p1_str = unsafe { string_as_str(p1) }.unwrap_or("");

    let conns = match CONNECTIONS.lock() {
        Ok(c) => c,
        Err(_) => return 0,
    };

    let db_conn = match conns.get(&db) {
        Some(c) => c,
        None => return 0,
    };

    let mut stmt = match db_conn.conn.prepare(sql_str) {
        Ok(s) => s,
        Err(_) => return 0,
    };

    let columns: Vec<String> = stmt.column_names().iter().map(|s| s.to_string()).collect();
    let col_count = columns.len();

    let mut rows: Vec<Vec<Value>> = Vec::new();
    let row_iter = match stmt.query_map([p1_str], |row| {
        let mut values = Vec::with_capacity(col_count);
        for i in 0..col_count {
            let val: Value = row.get(i).unwrap_or(Value::Null);
            values.push(val);
        }
        Ok(values)
    }) {
        Ok(iter) => iter,
        Err(_) => return 0,
    };

    for row_result in row_iter {
        if let Ok(row) = row_result {
            rows.push(row);
        }
    }

    let result_id = NEXT_RESULT_ID.fetch_add(1, Ordering::SeqCst);
    let result_set = ResultSet {
        columns,
        rows,
        current_row: 0,
    };

    if let Ok(mut results) = RESULTS.lock() {
        results.insert(result_id, result_set);
        result_id
    } else {
        0
    }
}

// ============================================================================
// Result Set Navigation
// ============================================================================

/// Move to next row in result set
/// Returns 1 if row is available, 0 if no more rows
#[no_mangle]
pub extern "C" fn sql_next(result: i64) -> i32 {
    if let Ok(mut results) = RESULTS.lock() {
        if let Some(rs) = results.get_mut(&result) {
            if rs.current_row < rs.rows.len() {
                rs.current_row += 1;
                return 1;
            }
        }
    }
    0
}

/// Reset result set to beginning
#[no_mangle]
pub extern "C" fn sql_reset(result: i64) {
    if let Ok(mut results) = RESULTS.lock() {
        if let Some(rs) = results.get_mut(&result) {
            rs.current_row = 0;
        }
    }
}

/// Get number of rows in result set
#[no_mangle]
pub extern "C" fn sql_row_count(result: i64) -> i64 {
    if let Ok(results) = RESULTS.lock() {
        if let Some(rs) = results.get(&result) {
            return rs.rows.len() as i64;
        }
    }
    0
}

/// Get number of columns in result set
#[no_mangle]
pub extern "C" fn sql_column_count(result: i64) -> i32 {
    if let Ok(results) = RESULTS.lock() {
        if let Some(rs) = results.get(&result) {
            return rs.columns.len() as i32;
        }
    }
    0
}

/// Get column name by index
#[no_mangle]
pub extern "C" fn sql_column_name(result: i64, col: i32) -> StringPtr {
    if let Ok(results) = RESULTS.lock() {
        if let Some(rs) = results.get(&result) {
            if let Some(name) = rs.columns.get(col as usize) {
                return string_new(name);
            }
        }
    }
    string_new("")
}

/// Get integer value from current row
#[no_mangle]
pub extern "C" fn sql_get_int(result: i64, col: i32) -> i64 {
    if let Ok(results) = RESULTS.lock() {
        if let Some(rs) = results.get(&result) {
            if rs.current_row > 0 && rs.current_row <= rs.rows.len() {
                let row = &rs.rows[rs.current_row - 1];
                if let Some(val) = row.get(col as usize) {
                    return match val {
                        Value::Integer(i) => *i,
                        Value::Real(f) => *f as i64,
                        Value::Text(s) => s.parse().unwrap_or(0),
                        _ => 0,
                    };
                }
            }
        }
    }
    0
}

/// Get float value from current row
#[no_mangle]
pub extern "C" fn sql_get_float(result: i64, col: i32) -> f64 {
    if let Ok(results) = RESULTS.lock() {
        if let Some(rs) = results.get(&result) {
            if rs.current_row > 0 && rs.current_row <= rs.rows.len() {
                let row = &rs.rows[rs.current_row - 1];
                if let Some(val) = row.get(col as usize) {
                    return match val {
                        Value::Real(f) => *f,
                        Value::Integer(i) => *i as f64,
                        Value::Text(s) => s.parse().unwrap_or(0.0),
                        _ => 0.0,
                    };
                }
            }
        }
    }
    0.0
}

/// Get string value from current row
#[no_mangle]
pub extern "C" fn sql_get_string(result: i64, col: i32) -> StringPtr {
    if let Ok(results) = RESULTS.lock() {
        if let Some(rs) = results.get(&result) {
            if rs.current_row > 0 && rs.current_row <= rs.rows.len() {
                let row = &rs.rows[rs.current_row - 1];
                if let Some(val) = row.get(col as usize) {
                    return match val {
                        Value::Text(s) => string_new(s),
                        Value::Integer(i) => string_new(&i.to_string()),
                        Value::Real(f) => string_new(&f.to_string()),
                        Value::Null => string_new(""),
                        Value::Blob(b) => {
                            // Try to interpret as UTF-8
                            match std::str::from_utf8(b) {
                                Ok(s) => string_new(s),
                                Err(_) => string_new(""),
                            }
                        }
                    };
                }
            }
        }
    }
    string_new("")
}

/// Get blob value from current row as byte array
#[no_mangle]
pub extern "C" fn sql_get_blob(result: i64, col: i32) -> ArrayPtr {
    if let Ok(results) = RESULTS.lock() {
        if let Some(rs) = results.get(&result) {
            if rs.current_row > 0 && rs.current_row <= rs.rows.len() {
                let row = &rs.rows[rs.current_row - 1];
                if let Some(val) = row.get(col as usize) {
                    if let Value::Blob(b) = val {
                        let mut arr = array_new::<u8>(b.len());
                        for &byte in b {
                            arr = unsafe { array_push(arr, byte) };
                        }
                        return arr;
                    }
                }
            }
        }
    }
    array_new::<u8>(0)
}

/// Check if value is NULL
#[no_mangle]
pub extern "C" fn sql_is_null(result: i64, col: i32) -> i32 {
    if let Ok(results) = RESULTS.lock() {
        if let Some(rs) = results.get(&result) {
            if rs.current_row > 0 && rs.current_row <= rs.rows.len() {
                let row = &rs.rows[rs.current_row - 1];
                if let Some(val) = row.get(col as usize) {
                    return matches!(val, Value::Null) as i32;
                }
            }
        }
    }
    1 // If we can't find it, treat as null
}

/// Free result set
#[no_mangle]
pub extern "C" fn sql_free_result(result: i64) {
    if let Ok(mut results) = RESULTS.lock() {
        results.remove(&result);
    }
}

// ============================================================================
// Transaction Support
// ============================================================================

/// Begin transaction
#[no_mangle]
pub extern "C" fn sql_begin(db: i64) -> i32 {
    if sql_execute(db, string_new("BEGIN")) >= 0 { 1 } else { 0 }
}

/// Commit transaction
#[no_mangle]
pub extern "C" fn sql_commit(db: i64) -> i32 {
    if sql_execute(db, string_new("COMMIT")) >= 0 { 1 } else { 0 }
}

/// Rollback transaction
#[no_mangle]
pub extern "C" fn sql_rollback(db: i64) -> i32 {
    if sql_execute(db, string_new("ROLLBACK")) >= 0 { 1 } else { 0 }
}

// ============================================================================
// Utility Functions
// ============================================================================

/// Get last insert rowid
#[no_mangle]
pub extern "C" fn sql_last_insert_id(db: i64) -> i64 {
    if let Ok(conns) = CONNECTIONS.lock() {
        if let Some(db_conn) = conns.get(&db) {
            return db_conn.conn.last_insert_rowid();
        }
    }
    0
}

/// Get last error message
#[no_mangle]
pub extern "C" fn sql_error(db: i64) -> StringPtr {
    // rusqlite doesn't provide easy access to last error, return empty
    string_new("")
}

/// Escape a string for SQL (basic - prefer parameterized queries)
#[no_mangle]
pub extern "C" fn sql_escape(s: StringPtr) -> StringPtr {
    let s_str = match unsafe { string_as_str(s) } {
        Some(s) => s,
        None => return string_new(""),
    };

    let escaped = s_str.replace('\'', "''");
    string_new(&escaped)
}

// ============================================================================
// Plugin Export
// ============================================================================

zrtl_plugin! {
    name: "zrtl_sql",
    symbols: [
        // Connection
        ("$Sql$open", sql_open),
        ("$Sql$open_memory", sql_open_memory),
        ("$Sql$close", sql_close),

        // Execute
        ("$Sql$execute", sql_execute),
        ("$Sql$execute_1", sql_execute_1),
        ("$Sql$execute_2", sql_execute_2),
        ("$Sql$execute_3", sql_execute_3),

        // Query
        ("$Sql$query", sql_query),
        ("$Sql$query_1", sql_query_1),

        // Result navigation
        ("$Sql$next", sql_next),
        ("$Sql$reset", sql_reset),
        ("$Sql$row_count", sql_row_count),
        ("$Sql$column_count", sql_column_count),
        ("$Sql$column_name", sql_column_name),

        // Get values
        ("$Sql$get_int", sql_get_int),
        ("$Sql$get_float", sql_get_float),
        ("$Sql$get_string", sql_get_string),
        ("$Sql$get_blob", sql_get_blob),
        ("$Sql$is_null", sql_is_null),
        ("$Sql$free_result", sql_free_result),

        // Transactions
        ("$Sql$begin", sql_begin),
        ("$Sql$commit", sql_commit),
        ("$Sql$rollback", sql_rollback),

        // Utility
        ("$Sql$last_insert_id", sql_last_insert_id),
        ("$Sql$error", sql_error),
        ("$Sql$escape", sql_escape),
    ]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_open_memory() {
        let db = sql_open_memory();
        assert!(db > 0);
        sql_close(db);
    }

    #[test]
    fn test_create_table() {
        let db = sql_open_memory();
        assert!(db > 0);

        let result = sql_execute(db, string_new("CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT)"));
        assert!(result >= 0);

        sql_close(db);
    }

    #[test]
    fn test_insert_and_query() {
        let db = sql_open_memory();
        assert!(db > 0);

        // Create table
        sql_execute(db, string_new("CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT, age INTEGER)"));

        // Insert data
        sql_execute(db, string_new("INSERT INTO users (name, age) VALUES ('Alice', 30)"));
        sql_execute(db, string_new("INSERT INTO users (name, age) VALUES ('Bob', 25)"));

        // Query
        let result = sql_query(db, string_new("SELECT * FROM users ORDER BY id"));
        assert!(result > 0);

        assert_eq!(sql_row_count(result), 2);
        assert_eq!(sql_column_count(result), 3);

        // First row
        assert_eq!(sql_next(result), 1);
        assert_eq!(sql_get_int(result, 0), 1);
        let name = sql_get_string(result, 1);
        assert_eq!(unsafe { string_as_str(name) }.unwrap(), "Alice");
        assert_eq!(sql_get_int(result, 2), 30);

        // Second row
        assert_eq!(sql_next(result), 1);
        assert_eq!(sql_get_int(result, 0), 2);
        let name = sql_get_string(result, 1);
        assert_eq!(unsafe { string_as_str(name) }.unwrap(), "Bob");
        assert_eq!(sql_get_int(result, 2), 25);

        // No more rows
        assert_eq!(sql_next(result), 0);

        sql_free_result(result);
        sql_close(db);
    }

    #[test]
    fn test_parameterized_query() {
        let db = sql_open_memory();
        sql_execute(db, string_new("CREATE TABLE items (name TEXT)"));
        sql_execute_1(db, string_new("INSERT INTO items VALUES (?)"), string_new("test"));

        let result = sql_query_1(db, string_new("SELECT * FROM items WHERE name = ?"), string_new("test"));
        assert_eq!(sql_row_count(result), 1);

        sql_free_result(result);
        sql_close(db);
    }

    #[test]
    fn test_last_insert_id() {
        let db = sql_open_memory();
        sql_execute(db, string_new("CREATE TABLE t (id INTEGER PRIMARY KEY)"));
        sql_execute(db, string_new("INSERT INTO t VALUES (NULL)"));

        let id = sql_last_insert_id(db);
        assert_eq!(id, 1);

        sql_execute(db, string_new("INSERT INTO t VALUES (NULL)"));
        let id = sql_last_insert_id(db);
        assert_eq!(id, 2);

        sql_close(db);
    }

    #[test]
    fn test_escape() {
        let input = string_new("O'Brien");
        let escaped = sql_escape(input);
        assert_eq!(unsafe { string_as_str(escaped) }.unwrap(), "O''Brien");
    }

    #[test]
    fn test_null_handling() {
        let db = sql_open_memory();
        sql_execute(db, string_new("CREATE TABLE t (a TEXT, b INTEGER)"));
        sql_execute(db, string_new("INSERT INTO t VALUES (NULL, 42)"));

        let result = sql_query(db, string_new("SELECT * FROM t"));
        sql_next(result);

        assert_eq!(sql_is_null(result, 0), 1);
        assert_eq!(sql_is_null(result, 1), 0);

        sql_free_result(result);
        sql_close(db);
    }
}
