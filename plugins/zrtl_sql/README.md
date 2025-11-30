# zrtl_sql

SQLite database operations for Zyntax-based languages.

## Overview

Provides SQLite database access with connection management, parameterized queries, result set navigation, transactions, and utility functions.

## Exported Symbols

### Connection

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Sql$open` | `(StringPtr) -> i64` | Open database file (0 on error) |
| `$Sql$open_memory` | `() -> i64` | Open in-memory database |
| `$Sql$close` | `(i64) -> ()` | Close database connection |

### Execute (INSERT/UPDATE/DELETE)

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Sql$execute` | `(i64, StringPtr) -> i64` | Execute SQL, return rows affected |
| `$Sql$execute_1` | `(i64, StringPtr, StringPtr) -> i64` | Execute with 1 parameter |
| `$Sql$execute_2` | `(i64, StringPtr, StringPtr, StringPtr) -> i64` | Execute with 2 parameters |
| `$Sql$execute_3` | `(i64, StringPtr, StringPtr, StringPtr, StringPtr) -> i64` | Execute with 3 parameters |

### Query (SELECT)

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Sql$query` | `(i64, StringPtr) -> i64` | Execute SELECT, return result set |
| `$Sql$query_1` | `(i64, StringPtr, StringPtr) -> i64` | Query with 1 parameter |

### Result Set Navigation

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Sql$next` | `(i64) -> i32` | Move to next row (1=ok, 0=done) |
| `$Sql$reset` | `(i64) -> ()` | Reset to beginning |
| `$Sql$row_count` | `(i64) -> i64` | Get total row count |
| `$Sql$column_count` | `(i64) -> i32` | Get column count |
| `$Sql$column_name` | `(i64, i32) -> StringPtr` | Get column name by index |

### Get Column Values

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Sql$get_int` | `(i64, i32) -> i64` | Get integer value |
| `$Sql$get_float` | `(i64, i32) -> f64` | Get float value |
| `$Sql$get_string` | `(i64, i32) -> StringPtr` | Get string value |
| `$Sql$get_blob` | `(i64, i32) -> ArrayPtr` | Get blob as byte array |
| `$Sql$is_null` | `(i64, i32) -> i32` | Check if value is NULL |
| `$Sql$free_result` | `(i64) -> ()` | Free result set |

### Transactions

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Sql$begin` | `(i64) -> i32` | Begin transaction |
| `$Sql$commit` | `(i64) -> i32` | Commit transaction |
| `$Sql$rollback` | `(i64) -> i32` | Rollback transaction |

### Utility

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Sql$last_insert_id` | `(i64) -> i64` | Get last inserted rowid |
| `$Sql$error` | `(i64) -> StringPtr` | Get last error message |
| `$Sql$escape` | `(StringPtr) -> StringPtr` | Escape string (prefer params!) |

## Usage Example

### Basic CRUD

```zig
// Open database
const db = $Sql$open("app.db");
if (db == 0) {
    // Failed to open
    return;
}

// Create table
$Sql$execute(db, "CREATE TABLE IF NOT EXISTS users (id INTEGER PRIMARY KEY, name TEXT, age INTEGER)");

// Insert with parameters (safe from SQL injection)
$Sql$execute_2(db, "INSERT INTO users (name, age) VALUES (?, ?)", "Alice", "30");
$Sql$execute_2(db, "INSERT INTO users (name, age) VALUES (?, ?)", "Bob", "25");

// Get last insert ID
const id = $Sql$last_insert_id(db);

// Query all users
const result = $Sql$query(db, "SELECT * FROM users ORDER BY id");
if (result != 0) {
    while ($Sql$next(result) == 1) {
        const id = $Sql$get_int(result, 0);
        const name = $Sql$get_string(result, 1);
        const age = $Sql$get_int(result, 2);

        $IO$print("User: ");
        $IO$println(name);
        string_free(name);
    }
    $Sql$free_result(result);
}

// Close database
$Sql$close(db);
```

### Parameterized Queries

```zig
// Search with parameter
const result = $Sql$query_1(db, "SELECT * FROM users WHERE name = ?", "Alice");

// Update with parameters
$Sql$execute_2(db, "UPDATE users SET age = ? WHERE name = ?", "31", "Alice");

// Delete with parameter
$Sql$execute_1(db, "DELETE FROM users WHERE name = ?", "Bob");
```

### Transactions

```zig
// Begin transaction
$Sql$begin(db);

// Multiple operations
$Sql$execute_1(db, "INSERT INTO accounts (balance) VALUES (?)", "1000");
$Sql$execute_1(db, "INSERT INTO accounts (balance) VALUES (?)", "2000");

// Commit or rollback
if (success) {
    $Sql$commit(db);
} else {
    $Sql$rollback(db);
}
```

### In-Memory Database

```zig
// Great for testing or temporary data
const db = $Sql$open_memory();

// Use like a regular database
$Sql$execute(db, "CREATE TABLE temp (value TEXT)");
// ...

$Sql$close(db);  // Data is lost
```

### Handling NULL

```zig
const result = $Sql$query(db, "SELECT nullable_column FROM table");
while ($Sql$next(result) == 1) {
    if ($Sql$is_null(result, 0) == 1) {
        // Value is NULL
    } else {
        const value = $Sql$get_string(result, 0);
        // ...
        string_free(value);
    }
}
```

## Result Set Lifecycle

1. `$Sql$query` returns a result set handle
2. Call `$Sql$next` to move to first/next row (returns 1 while rows exist)
3. Use `$Sql$get_*` functions to read column values
4. Column indices are 0-based
5. Call `$Sql$free_result` when done

## Return Values

- `$Sql$open` / `$Sql$open_memory`: Database handle (0 on error)
- `$Sql$execute`: Rows affected (-1 on error)
- `$Sql$query`: Result set handle (0 on error)
- `$Sql$next`: 1 if row available, 0 if done

## Memory Management

- Result sets must be freed with `$Sql$free_result`
- Strings from `$Sql$get_string` / `$Sql$column_name` must be freed
- Database connections should be closed with `$Sql$close`

## Dependencies

- `zrtl` - Core ZRTL SDK
- `rusqlite` - SQLite bindings
- `lazy_static` - Handle storage
