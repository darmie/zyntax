/*
 * Example ZRTL Plugin: Hello Runtime
 *
 * This demonstrates how to create a simple ZRTL plugin in C.
 *
 * Build:
 *   Linux:   gcc -shared -fPIC -o hello_runtime.zrtl hello_runtime.c
 *   macOS:   clang -shared -fPIC -o hello_runtime.zrtl hello_runtime.c
 *   Windows: cl /LD hello_runtime.c /Fe:hello_runtime.zrtl
 *
 * Use:
 *   zyntax compile --runtime hello_runtime.zrtl --source input.hx
 */

#include "../zrtl.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* ============================================================
 * Runtime functions
 * ============================================================ */

/* Simple addition */
int32_t hello_add(int32_t a, int32_t b) {
    return a + b;
}

/* Print a message */
void hello_print(const char* msg) {
    printf("[HelloRuntime] %s\n", msg);
}

/* Print an integer */
void hello_print_int(int32_t value) {
    printf("[HelloRuntime] %d\n", value);
}

/* Create a greeting string */
const char* hello_greet(const char* name) {
    static char buffer[256];
    snprintf(buffer, sizeof(buffer), "Hello, %s!", name);
    return buffer;
}

/* Factorial (recursive) */
int32_t hello_factorial(int32_t n) {
    if (n <= 1) return 1;
    return n * hello_factorial(n - 1);
}

/* Fibonacci */
int32_t hello_fibonacci(int32_t n) {
    if (n <= 1) return n;
    int32_t a = 0, b = 1;
    for (int32_t i = 2; i <= n; i++) {
        int32_t tmp = a + b;
        a = b;
        b = tmp;
    }
    return b;
}

/* ============================================================
 * ZRTL Plugin Exports
 * ============================================================ */

/* Export symbol table */
ZRTL_SYMBOLS_BEGIN
    ZRTL_SYMBOL("$Hello$add", hello_add),
    ZRTL_SYMBOL("$Hello$print", hello_print),
    ZRTL_SYMBOL("$Hello$printInt", hello_print_int),
    ZRTL_SYMBOL("$Hello$greet", hello_greet),
    ZRTL_SYMBOL("$Hello$factorial", hello_factorial),
    ZRTL_SYMBOL("$Hello$fibonacci", hello_fibonacci),
ZRTL_SYMBOLS_END

/* Export plugin info */
ZRTL_PLUGIN_INFO("hello_runtime")
