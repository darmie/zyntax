/*
 * ZRTL SDK Test Suite
 *
 * Demonstrates and tests the ZRTL SDK functionality including:
 * - String operations (canonical format)
 * - Array operations (canonical format)
 * - Iterator protocol
 * - Test harness macros
 *
 * Build and run:
 *   cd sdk/tests
 *   gcc -o test_zrtl test_zrtl.c && ./test_zrtl
 */

#include "../zrtl.h"

/* Initialize test state before test functions */
ZRTL_TEST_INIT();

/* ============================================================
 * String Tests
 * ============================================================ */

ZRTL_TEST(test_string_create) {
    ZrtlStringPtr str = zrtl_string_new("Hello");
    ZRTL_ASSERT_NOT_NULL(str);
    ZRTL_ASSERT_EQ(ZRTL_STRING_LENGTH(str), 5);

    /* Verify actual bytes */
    const char* data = ZRTL_STRING_DATA(str);
    ZRTL_ASSERT_EQ(data[0], 'H');
    ZRTL_ASSERT_EQ(data[1], 'e');
    ZRTL_ASSERT_EQ(data[2], 'l');
    ZRTL_ASSERT_EQ(data[3], 'l');
    ZRTL_ASSERT_EQ(data[4], 'o');

    zrtl_string_free(str);
    ZRTL_PASS();
}

ZRTL_TEST(test_string_empty) {
    ZrtlStringPtr str = zrtl_string_empty();
    ZRTL_ASSERT_NOT_NULL(str);
    ZRTL_ASSERT_EQ(ZRTL_STRING_LENGTH(str), 0);

    zrtl_string_free(str);
    ZRTL_PASS();
}

ZRTL_TEST(test_string_from_bytes) {
    const char bytes[] = { 'A', 'B', 'C' };
    ZrtlStringPtr str = zrtl_string_from_bytes(bytes, 3);
    ZRTL_ASSERT_NOT_NULL(str);
    ZRTL_ASSERT_EQ(ZRTL_STRING_LENGTH(str), 3);

    const char* data = ZRTL_STRING_DATA(str);
    ZRTL_ASSERT_EQ(data[0], 'A');
    ZRTL_ASSERT_EQ(data[1], 'B');
    ZRTL_ASSERT_EQ(data[2], 'C');

    zrtl_string_free(str);
    ZRTL_PASS();
}

ZRTL_TEST(test_string_copy) {
    ZrtlStringPtr original = zrtl_string_new("Test");
    ZRTL_ASSERT_NOT_NULL(original);

    ZrtlStringPtr copy = zrtl_string_copy(original);
    ZRTL_ASSERT_NOT_NULL(copy);
    ZRTL_ASSERT_NE(original, copy);  /* Different pointers */
    ZRTL_ASSERT_EQ(ZRTL_STRING_LENGTH(copy), ZRTL_STRING_LENGTH(original));

    /* Contents should be equal */
    ZRTL_ASSERT(zrtl_string_equals(original, copy));

    zrtl_string_free(original);
    zrtl_string_free(copy);
    ZRTL_PASS();
}

ZRTL_TEST(test_string_equals) {
    ZrtlStringPtr a = zrtl_string_new("Hello");
    ZrtlStringPtr b = zrtl_string_new("Hello");
    ZrtlStringPtr c = zrtl_string_new("World");
    ZrtlStringPtr d = zrtl_string_new("Hell");

    ZRTL_ASSERT(zrtl_string_equals(a, b));   /* Same content */
    ZRTL_ASSERT(!zrtl_string_equals(a, c));  /* Different content */
    ZRTL_ASSERT(!zrtl_string_equals(a, d));  /* Different length */

    zrtl_string_free(a);
    zrtl_string_free(b);
    zrtl_string_free(c);
    zrtl_string_free(d);
    ZRTL_PASS();
}

ZRTL_TEST(test_string_view) {
    ZrtlStringPtr str = zrtl_string_new("Test string");
    ZrtlStringView view = zrtl_string_view(str);

    ZRTL_ASSERT_NOT_NULL(view.data);
    ZRTL_ASSERT_EQ(view.length, 11);
    ZRTL_ASSERT_EQ(view.data[0], 'T');

    zrtl_string_free(str);
    ZRTL_PASS();
}

/* ============================================================
 * Array Tests
 * ============================================================ */

ZRTL_TEST(test_array_create) {
    ZrtlArrayPtr arr = zrtl_array_new_i32(8);
    ZRTL_ASSERT_NOT_NULL(arr);
    ZRTL_ASSERT_EQ(ZRTL_ARRAY_LENGTH(arr), 0);
    ZRTL_ASSERT_GE(ZRTL_ARRAY_CAPACITY(arr), 8);

    zrtl_array_free(arr);
    ZRTL_PASS();
}

ZRTL_TEST(test_array_push) {
    ZrtlArrayPtr arr = zrtl_array_new_i32(4);
    ZRTL_ASSERT_NOT_NULL(arr);

    arr = zrtl_array_push_i32(arr, 10);
    ZRTL_ASSERT_NOT_NULL(arr);
    ZRTL_ASSERT_EQ(ZRTL_ARRAY_LENGTH(arr), 1);
    ZRTL_ASSERT_EQ(zrtl_array_get_i32(arr, 0), 10);

    arr = zrtl_array_push_i32(arr, 20);
    arr = zrtl_array_push_i32(arr, 30);
    ZRTL_ASSERT_EQ(ZRTL_ARRAY_LENGTH(arr), 3);
    ZRTL_ASSERT_EQ(zrtl_array_get_i32(arr, 1), 20);
    ZRTL_ASSERT_EQ(zrtl_array_get_i32(arr, 2), 30);

    zrtl_array_free(arr);
    ZRTL_PASS();
}

ZRTL_TEST(test_array_growth) {
    /* Start with small capacity to force growth */
    ZrtlArrayPtr arr = zrtl_array_new_i32(2);
    ZRTL_ASSERT_NOT_NULL(arr);

    /* Push many elements to trigger reallocation */
    for (int i = 0; i < 100; i++) {
        arr = zrtl_array_push_i32(arr, i * 10);
        ZRTL_ASSERT_NOT_NULL(arr);
    }

    ZRTL_ASSERT_EQ(ZRTL_ARRAY_LENGTH(arr), 100);

    /* Verify all values are correct */
    for (int i = 0; i < 100; i++) {
        ZRTL_ASSERT_EQ(zrtl_array_get_i32(arr, i), i * 10);
    }

    zrtl_array_free(arr);
    ZRTL_PASS();
}

ZRTL_TEST(test_array_bounds) {
    ZrtlArrayPtr arr = zrtl_array_new_i32(4);
    arr = zrtl_array_push_i32(arr, 42);

    /* Out of bounds access should return 0 */
    ZRTL_ASSERT_EQ(zrtl_array_get_i32(arr, 1), 0);
    ZRTL_ASSERT_EQ(zrtl_array_get_i32(arr, -1), 0);
    ZRTL_ASSERT_EQ(zrtl_array_get_i32(arr, 100), 0);

    zrtl_array_free(arr);
    ZRTL_PASS();
}

/* ============================================================
 * Iterator Tests
 * ============================================================ */

ZRTL_TEST(test_array_iterator) {
    ZrtlArrayPtr arr = zrtl_array_new_i32(4);
    arr = zrtl_array_push_i32(arr, 10);
    arr = zrtl_array_push_i32(arr, 20);
    arr = zrtl_array_push_i32(arr, 30);

    ZrtlArrayIterator it = zrtl_array_iterator(arr);

    ZRTL_ASSERT(zrtl_array_iterator_has_next(&it));
    ZRTL_ASSERT_EQ(zrtl_array_iterator_next_i32(&it), 10);

    ZRTL_ASSERT(zrtl_array_iterator_has_next(&it));
    ZRTL_ASSERT_EQ(zrtl_array_iterator_next_i32(&it), 20);

    ZRTL_ASSERT(zrtl_array_iterator_has_next(&it));
    ZRTL_ASSERT_EQ(zrtl_array_iterator_next_i32(&it), 30);

    ZRTL_ASSERT(!zrtl_array_iterator_has_next(&it));

    zrtl_array_free(arr);
    ZRTL_PASS();
}

ZRTL_TEST(test_array_iterator_reset) {
    ZrtlArrayPtr arr = zrtl_array_new_i32(4);
    arr = zrtl_array_push_i32(arr, 5);
    arr = zrtl_array_push_i32(arr, 10);

    ZrtlArrayIterator it = zrtl_array_iterator(arr);

    /* Consume all elements */
    zrtl_array_iterator_next_i32(&it);
    zrtl_array_iterator_next_i32(&it);
    ZRTL_ASSERT(!zrtl_array_iterator_has_next(&it));

    /* Reset and iterate again */
    zrtl_array_iterator_reset(&it);
    ZRTL_ASSERT(zrtl_array_iterator_has_next(&it));
    ZRTL_ASSERT_EQ(zrtl_array_iterator_next_i32(&it), 5);

    zrtl_array_free(arr);
    ZRTL_PASS();
}

ZRTL_TEST(test_string_iterator_bytes) {
    ZrtlStringPtr str = zrtl_string_new("ABC");
    ZrtlStringIterator it = zrtl_string_iterator(str);

    ZRTL_ASSERT(zrtl_string_iterator_has_next(&it));
    ZRTL_ASSERT_EQ(zrtl_string_iterator_next_byte(&it), 'A');

    ZRTL_ASSERT(zrtl_string_iterator_has_next(&it));
    ZRTL_ASSERT_EQ(zrtl_string_iterator_next_byte(&it), 'B');

    ZRTL_ASSERT(zrtl_string_iterator_has_next(&it));
    ZRTL_ASSERT_EQ(zrtl_string_iterator_next_byte(&it), 'C');

    ZRTL_ASSERT(!zrtl_string_iterator_has_next(&it));

    zrtl_string_free(str);
    ZRTL_PASS();
}

ZRTL_TEST(test_string_iterator_codepoints) {
    /* UTF-8 string: "Aé中" (A=1 byte, é=2 bytes, 中=3 bytes) */
    const char utf8[] = { 'A', 0xC3, 0xA9, 0xE4, 0xB8, 0xAD };
    ZrtlStringPtr str = zrtl_string_from_bytes(utf8, 6);
    ZrtlStringIterator it = zrtl_string_iterator(str);

    /* 'A' = U+0041 */
    ZRTL_ASSERT(zrtl_string_iterator_has_next(&it));
    ZRTL_ASSERT_EQ(zrtl_string_iterator_next_codepoint(&it), 0x41);

    /* 'é' = U+00E9 */
    ZRTL_ASSERT(zrtl_string_iterator_has_next(&it));
    ZRTL_ASSERT_EQ(zrtl_string_iterator_next_codepoint(&it), 0xE9);

    /* '中' = U+4E2D */
    ZRTL_ASSERT(zrtl_string_iterator_has_next(&it));
    ZRTL_ASSERT_EQ(zrtl_string_iterator_next_codepoint(&it), 0x4E2D);

    ZRTL_ASSERT(!zrtl_string_iterator_has_next(&it));

    zrtl_string_free(str);
    ZRTL_PASS();
}

/* ============================================================
 * DynamicBox Tests
 * ============================================================ */

ZRTL_TEST(test_box_i32) {
    int32_t value = 42;
    ZrtlDynamicBox box = zrtl_box_i32(&value);

    ZRTL_ASSERT_EQ(box.tag, ZRTL_TAG_I32);
    ZRTL_ASSERT_EQ(box.size, sizeof(int32_t));
    ZRTL_ASSERT_NOT_NULL(box.data);
    ZRTL_ASSERT_EQ(zrtl_box_as_i32(&box), 42);

    ZRTL_PASS();
}

ZRTL_TEST(test_box_f64) {
    double value = 3.14159;
    ZrtlDynamicBox box = zrtl_box_f64(&value);

    ZRTL_ASSERT_EQ(box.tag, ZRTL_TAG_F64);
    ZRTL_ASSERT_FLOAT_EQ(zrtl_box_as_f64(&box), 3.14159, 0.00001);

    ZRTL_PASS();
}

ZRTL_TEST(test_box_clone) {
    int32_t value = 100;
    ZrtlDynamicBox original = zrtl_box_i32(&value);

    /* Clone creates a heap copy */
    ZrtlDynamicBox clone = zrtl_box_clone(&original);

    ZRTL_ASSERT_NE(original.data, clone.data);  /* Different memory */
    ZRTL_ASSERT_EQ(zrtl_box_as_i32(&clone), 100);  /* Same value */
    ZRTL_ASSERT_NOT_NULL(clone.dropper);  /* Has destructor */

    zrtl_box_free(&clone);
    ZRTL_PASS();
}

/* ============================================================
 * Type Tag Tests
 * ============================================================ */

ZRTL_TEST(test_type_tags) {
    /* Verify tag construction and extraction */
    ZrtlTypeTag tag = ZRTL_MAKE_TAG(ZRTL_CAT_INT, ZRTL_PRIM_32, ZRTL_FLAG_NULLABLE);

    ZRTL_ASSERT_EQ(ZRTL_TAG_CATEGORY(tag), ZRTL_CAT_INT);
    ZRTL_ASSERT_EQ(ZRTL_TAG_TYPE_ID(tag), ZRTL_PRIM_32);
    ZRTL_ASSERT_EQ(ZRTL_TAG_FLAGS(tag), ZRTL_FLAG_NULLABLE);

    /* Verify pre-defined tags */
    ZRTL_ASSERT_EQ(ZRTL_TAG_CATEGORY(ZRTL_TAG_VOID), ZRTL_CAT_VOID);
    ZRTL_ASSERT_EQ(ZRTL_TAG_CATEGORY(ZRTL_TAG_BOOL), ZRTL_CAT_BOOL);
    ZRTL_ASSERT_EQ(ZRTL_TAG_CATEGORY(ZRTL_TAG_STRING), ZRTL_CAT_STRING);

    ZRTL_PASS();
}

/* ============================================================
 * Test Main
 * ============================================================ */

ZRTL_TEST_MAIN(
    /* String tests */
    ZRTL_RUN(test_string_create),
    ZRTL_RUN(test_string_empty),
    ZRTL_RUN(test_string_from_bytes),
    ZRTL_RUN(test_string_copy),
    ZRTL_RUN(test_string_equals),
    ZRTL_RUN(test_string_view),

    /* Array tests */
    ZRTL_RUN(test_array_create),
    ZRTL_RUN(test_array_push),
    ZRTL_RUN(test_array_growth),
    ZRTL_RUN(test_array_bounds),

    /* Iterator tests */
    ZRTL_RUN(test_array_iterator),
    ZRTL_RUN(test_array_iterator_reset),
    ZRTL_RUN(test_string_iterator_bytes),
    ZRTL_RUN(test_string_iterator_codepoints),

    /* Box tests */
    ZRTL_RUN(test_box_i32),
    ZRTL_RUN(test_box_f64),
    ZRTL_RUN(test_box_clone),
    ZRTL_RUN(test_type_tags)
)
