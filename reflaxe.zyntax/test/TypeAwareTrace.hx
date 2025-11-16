// Test type-aware trace that prints actual values
class TypeAwareTrace {
    static function main() {
        trace("[TypeAwareTrace] Starting type-aware trace test...");

        // Test integers
        trace("[TypeAwareTrace] Testing integer trace:");
        var x = 42;
        trace(x);

        // Test floats
        trace("[TypeAwareTrace] Testing float trace:");
        var pi = 3.14159;
        trace(pi);

        // Test booleans
        trace("[TypeAwareTrace] Testing boolean trace:");
        var flag = true;
        trace(flag);
        trace(false);

        // Test strings
        trace("[TypeAwareTrace] Testing string trace:");
        trace("String literal test");

        // Test arithmetic results
        trace("[TypeAwareTrace] Testing arithmetic result trace:");
        var sum = 10 + 20;
        trace(sum);

        var product = 5 * 6;
        trace(product);

        trace("[TypeAwareTrace] ✓ PASS - All type-aware trace tests completed");
    }
}
