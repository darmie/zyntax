// Test trace with string concatenation involving integers
class StringConcatTrace {
    static function main() {
        trace("[StringConcatTrace] Starting test...");

        var x = 42;
        var pi = 3.14;
        var flag = true;

        // String concatenation with integer
        trace("x = " + x);

        // String concatenation with float
        trace("pi = " + pi);

        // String concatenation with boolean
        trace("flag = " + flag);

        // Multiple concatenations
        trace("Values: x=" + x + ", pi=" + pi + ", flag=" + flag);

        // Pure integer should still use Int$println
        trace(x);

        trace("[StringConcatTrace] ✓ PASS - All tests completed");
    }
}
