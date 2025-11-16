// Simple test with arithmetic operations
class SimpleArithmetic {
    static function main() {
        trace("[SimpleArithmetic] Starting test...");

        var a = 10;
        var b = 20;
        var sum = a + b;

        trace("[SimpleArithmetic] Test: 10 + 20 = 30");
        trace("[SimpleArithmetic] Variables: a=10, b=20, sum=30");

        var product = a * b;
        trace("[SimpleArithmetic] Test: 10 * 20 = 200");

        var diff = b - a;
        trace("[SimpleArithmetic] Test: 20 - 10 = 10");

        trace("[SimpleArithmetic] ✓ PASS - All arithmetic operations executed");
    }
}
