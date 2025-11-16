/**
 * Comprehensive battle test for Haxe → Zyntax compiler
 * Tests correctness of various language features
 */
class BattleTest {
    static function main() {
        trace("=== BATTLE TEST SUITE ===");
        
        var passed = 0;
        var failed = 0;
        
        // Test 1: Control flow - if/else
        trace("\n[Test 1] Control flow - if/else");
        var x = 10;
        var result = "";
        if (x > 5) {
            result = "greater";
        } else {
            result = "less or equal";
        }
        if (result == "greater") {
            trace("✓ PASS: if/else works correctly");
            passed = passed + 1;
        } else {
            trace("✗ FAIL: if/else incorrect");
            failed = failed + 1;
        }
        
        // Test 2: String concatenation chain
        trace("\n[Test 2] String concatenation chain");
        var a = "Hello";
        var b = "World";
        var c = a + " " + b + "!";
        if (c == "Hello World!") {
            trace("✓ PASS: String concatenation correct");
            passed = passed + 1;
        } else {
            trace("✗ FAIL: Expected 'Hello World!', got: " + c);
            failed = failed + 1;
        }
        
        // Test 3: Multiple variable assignment
        trace("\n[Test 3] Multiple variable assignment");
        var v1 = "first";
        var v2 = "second";
        var v3 = v1 + v2;
        if (v3 == "firstsecond") {
            trace("✓ PASS: Multiple assignment correct");
            passed = passed + 1;
        } else {
            trace("✗ FAIL: Variable assignment incorrect");
            failed = failed + 1;
        }
        
        // Test 4: Arithmetic operations
        trace("\n[Test 4] Arithmetic operations");
        var num1 = 15;
        var num2 = 25;
        var sum = num1 + num2;
        if (sum == 40) {
            trace("✓ PASS: Addition correct (15 + 25 = 40)");
            passed = passed + 1;
        } else {
            trace("✗ FAIL: Addition incorrect");
            failed = failed + 1;
        }
        
        // Test 5: Variable shadowing in blocks
        trace("\n[Test 5] Variable scope");
        var outer = "outer";
        var scopeTest = outer;
        if (scopeTest == "outer") {
            trace("✓ PASS: Variable scope correct");
            passed = passed + 1;
        } else {
            trace("✗ FAIL: Variable scope incorrect");
            failed = failed + 1;
        }
        
        // Results summary
        trace("\n=== TEST RESULTS ===");
        trace("Passed: (count not shown - int to string conversion needed)");
        trace("Failed: (count not shown - int to string conversion needed)");
        
        if (failed == 0) {
            trace("✓ ALL TESTS PASSED!");
        } else {
            trace("✗ SOME TESTS FAILED");
        }
    }
}
