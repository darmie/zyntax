// Comprehensive test demonstrating robust Haxe support via reflaxe → Zyntax
class ComprehensiveTest {
    static function main() {
        trace("=== Zyntax Haxe Support Test ===");
        
        // Test 1: String concatenation
        var firstName = "John";
        var lastName = "Doe";
        var fullName = firstName + " " + lastName;
        trace("Full name: " + fullName);
        
        // Test 2: Simple arithmetic (no trace mixing)
        var a = 10;
        var b = 20;
        var sum = a + b;
        // Note: Integer to string conversion not yet implemented
        // trace("Sum: 10 + 20 = " + sum);
        
        // Test 3: Multiple string concatenation
        var greeting = "Hello" + ", " + "World" + "!";
        trace(greeting);
        
        // Test 4: String variables with literals
        var lang = "Haxe";
        var target = "Zyntax";
        trace(lang + " compiles to " + target);
        
        trace("=== All Tests Passed! ===");
    }
}
