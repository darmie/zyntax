// Standard Haxe code - no special imports!
// The TypeReplacer macro will automatically transform String -> ZyntaxString

class AutoStringTest {
    static function main() {
        trace("=== Automatic String Transformation Test ===");

        // Standard Haxe string concatenation
        var greeting = "Hello";
        var target = "World";
        var message = greeting + " " + target;  // Should use ZyntaxString's + operator

        trace("Message: " + message);
        trace("Expected: Hello World");

        // String length property
        var len = message.length;
        trace("\nLength: " + len);
        trace("Expected: 11");

        // String methods
        var upper = message.toUpperCase();
        trace("\nUppercase: " + upper);
        trace("Expected: HELLO WORLD");

        var sub = message.substring(0, 5);
        trace("\nSubstring(0,5): " + sub);
        trace("Expected: Hello");

        trace("\n=== Test Complete ===");
    }
}
