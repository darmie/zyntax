// Test that string concatenation is transformed to $String$concat runtime call
// This is STANDARD Haxe code - no special imports or types needed!

class StringConcatRuntimeTest {
    static function main() {
        // Simple string concatenation
        var hello = "Hello";
        var world = "World";
        var message = hello + " " + world;  // Should become: $String$concat($String$concat(hello, " "), world)

        // Print using runtime function
        trace("Message: " + message);

        // Test with literals
        var greeting = "Hi" + " there";
        trace("Greeting: " + greeting);

        trace("=== Test Complete ===");
    }
}
