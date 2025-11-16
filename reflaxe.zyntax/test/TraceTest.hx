// Test trace functionality with string concatenation
class TraceTest {
    static function main() {
        var hello = "Hello";
        var world = "World";
        var message = hello + " " + world;
        
        trace(message);
        trace("Test complete!");
    }
}
