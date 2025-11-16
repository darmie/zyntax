// Test with Haxe string print function
@:native("$String$println")
extern function println(s: String): Void;

class PrintTest {
    static function main() {
        var hello = "Hello";
        var world = "World";
        var message = hello + " " + world;
        
        println(message);
        println("Concatenation works!");
    }
}
