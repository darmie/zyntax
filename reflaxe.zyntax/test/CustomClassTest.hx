// Test with a custom class that has actual Haxe implementation
class Counter {
    public var value:Int;

    public function new() {
        value = 0;
    }

    public function increment():Void {
        value = value + 1;
    }

    public function getValue():Int {
        return value;
    }
}

class CustomClassTest {
    public static function main():Void {
        // Test custom class
        var counter = new Counter();
        counter.increment();
        counter.increment();
        counter.increment();
        var result = counter.getValue();
        trace("Counter value: " + result);
    }
}
