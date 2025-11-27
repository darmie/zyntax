class SimpleWhileTest {
    public static function calculate():Int {
        // Simple while loop that returns a value
        var sum = 0;
        var i = 0;
        while (i < 5) {
            sum = sum + i;
            i = i + 1;
        }
        return sum;  // Should return 0+1+2+3+4 = 10
    }

    public static function main():Void {
        var result = calculate();
        // result is 10
    }
}
