class ForLoopTest {
    public static function main():Void {
        // Test 1: Simple while loop (which we know works)
        var sum = 0;
        var i = 0;
        while (i < 5) {
            sum = sum + i;
            i = i + 1;
        }

        // Test 2: For-in loop with IntIterator (0...5)
        var sum2 = 0;
        for (j in 0...5) {
            sum2 = sum2 + j;
        }

        // Print results
        trace("Sum from while: " + sum);
        trace("Sum from for: " + sum2);
    }
}
