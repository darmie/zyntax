class ControlFlowTest {
    static function main() {
        trace("[ControlFlow] === Testing All Control Flow Constructs ===");
        
        // Test 1: If-else
        trace("[ControlFlow] Test 1: If-else statement");
        var x = 10;
        var test1 = false;
        if (x > 5) {
            test1 = true;
        } else {
            test1 = false;
        }
        trace("[ControlFlow]   Result: If-else executed");
        
        // Test 2: If without else
        trace("[ControlFlow] Test 2: If without else");
        var test2 = false;
        if (x == 10) {
            test2 = true;
        }
        trace("[ControlFlow]   Result: If executed");
        
        // Test 3: While loop
        trace("[ControlFlow] Test 3: While loop (3 iterations)");
        var counter = 0;
        while (counter < 3) {
            counter = counter + 1;
        }
        trace("[ControlFlow]   Result: While loop completed");
        
        // Test 4: Block expressions
        trace("[ControlFlow] Test 4: Block expressions");
        var result = {
            var a = 5;
            var b = 10;
            a + b;
        };
        trace("[ControlFlow]   Result: Block expression evaluated");
        
        trace("[ControlFlow] === All Control Flow Tests Complete ===");
    }
}
