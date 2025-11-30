import haxe.ds.StringMap;
import haxe.ds.IntMap;
import Math;

class ImportTest {
    public static function main():Void {
        // Test Math functions
        var x = 16;
        var sqrtResult = Math.sqrt(x);

        // Test absolute value
        var negative = -42;
        var absResult = Math.abs(negative);

        // Test min/max
        var a = 10;
        var b = 20;
        var minVal = Math.min(a, b);
        var maxVal = Math.max(a, b);

        trace("sqrt(16) = " + sqrtResult);
        trace("abs(-42) = " + absResult);
        trace("min(10, 20) = " + minVal);
        trace("max(10, 20) = " + maxVal);
    }
}
