class SimpleIfTrace {
    static function main() {
        trace("Before if");
        var x = 10;
        if (x > 5) {
            trace("Inside then branch");
        } else {
            trace("Inside else branch");
        }
        trace("After if");
    }
}
