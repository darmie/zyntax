package zyntax.runtime;

/**
 * Extern definitions for Zyntax print functions
 */
@:native("print_i32")
extern class Zyntax {
    public static function print_i32(value: Int): Void;
    public static function println_i32(value: Int): Void;
    public static function print_f64(value: Float): Void;
    public static function println_f64(value: Float): Void;
    public static function print_bool(value: Bool): Void;
    public static function println_bool(value: Bool): Void;
}
