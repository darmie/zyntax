package zyntax.runtime;

/**
 * Abstract wrapper for String that provides operator overloading
 * for Zyntax runtime integration.
 *
 * This abstract can be used to intercept string operations and map them
 * to Zyntax runtime calls. The reflaxe.zyntax compiler should recognize
 * calls to ZyntaxStringExtern and lower them to direct runtime calls.
 */
abstract ZyntaxString(String) from String to String {

    /**
     * String concatenation operator
     * Maps to: $String$concat
     */
    @:op(A + B)
    public inline static function concat(a: ZyntaxString, b: ZyntaxString): ZyntaxString {
        return cast ZyntaxStringExtern.concat(cast a, cast b);
    }

    /**
     * String equality operator
     * Maps to: $String$equals
     */
    @:op(A == B)
    public inline static function equals(a: ZyntaxString, b: ZyntaxString): Bool {
        return ZyntaxStringExtern.equals(cast a, cast b);
    }

    /**
     * String inequality operator
     */
    @:op(A != B)
    public inline static function notEquals(a: ZyntaxString, b: ZyntaxString): Bool {
        return !equals(a, b);
    }

    /**
     * Get string length property
     * Maps to: $String$length
     */
    public var length(get, never): Int;
    inline function get_length(): Int {
        return ZyntaxStringExtern.length(cast this);
    }

    /**
     * Get character at index
     * Maps to: $String$charAt
     */
    public inline function charAt(index: Int): String {
        return cast ZyntaxStringExtern.charAt(cast this, index);
    }

    /**
     * Get character code at index
     * Maps to: $String$charCodeAt
     */
    public inline function charCodeAt(index: Int): Null<Int> {
        return ZyntaxStringExtern.charCodeAt(cast this, index);
    }

    /**
     * Get substring (from start to end, exclusive)
     * Maps to: $String$substring
     */
    public inline function substring(startIndex: Int, ?endIndex: Int): String {
        if (endIndex == null) endIndex = this.length;
        return cast ZyntaxStringExtern.substring(cast this, startIndex, endIndex);
    }

    /**
     * Get substring with length
     * Maps to: $String$substr
     */
    public inline function substr(pos: Int, ?len: Int): String {
        if (len == null) len = this.length - pos;
        return cast ZyntaxStringExtern.substr(cast this, pos, len);
    }

    /**
     * Convert to lowercase
     * Maps to: $String$toLowerCase
     */
    public inline function toLowerCase(): String {
        return cast ZyntaxStringExtern.toLowerCase(cast this);
    }

    /**
     * Convert to uppercase
     * Maps to: $String$toUpperCase
     */
    public inline function toUpperCase(): String {
        return cast ZyntaxStringExtern.toUpperCase(cast this);
    }

    /**
     * Find index of substring
     * Maps to: $String$indexOf
     */
    public inline function indexOf(str: String, ?startIndex: Int): Int {
        if (startIndex == null) startIndex = 0;
        return ZyntaxStringExtern.indexOf(cast this, cast str, startIndex);
    }

    /**
     * Find last index of substring
     * Maps to: $String$lastIndexOf
     */
    public inline function lastIndexOf(str: String, ?startIndex: Int): Int {
        if (startIndex == null) startIndex = -1;
        return ZyntaxStringExtern.lastIndexOf(cast this, cast str, startIndex);
    }
}

/**
 * Extern class that the reflaxe.zyntax compiler recognizes and lowers
 * to direct runtime calls. Each method here maps to a Zyntax runtime symbol.
 */
@:native("$String")
extern class ZyntaxStringExtern {
    @:native("$String$concat")
    public static function concat(a: Dynamic, b: Dynamic): Dynamic;

    @:native("$String$equals")
    public static function equals(a: Dynamic, b: Dynamic): Bool;

    @:native("$String$length")
    public static function length(s: Dynamic): Int;

    @:native("$String$charAt")
    public static function charAt(s: Dynamic, index: Int): Dynamic;

    @:native("$String$charCodeAt")
    public static function charCodeAt(s: Dynamic, index: Int): Int;

    @:native("$String$substring")
    public static function substring(s: Dynamic, start: Int, end: Int): Dynamic;

    @:native("$String$substr")
    public static function substr(s: Dynamic, pos: Int, len: Int): Dynamic;

    @:native("$String$toLowerCase")
    public static function toLowerCase(s: Dynamic): Dynamic;

    @:native("$String$toUpperCase")
    public static function toUpperCase(s: Dynamic): Dynamic;

    @:native("$String$indexOf")
    public static function indexOf(s: Dynamic, search: Dynamic, start: Int): Int;

    @:native("$String$lastIndexOf")
    public static function lastIndexOf(s: Dynamic, search: Dynamic, start: Int): Int;
}
