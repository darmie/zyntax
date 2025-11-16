package zyntax.runtime;

/**
 * Abstract wrapper for Array that provides operator overloading
 * and method mappings for Zyntax runtime integration.
 *
 * This abstract ensures array operations are properly mapped to
 * Zyntax runtime calls with correct pointer handling for
 * operations that may reallocate.
 */
@:coreType
abstract ZyntaxArray<T>(ZyntaxArrayPtr) {

    /**
     * Create new array
     * Maps to: $Array$new
     */
    public inline function new() {
        this = untyped __zyntax__("$Array$new");
    }

    /**
     * Array access operator (get)
     * Maps to: $Array$get
     */
    @:arrayAccess
    public inline function get(index: Int): T {
        return untyped __zyntax__("$Array$get", this, index);
    }

    /**
     * Array access operator (set)
     * Maps to: $Array$set
     */
    @:arrayAccess
    public inline function set(index: Int, value: T): T {
        untyped __zyntax__("$Array$set", this, index, value);
        return value;
    }

    /**
     * Get array length property
     * Maps to: $Array$length
     */
    public var length(get, never): Int;
    inline function get_length(): Int {
        return untyped __zyntax__("$Array$length", this);
    }

    /**
     * Push element to end of array (may reallocate)
     * Maps to: $Array$push
     */
    public inline function push(x: T): Int {
        // Update internal pointer since push may reallocate
        this = untyped __zyntax__("$Array$push", this, x);
        return this.length;
    }

    /**
     * Pop element from end of array
     * Maps to: $Array$pop
     */
    public inline function pop(): Null<T> {
        return untyped __zyntax__("$Array$pop", this);
    }

    /**
     * Remove and return first element
     * Maps to: $Array$shift
     */
    public inline function shift(): Null<T> {
        return untyped __zyntax__("$Array$shift", this);
    }

    /**
     * Insert element at beginning (may reallocate)
     * Maps to: $Array$unshift
     */
    public inline function unshift(x: T): Void {
        // Update internal pointer since unshift may reallocate
        this = untyped __zyntax__("$Array$unshift", this, x);
    }

    /**
     * Insert element at index (may reallocate)
     * Maps to: $Array$insert
     */
    public inline function insert(pos: Int, x: T): Void {
        // Update internal pointer since insert may reallocate
        this = untyped __zyntax__("$Array$insert", this, pos, x);
    }

    /**
     * Remove element at index
     * Maps to: $Array$remove
     */
    public inline function remove(x: T): Bool {
        var idx = indexOf(x);
        if (idx < 0) return false;
        untyped __zyntax__("$Array$remove", this, idx);
        return true;
    }

    /**
     * Find index of element
     * Maps to: $Array$indexOf
     */
    public inline function indexOf(x: T, ?fromIndex: Int): Int {
        if (fromIndex == null) fromIndex = 0;
        return untyped __zyntax__("$Array$indexOf", this, x);
    }

    /**
     * Check if array contains element
     * Maps to: $Array$contains
     */
    public inline function contains(x: T): Bool {
        return untyped __zyntax__("$Array$contains", this, x) != 0;
    }

    /**
     * Reverse array in place
     * Maps to: $Array$reverse
     */
    public inline function reverse(): Void {
        untyped __zyntax__("$Array$reverse", this);
    }

    /**
     * Create shallow copy of array
     * Maps to: $Array$copy
     */
    public inline function copy(): ZyntaxArray<T> {
        return cast untyped __zyntax__("$Array$copy", this);
    }

    /**
     * Convert from standard Haxe array
     */
    @:from
    public static inline function fromArray<T>(arr: Array<T>): ZyntaxArray<T> {
        // Create new ZyntaxArray and copy elements
        var result = new ZyntaxArray<T>();
        for (elem in arr) {
            result.push(elem);
        }
        return result;
    }

    /**
     * Convert to standard Haxe array
     */
    @:to
    public inline function toArray(): Array<T> {
        var result = [];
        var len = this.length;
        for (i in 0...len) {
            result.push(get(i));
        }
        return result;
    }
}

/**
 * Internal type representing the array pointer
 */
@:coreType
private abstract ZyntaxArrayPtr(Dynamic) {}
