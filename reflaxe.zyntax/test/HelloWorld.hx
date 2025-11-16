class HelloWorld {
	static function main() {
		Zyntax.puts("Hello from Haxe!");

		// Test array operations
		var numbers = [10, 32];

		var x:Int = add(numbers[0], numbers[1]);
		Zyntax.println_i32(x);
		Zyntax.puts("Done!");
	}

	static function add(a:Int, b:Int):Int {
		return a + b;
	}
}

@:native("zyntax")
extern class Zyntax {
	static function println_i32(value:Int):Void;
	static function puts(str:String):Int;

	@:native("$Array$push")
	static function array_push(array:Array<Int>, element:Int):Array<Int>;
}
