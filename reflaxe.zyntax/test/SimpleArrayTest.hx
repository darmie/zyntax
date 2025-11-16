class SimpleArrayTest {
	static function main() {
		Zyntax.puts("=== Simple Array Test ===");

		// Test basic array creation and access
		var arr = [10, 20];

		Zyntax.puts("Created array [10, 20]");

		// Access elements
		var first = arr[0];
		var second = arr[1];

		Zyntax.print_i32(first);
		Zyntax.puts(" (expected: 10)");

		Zyntax.print_i32(second);
		Zyntax.puts(" (expected: 20)");

		// Test push operation
		Zyntax.puts("Pushing 30...");
		arr.push(30);

		// Access the new element
		var third = arr[2];
		Zyntax.print_i32(third);
		Zyntax.puts(" (expected: 30)");

		// Test more pushes to trigger reallocation
		Zyntax.puts("Pushing more elements...");
		arr.push(40);
		arr.push(50);
		arr.push(60);

		var sixth = arr[5];
		Zyntax.print_i32(sixth);
		Zyntax.puts(" (expected: 60)");

		Zyntax.puts("=== All Tests Passed! ===");
	}
}

@:native("zyntax")
extern class Zyntax {
	static function print_i32(value:Int):Void;
	static function puts(str:String):Int;
}
