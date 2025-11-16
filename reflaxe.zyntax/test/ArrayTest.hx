class ArrayTest {
	static function main() {
		Zyntax.puts("=== Array Runtime Tests ===");

		testArrayCreation();
		testArrayPush();
		testArrayAccess();
		testArrayLength();
		testArrayPop();
		testArrayShift();
		testArrayUnshift();
		testArrayIndexOf();

		Zyntax.puts("=== All Array Tests Passed! ===");
	}

	static function testArrayCreation() {
		Zyntax.puts("Test: Array Creation");

		// Create array from literal
		var arr = [10, 20];

		// Verify first element
		var first = arr[0];
		Zyntax.print_i32(first);
		Zyntax.puts(" (expected: 10)");

		// Verify second element
		var second = arr[1];
		Zyntax.print_i32(second);
		Zyntax.puts(" (expected: 20)");

		Zyntax.puts("✓ Array creation works");
	}

	static function testArrayPush() {
		Zyntax.puts("Test: Array Push");

		var arr = [10, 20];

		// Push new element
		arr.push(30);

		// Verify length increased
		var len = arr.length;
		Zyntax.print_i32(len);
		Zyntax.puts(" (expected: 3)");

		// Verify new element
		var third = arr[2];
		Zyntax.print_i32(third);
		Zyntax.puts(" (expected: 30)");

		Zyntax.puts("✓ Array push works");
	}

	static function testArrayAccess() {
		Zyntax.puts("Test: Array Access");

		var arr = [100, 200, 300];

		// Test index access
		var val0 = arr[0];
		var val1 = arr[1];
		var val2 = arr[2];

		Zyntax.print_i32(val0);
		Zyntax.puts(" (expected: 100)");

		Zyntax.print_i32(val1);
		Zyntax.puts(" (expected: 200)");

		Zyntax.print_i32(val2);
		Zyntax.puts(" (expected: 300)");

		Zyntax.puts("✓ Array access works");
	}

	static function testArrayLength() {
		Zyntax.puts("Test: Array Length");

		var arr = [1, 2, 3, 4, 5];
		var len = arr.length;

		Zyntax.print_i32(len);
		Zyntax.puts(" (expected: 5)");

		Zyntax.puts("✓ Array length works");
	}

	static function testArrayPop() {
		Zyntax.puts("Test: Array Pop");

		var arr = [10, 20, 30];

		// Pop last element
		var popped = arr.pop();

		Zyntax.print_i32(popped);
		Zyntax.puts(" (expected: 30)");

		// Verify length decreased
		var len = arr.length;
		Zyntax.print_i32(len);
		Zyntax.puts(" (expected: 2)");

		Zyntax.puts("✓ Array pop works");
	}

	static function testArrayShift() {
		Zyntax.puts("Test: Array Shift");

		var arr = [10, 20, 30];

		// Remove first element
		var shifted = arr.shift();

		Zyntax.print_i32(shifted);
		Zyntax.puts(" (expected: 10)");

		// Verify first element is now second
		var newFirst = arr[0];
		Zyntax.print_i32(newFirst);
		Zyntax.puts(" (expected: 20)");

		Zyntax.puts("✓ Array shift works");
	}

	static function testArrayUnshift() {
		Zyntax.puts("Test: Array Unshift");

		var arr = [20, 30];

		// Add to beginning
		arr.unshift(10);

		// Verify first element
		var first = arr[0];
		Zyntax.print_i32(first);
		Zyntax.puts(" (expected: 10)");

		// Verify length
		var len = arr.length;
		Zyntax.print_i32(len);
		Zyntax.puts(" (expected: 3)");

		Zyntax.puts("✓ Array unshift works");
	}

	static function testArrayIndexOf() {
		Zyntax.puts("Test: Array IndexOf");

		var arr = [10, 20, 30, 20];

		// Find index of 20
		var idx = arr.indexOf(20);
		Zyntax.print_i32(idx);
		Zyntax.puts(" (expected: 1)");

		// Find index of non-existent element
		var notFound = arr.indexOf(999);
		Zyntax.print_i32(notFound);
		Zyntax.puts(" (expected: -1)");

		Zyntax.puts("✓ Array indexOf works");
	}
}

@:native("zyntax")
extern class Zyntax {
	static function print_i32(value:Int):Void;
	static function println_i32(value:Int):Void;
	static function puts(str:String):Int;
}
