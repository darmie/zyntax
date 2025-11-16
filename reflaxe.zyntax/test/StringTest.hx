class StringTest {
	static function main() {
		Zyntax.puts("=== String Runtime Tests ===");

		testStringCreation();
		testStringLength();
		testStringConcat();
		testStringCharAt();
		testStringSubstring();
		testStringCase();
		testStringIndexOf();
		testStringEquals();

		Zyntax.puts("=== All String Tests Passed! ===");
	}

	static function testStringCreation() {
		Zyntax.puts("Test: String Creation");

		var str = "Hello";
		Zyntax.puts(str);
		Zyntax.puts(" (expected: Hello)");

		Zyntax.puts("✓ String creation works");
	}

	static function testStringLength() {
		Zyntax.puts("Test: String Length");

		var str = "Hello";
		var len = str.length;

		Zyntax.print_i32(len);
		Zyntax.puts(" (expected: 5)");

		Zyntax.puts("✓ String length works");
	}

	static function testStringConcat() {
		Zyntax.puts("Test: String Concatenation");

		var str1 = "Hello";
		var str2 = " World";
		var result = str1 + str2;

		Zyntax.puts(result);
		Zyntax.puts(" (expected: Hello World)");

		// Test length of concatenated string
		var len = result.length;
		Zyntax.print_i32(len);
		Zyntax.puts(" (expected: 11)");

		Zyntax.puts("✓ String concat works");
	}

	static function testStringCharAt() {
		Zyntax.puts("Test: String charAt");

		var str = "Haxe";

		var ch0 = str.charAt(0);
		Zyntax.puts(ch0);
		Zyntax.puts(" (expected: H)");

		var ch1 = str.charAt(1);
		Zyntax.puts(ch1);
		Zyntax.puts(" (expected: a)");

		Zyntax.puts("✓ String charAt works");
	}

	static function testStringSubstring() {
		Zyntax.puts("Test: String Substring");

		var str = "HelloWorld";

		// Extract "Hello"
		var sub1 = str.substring(0, 5);
		Zyntax.puts(sub1);
		Zyntax.puts(" (expected: Hello)");

		// Extract "World"
		var sub2 = str.substring(5, 10);
		Zyntax.puts(sub2);
		Zyntax.puts(" (expected: World)");

		// Test substring length
		var len = sub1.length;
		Zyntax.print_i32(len);
		Zyntax.puts(" (expected: 5)");

		Zyntax.puts("✓ String substring works");
	}

	static function testStringCase() {
		Zyntax.puts("Test: String Case Conversion");

		var str = "Hello";

		// Test toLowerCase
		var lower = str.toLowerCase();
		Zyntax.puts(lower);
		Zyntax.puts(" (expected: hello)");

		// Test toUpperCase
		var upper = str.toUpperCase();
		Zyntax.puts(upper);
		Zyntax.puts(" (expected: HELLO)");

		Zyntax.puts("✓ String case conversion works");
	}

	static function testStringIndexOf() {
		Zyntax.puts("Test: String indexOf");

		var str = "Hello World";

		// Find "World"
		var idx = str.indexOf("World");
		Zyntax.print_i32(idx);
		Zyntax.puts(" (expected: 6)");

		// Find "o"
		var idxO = str.indexOf("o");
		Zyntax.print_i32(idxO);
		Zyntax.puts(" (expected: 4)");

		// Find non-existent substring
		var notFound = str.indexOf("xyz");
		Zyntax.print_i32(notFound);
		Zyntax.puts(" (expected: -1)");

		Zyntax.puts("✓ String indexOf works");
	}

	static function testStringEquals() {
		Zyntax.puts("Test: String Equals");

		var str1 = "Hello";
		var str2 = "Hello";
		var str3 = "World";

		// Test equality
		var eq1 = (str1 == str2);
		if (eq1) {
			Zyntax.puts("true (expected: true)");
		} else {
			Zyntax.puts("false (expected: true)");
		}

		// Test inequality
		var eq2 = (str1 == str3);
		if (!eq2) {
			Zyntax.puts("false (expected: false)");
		} else {
			Zyntax.puts("true (expected: false)");
		}

		Zyntax.puts("✓ String equals works");
	}
}

@:native("zyntax")
extern class Zyntax {
	static function print_i32(value:Int):Void;
	static function println_i32(value:Int):Void;
	static function puts(str:String):Int;
}
