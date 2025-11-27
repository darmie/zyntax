import haxe.ds.StringMap;
import haxe.ds.IntMap;

class MapTest {
    public static function main():Void {
        // Test StringMap
        var strMap = new StringMap<Int>();
        strMap.set("one", 1);
        strMap.set("two", 2);
        strMap.set("three", 3);

        var val1 = strMap.get("one");
        var val2 = strMap.get("two");

        // Test IntMap
        var intMap = new IntMap<String>();
        intMap.set(1, "one");
        intMap.set(2, "two");

        var str1 = intMap.get(1);
        var str2 = intMap.get(2);

        // Test exists
        var hasOne = strMap.exists("one");
        var hasFour = strMap.exists("four");

        trace("StringMap test:");
        trace("  get('one') = " + val1);
        trace("  get('two') = " + val2);
        trace("IntMap test:");
        trace("  get(1) = " + str1);
        trace("  get(2) = " + str2);
    }
}
