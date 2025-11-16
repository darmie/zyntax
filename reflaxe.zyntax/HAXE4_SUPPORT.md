# Haxe 4 Full Support Roadmap

## Current Status

### ✅ Working Features

1. **Basic Types**
   - ✅ Int (i32)
   - ✅ Float (f64)
   - ✅ Bool
   - ✅ String (basic)
   - ✅ Array\<Int\> (with runtime support)

2. **Expressions**
   - ✅ Literals (int, float, string, bool)
   - ✅ Variables (TLocal)
   - ✅ Binary operations (+, -, *, /, ==, !=, <, >, etc.)
   - ✅ Function calls (TCall)
   - ✅ Array access (TArray)
   - ✅ Array literals (TArrayDecl)
   - ✅ Field access (TField)

3. **Control Flow**
   - ✅ If/else (TIf)
   - ✅ While loops (TWhile)
   - ✅ Block expressions (TBlock)
   - ✅ Return statements (TReturn)
   - ✅ Variable declarations (TVar)

4. **Functions**
   - ✅ Static functions
   - ✅ Function parameters
   - ✅ Return values
   - ✅ Extern functions (@:native)

5. **Runtime Support**
   - ✅ Array: create, get, push, pop, shift, unshift, indexOf, etc. (20+ functions)
   - ✅ String: concat, charAt, substring, indexOf, etc. (25+ functions - needs compiler integration)
   - ✅ Print functions (print_i32, println_i32, puts)

### ⚠️ Partially Implemented

1. **String Operations**
   - ⚠️ String literals work but concatenation needs mapping
   - ⚠️ String methods exist in runtime but need compiler calls
   - ⚠️ Need property → method call transformation

2. **Array Operations**
   - ⚠️ Array methods (pop, shift, etc.) exist but need compiler mapping
   - ⚠️ `.length` property needs transformation to function call

### ❌ Missing Features

#### Core Language Features

1. **For Loops**
   - ❌ `for (i in 0...10)` - Range iteration
   - ❌ `for (item in array)` - Iterator pattern
   - ❌ `for (key => value in map)` - Key-value iteration

2. **Switch/Match**
   - ❌ Switch statements
   - ❌ Pattern matching
   - ❌ Enum matching

3. **Try/Catch**
   - ❌ Exception handling
   - ❌ Try/catch/finally blocks

4. **Classes & Objects**
   - ❌ Class instantiation (`new MyClass()`)
   - ❌ Instance methods
   - ❌ Instance fields
   - ❌ Constructors
   - ❌ `this` keyword
   - ❌ Inheritance
   - ❌ Method overriding

5. **Interfaces & Abstract Types**
   - ❌ Interface implementation
   - ❌ Abstract type conversions
   - ❌ Type parameters/generics

6. **Enums**
   - ❌ Enum declaration
   - ❌ Enum constructors
   - ❌ Enum value extraction

#### Standard Library Types

1. **Int64**
   - ❌ 64-bit integer support
   - ❌ Int64 arithmetic
   - ❌ Int64 conversions

2. **Map/Dictionary**
   - ❌ Map\<K, V\> implementation
   - ❌ Hash table runtime
   - ❌ get/set/exists/remove operations

3. **Null Safety**
   - ❌ Null\<T\> type
   - ❌ Null coalescing (`??`)
   - ❌ Null-safe access (`?.`)

4. **Dynamic**
   - ❌ Dynamic type support
   - ❌ Reflection

5. **Lambda/Closures**
   - ❌ Anonymous functions
   - ❌ Closures with capture
   - ❌ Function types

6. **Type Conversions**
   - ❌ Std.int()
   - ❌ Std.string()
   - ❌ Std.parseInt()
   - ❌ Std.parseFloat()
   - ❌ Type casting

#### Advanced Features

1. **Metadata**
   - ✅ @:native (partially)
   - ❌ @:keep
   - ❌ @:inline
   - ❌ Custom metadata

2. **Macros**
   - ❌ Build macros
   - ❌ Expression macros

3. **Module-level Code**
   - ❌ Static initialization
   - ❌ Module-level variables

## Priority Roadmap

### Phase 1: Complete Core Runtime (CURRENT)

**Goal**: Full Array & String support working end-to-end

**Tasks**:
- ✅ Implement Array runtime functions
- ✅ Implement String runtime functions
- ⚠️ Update compiler to map string operations to runtime calls
- ⚠️ Update compiler to map array property access to function calls
- ⚠️ Test full array/string functionality

**Deliverables**:
- Working array operations with .length, .pop(), etc.
- Working string concatenation and methods
- Comprehensive test suite

### Phase 2: Control Flow & Loops

**Goal**: Support all Haxe control flow constructs

**Tasks**:
- [ ] Implement `for (i in 0...n)` range loops
- [ ] Implement `for (item in array)` iterator loops
- [ ] Implement switch/case statements
- [ ] Add enum support for switch matching
- [ ] Implement try/catch/finally

**Deliverables**:
- Loop test suite
- Switch statement tests
- Exception handling tests

### Phase 3: Classes & Objects

**Goal**: Object-oriented programming support

**Tasks**:
- [ ] Implement class instantiation
- [ ] Add constructor support
- [ ] Implement instance methods
- [ ] Implement instance fields
- [ ] Add `this` keyword support
- [ ] Implement inheritance
- [ ] Add super calls
- [ ] Implement method overriding

**Deliverables**:
- Class-based test programs
- OOP examples (shapes, animals, etc.)

### Phase 4: Standard Library Types

**Goal**: Essential Haxe std types

**Tasks**:
- [ ] Implement Map\<K, V\> with hash table runtime
- [ ] Add Int64 support
- [ ] Implement Null\<T\> type
- [ ] Add Std class (int, string, parseInt, etc.)
- [ ] Implement StringBuf for mutable strings

**Deliverables**:
- Map/Dictionary operations
- Type conversion utilities
- Null-safe code examples

### Phase 5: Advanced Features

**Goal**: Closures, generics, and advanced typing

**Tasks**:
- [ ] Implement anonymous functions
- [ ] Add closure capture support
- [ ] Implement generic type parameters
- [ ] Add interface support
- [ ] Implement abstract types

**Deliverables**:
- Functional programming examples
- Generic data structures
- Interface-based designs

## Immediate Next Steps

### 1. Complete String Integration (Week 1)

**Compiler Changes Needed** (in `ZyntaxCompiler.hx`):

```haxe
// String concatenation
case TBinop(OpAdd, e1, e2):
    if (isStringType(e1.t) || isStringType(e2.t)) {
        // Generate call to $String$concat
        return TCall(
            TIdent("$String$concat"),
            [compileExpression(e1), compileExpression(e2)]
        );
    }

// String.length property
case TField(obj, FInstance(_, _, {name: "length"})):
    if (isStringType(obj.t)) {
        // Generate call to $String$length
        return TCall(
            TIdent("$String$length"),
            [compileExpression(obj)]
        );
    }

// String methods
case TCall(TField(obj, FInstance(_, _, {name: methodName})), args):
    if (isStringType(obj.t)) {
        switch (methodName) {
            case "charAt": return mapToRuntime("$String$charAt", obj, args);
            case "substring": return mapToRuntime("$String$substring", obj, args);
            case "indexOf": return mapToRuntime("$String$indexOf", obj, args);
            // ... etc
        }
    }
```

**Tests**:
- [ ] String concatenation test
- [ ] String method test (.charAt, .substring, etc.)
- [ ] String comparison test

### 2. Complete Array Integration (Week 1)

**Compiler Changes Needed**:

```haxe
// Array.length property
case TField(obj, FInstance(_, _, {name: "length"})):
    if (isArrayType(obj.t)) {
        return TCall(TIdent("$Array$length"), [compileExpression(obj)]);
    }

// Array methods
case TCall(TField(obj, FInstance(_, _, {name: methodName})), args):
    if (isArrayType(obj.t)) {
        switch (methodName) {
            case "push": return mapToRuntime("$Array$push", obj, args);
            case "pop": return mapToRuntime("$Array$pop", obj, args);
            case "shift": return mapToRuntime("$Array$shift", obj, args);
            // ... etc
        }
    }
```

**Tests**:
- [x] Array creation and access (DONE)
- [ ] Array.length property
- [ ] Array.push/pop/shift/unshift
- [ ] Array.indexOf/contains

### 3. Implement For Loops (Week 2)

**Required**:
- Range iterator (`0...n`)
- Array iterator support
- Lowering to while loops in HIR

**Example**:
```haxe
// Haxe source
for (i in 0...10) {
    trace(i);
}

// Should become:
{
    var i = 0;
    while (i < 10) {
        trace(i);
        i = i + 1;
    }
}
```

### 4. Implement Map\<K, V\> (Week 2-3)

**Runtime Implementation**:
- Hash table with separate chaining
- Generic key/value types
- Operations: get, set, exists, remove, keys, values

**Functions Needed**:
```rust
$Map$new()
$Map$set(map, key, value)
$Map$get(map, key, default)
$Map$exists(map, key)
$Map$remove(map, key)
$Map$keys(map) -> Array
$Map$values(map) -> Array
```

### 5. Implement Std Class (Week 3)

**Type Conversions**:
```haxe
Std.int(value: Float): Int
Std.string(value: Dynamic): String
Std.parseInt(str: String): Null<Int>
Std.parseFloat(str: String): Float
```

**Runtime Functions**:
```rust
$Std$int(f64) -> i32
$Std$string_from_int(i32) -> String
$Std$string_from_float(f64) -> String
$Std$parseInt(String) -> Null<i32>
$Std$parseFloat(String) -> f64
```

## Testing Strategy

### Test Categories

1. **Unit Tests** - Individual runtime functions
2. **Integration Tests** - Full Haxe programs
3. **Benchmark Tests** - Performance validation
4. **Regression Tests** - Ensure fixes don't break

### Test Programs

1. **Basic**:
   - Hello World ✅
   - Arithmetic
   - Array operations ✅
   - String operations

2. **Intermediate**:
   - Fibonacci (recursion)
   - Sorting algorithms
   - Data structures (stack, queue)
   - String processing

3. **Advanced**:
   - JSON parser
   - Mini HTTP server
   - Game logic
   - Compiler/interpreter

## Success Criteria

### Minimum Viable Haxe 4 Support

- ✅ Basic types (Int, Float, Bool, String, Array)
- ⚠️ Full Array support with all methods
- ⚠️ Full String support with all methods
- ❌ For loops (range and iterator)
- ❌ Map\<K, V\>
- ❌ Std type conversions
- ❌ Basic class/object support

### Full Haxe 4 Support

All of the above plus:
- Inheritance & interfaces
- Enums & pattern matching
- Try/catch exception handling
- Closures & anonymous functions
- Generics
- Null safety

## Current Focus

**Week 1 Priority**: Complete Phase 1
1. Fix string concatenation compiler mapping
2. Fix array/string property access (`.length`)
3. Map all array methods to runtime calls
4. Map all string methods to runtime calls
5. Validate with comprehensive test suite

**Success Metric**: All ArrayTest.hx and StringTest.hx tests passing

Then we'll be ready to move forward with HMR and memory optimizations with a solid Haxe foundation!
