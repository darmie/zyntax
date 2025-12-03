# ZynML Missing Grammar Features - Implementation Plan

Based on the unified ML DSL design (`00-unified-ml-dsl.md`), this document tracks what syntax features still need to be implemented in the ZynML grammar.

## Currently Implemented ✅

- [x] Basic types (int, float, bool, string)
- [x] Structs with field definitions
- [x] Functions with parameters and return types
- [x] Generic type parameters
- [x] Trait definitions and impl blocks
- [x] Let bindings
- [x] If/else statements
- [x] While loops
- [x] For loops
- [x] Binary operators (+, -, *, /, ==, !=, <, >, etc.)
- [x] Pipe operator (|>)
- [x] Method calls (obj.method())
- [x] Field access (obj.field)
- [x] Array indexing
- [x] Import statements (basic)
- [x] Type aliases
- [x] Opaque types (@opaque annotation)
- [x] Comments (// and /* */)

## Missing Features - Prioritized

### Priority 1: Core Language Features (Essential for ML workflows)

#### 1. Module System
**Status**: Not implemented
**Design**: See lines 69 in design doc

```zynml
module recommendation_pipeline

// Nested modules
module ml.models {
    // ...
}
```

**Grammar additions needed**:
```pest
module_def = { "module" ~ module_path ~ module_body? }
module_path = { identifier ~ ("." ~ identifier)* }
module_body = { "{" ~ top_level_items ~ "}" }
```

**Complexity**: Medium - Needs namespace tracking in compiler
**Estimated time**: 4-6 hours

---

#### 2. Enhanced Import System
**Status**: Partial (basic import only)
**Design**: See lines 71-78

```zynml
import zynml.tensor as T
import zynml.vector as V
from zynml.image import resize, crop
```

**Grammar additions needed**:
```pest
import_stmt = { import_as | import_from | import_simple }
import_as = { "import" ~ module_path ~ "as" ~ identifier }
import_from = { "from" ~ module_path ~ "import" ~ import_items }
import_items = { identifier ~ ("," ~ identifier)* | "*" }
import_simple = { "import" ~ module_path }
```

**Complexity**: Low-Medium
**Estimated time**: 2-3 hours

---

#### 3. Pipeline Keyword and Syntax
**Status**: Not implemented
**Design**: See lines 148-236

```zynml
pipeline image_search(query: text, top_k: int = 10) -> list[{image, score: float}]:
    """Search images by text description."""
    let query_embedding = encoder.forward(query)
    // ...
    return results
```

**Grammar additions needed**:
```pest
pipeline_def = { "pipeline" ~ identifier ~ type_params? ~ "(" ~ fn_params? ~ ")" ~ ("->" ~ type_expr)? ~ ":" ~ pipeline_body }
pipeline_body = { string_literal? ~ block }  // Optional docstring, then block
```

**Complexity**: Low (syntactic sugar over functions)
**Estimated time**: 2-3 hours

---

#### 4. Match/Case Expressions
**Status**: Not implemented
**Design**: See lines 421-424

```zynml
match detection.class:
    case "person": process_person(detection)
    case "vehicle": process_vehicle(detection)
    case _: ignore()
```

**Grammar additions needed**:
```pest
match_expr = { "match" ~ expr ~ ":" ~ match_arms }
match_arms = { match_arm+ }
match_arm = { "case" ~ pattern ~ ":" ~ (block | expr) }
pattern = { literal_pattern | wildcard_pattern | binding_pattern | struct_pattern }
wildcard_pattern = { "_" }
binding_pattern = { identifier }
literal_pattern = { string_literal | number | "true" | "false" }
struct_pattern = { identifier ~ "{" ~ field_patterns ~ "}" }
```

**Complexity**: Medium-High (needs pattern matching in compiler)
**Estimated time**: 6-8 hours

---

#### 5. Try/Catch Error Handling
**Status**: Not implemented
**Design**: See lines 428-434

```zynml
try:
    let result = risky_operation()
catch ModelError as e:
    render error("Model failed: {e.message}")
catch IOError as e:
    render warning("IO error: {e.message}")
```

**Grammar additions needed**:
```pest
try_stmt = { "try" ~ ":" ~ block ~ catch_clauses }
catch_clauses = { catch_clause+ }
catch_clause = { "catch" ~ identifier ~ ("as" ~ identifier)? ~ ":" ~ block }
```

**Complexity**: High (needs exception type system and unwinding)
**Estimated time**: 8-12 hours

---

### Priority 2: ML-Specific Syntax (Important for DSL feel)

#### 6. Load Function with Type Casting
**Status**: Not implemented
**Design**: See lines 92-116

```zynml
let product_embeddings = load("products.npy") as tensor[100000, 384]
let photo = load("image.jpg") as image
let config = load("config.yaml") as dict
```

**Grammar additions needed**:
```pest
load_expr = { "load" ~ "(" ~ expr ~ ")" ~ ("as" ~ type_expr)? }
```

**Complexity**: Low (just syntax, loader dispatch in runtime)
**Estimated time**: 1-2 hours

---

#### 7. Stream Function
**Status**: Not implemented
**Design**: See lines 114-116

```zynml
let sensor_stream = stream("mqtt://sensors/+") as timeseries
let audio_input = stream("microphone") as audio
```

**Grammar additions needed**:
```pest
stream_expr = { "stream" ~ "(" ~ expr ~ ")" ~ ("as" ~ type_expr)? }
```

**Complexity**: Low (syntax only, runtime handles streaming)
**Estimated time**: 1 hour

---

#### 8. Model Function with Configuration Blocks
**Status**: Not implemented
**Design**: See lines 118-143

```zynml
let encoder = model("sentence-bert.onnx") {
    input: text -> tensor[512],
    output: Embedding
}
```

**Grammar additions needed**:
```pest
model_expr = { "model" ~ "(" ~ expr ~ ")" ~ config_block? }
config_block = { "{" ~ config_entries ~ "}" }
config_entries = { config_entry ~ ("," ~ config_entry)* ~ ","? }
config_entry = { identifier ~ ":" ~ expr }
```

**Complexity**: Low-Medium
**Estimated time**: 2-3 hours

---

#### 9. Compute Blocks with Kernel Annotations
**Status**: Not implemented
**Design**: See lines 256-349

```zynml
let result = compute(tensor) {
    @kernel elementwise
    for i in 0..len:
        out[i] = sin(x[i]) * exp(-x[i] * x[i])
}
```

**Grammar additions needed**:
```pest
compute_expr = { "compute" ~ "(" ~ expr_list ~ ")" ~ compute_modifiers? ~ compute_block }
compute_modifiers = { ("@" ~ identifier ~ "(" ~ expr ~ ")")* }
compute_block = { "{" ~ kernel_annotations? ~ statements ~ "}" }
kernel_annotations = { ("@" ~ identifier ~ kernel_args?)+ }
kernel_args = { identifier | "(" ~ expr_list ~ ")" }
```

**Complexity**: High (needs GPU code generation)
**Estimated time**: 12-16 hours (backend integration)

---

#### 10. Render Keyword and Blocks
**Status**: Not implemented
**Design**: See lines 351-402

```zynml
render photo
render photo { title: "Original Image", width: 400 }
render grid([photo, processed], cols=3) { titles: ["A", "B"] }
```

**Grammar additions needed**:
```pest
render_stmt = { "render" ~ expr ~ config_block? }
```

**Complexity**: Low (just captures expression for output)
**Estimated time**: 1-2 hours

---

### Priority 3: Advanced Features (Nice to have)

#### 11. List/Dict Comprehensions
**Status**: Not implemented
**Design**: See lines 160-163

```zynml
let results = [
    {image: load(image_paths[idx]), score: score}
    for idx, score in results
]
```

**Grammar additions needed**:
```pest
list_comprehension = { "[" ~ expr ~ "for" ~ pattern ~ ("," ~ pattern)* ~ "in" ~ expr ~ ("if" ~ expr)? ~ "]" }
dict_comprehension = { "{" ~ expr ~ ":" ~ expr ~ "for" ~ pattern ~ "in" ~ expr ~ ("if" ~ expr)? ~ "}" }
```

**Complexity**: Medium
**Estimated time**: 4-5 hours

---

#### 12. Parallel Execution Operator
**Status**: Not implemented
**Design**: See lines 247-251

```zynml
let (embeddings, metadata) = (
    texts |> encode_batch() &
    texts |> extract_metadata()
)
```

**Grammar additions needed**:
```pest
parallel_expr = { "(" ~ expr ~ ("&" ~ expr)+ ~ ")" }
```

**Complexity**: Medium (needs async runtime integration)
**Estimated time**: 6-8 hours

---

#### 13. Ternary Operator
**Status**: Not implemented
**Design**: See line 254

```zynml
let output = confidence > 0.9 ? high_conf_path() : low_conf_path()
```

**Grammar additions needed**:
```pest
ternary_expr = { expr ~ "?" ~ expr ~ ":" ~ expr }
```

**Complexity**: Low
**Estimated time**: 1 hour

---

#### 14. Decorators (@cache, @memoize, @config)
**Status**: Partial (@opaque exists)
**Design**: See lines 453-473

```zynml
@cache(ttl=1h)
pipeline expensive_embedding(text: text) -> Embedding:
    return encoder.forward(text)

@memoize
fn compute_statistics(data: tensor) -> stats:
    return S.describe(data)
```

**Grammar additions needed**:
```pest
decorator = { "@" ~ identifier ~ decorator_args? }
decorator_args = { "(" ~ expr_list ~ ")" }
decorated_def = { decorator+ ~ (fn_def | pipeline_def) }
```

**Complexity**: Medium
**Estimated time**: 3-4 hours

---

#### 15. Config Blocks (Top-level)
**Status**: Not implemented
**Design**: See lines 463-474

```zynml
config {
    device: "cpu",
    precision: "float32",
    batch_size: 32
}
```

**Grammar additions needed**:
```pest
config_def = { "config" ~ config_block }
// Reuse config_block from model_expr
```

**Complexity**: Low
**Estimated time**: 1 hour

---

#### 16. Yield Keyword
**Status**: Not implemented
**Design**: See line 282

```zynml
let total = compute(data) {
    @kernel reduce(+)
    for i in 0..len:
        yield data[i] * data[i]
}
```

**Grammar additions needed**:
```pest
yield_stmt = { "yield" ~ expr }
```

**Complexity**: Medium (needs generator support in compiler)
**Estimated time**: 5-6 hours

---

#### 17. Duration Literals
**Status**: Not implemented
**Design**: See lines 217, 453

```zynml
let window = 1h
@cache(ttl=1h)
stream |> chunk(duration=5s, overlap=1s)
```

**Grammar additions needed**:
```pest
duration_literal = { number ~ duration_unit }
duration_unit = { "ms" | "s" | "m" | "h" | "d" }
```

**Complexity**: Low
**Estimated time**: 1 hour

---

#### 18. Multiline Strings and Docstrings
**Status**: Not implemented
**Design**: See line 149

```zynml
pipeline image_search(...):
    """Search images by text description."""
    // ...
```

**Grammar additions needed**:
```pest
string_literal = { triple_quoted_string | single_quoted_string | double_quoted_string }
triple_quoted_string = { "\"\"\"" ~ (!"\"\"\"" ~ ANY)* ~ "\"\"\"" }
```

**Complexity**: Low
**Estimated time**: 1 hour

---

#### 19. Tuple Type and Expressions
**Status**: Not implemented
**Design**: See lines 512, 520

```zynml
type BBox = tuple[float, float, float, float]
let (embeddings, metadata) = results
```

**Grammar additions needed**:
```pest
tuple_type = { "tuple" ~ "[" ~ type_expr ~ ("," ~ type_expr)* ~ "]" }
tuple_expr = { "(" ~ expr ~ ("," ~ expr)+ ~ ")" }
tuple_pattern = { "(" ~ pattern ~ ("," ~ pattern)+ ~ ")" }
```

**Complexity**: Low-Medium
**Estimated time**: 2-3 hours

---

#### 20. Named Arguments
**Status**: Not implemented
**Design**: Throughout examples

```zynml
I.normalize(img, mean=[0.485, 0.456, 0.406], std=[0.229, 0.224, 0.225])
V.topk(scores, k=top_k)
```

**Grammar additions needed**:
```pest
call_expr = { primary ~ "(" ~ call_args? ~ ")" }
call_args = { call_arg ~ ("," ~ call_arg)* ~ ","? }
call_arg = { named_arg | positional_arg }
named_arg = { identifier ~ "=" ~ expr }
positional_arg = { expr }
```

**Complexity**: Medium
**Estimated time**: 3-4 hours

---

## Implementation Order Recommendation

### Phase 1: Core Language (Weeks 1-2)
1. Module system
2. Enhanced imports (as, from)
3. Match/case expressions
4. Ternary operator
5. Tuple types and expressions
6. Multiline strings/docstrings

### Phase 2: ML-Specific Syntax (Weeks 3-4)
7. Pipeline keyword
8. Load function with type casting
9. Stream function
10. Model function with config blocks
11. Render keyword
12. Named arguments

### Phase 3: Advanced Features (Weeks 5-6)
13. List/dict comprehensions
14. Decorators (@cache, @memoize)
15. Config blocks
16. Duration literals
17. Try/catch error handling

### Phase 4: Performance Features (Weeks 7-8)
18. Compute blocks with @kernel
19. Parallel execution operator (&)
20. Yield keyword

## Testing Strategy

For each feature:
1. Add grammar rules
2. Write unit tests in `integration_test.rs`
3. Add example files in `examples/`
4. Update documentation
5. Verify with real ML workflow

## Notes

- Some features (like compute blocks) require significant runtime/backend work beyond grammar
- Error handling (try/catch) needs exception types in type system
- Parallel execution needs async runtime integration
- Most features are syntactic sugar and can be incrementally added
