# zrtl_xml

XML parsing and generation for templating and data exchange.

## Functions

### Parsing
- `$Xml$parse(xml: String) -> Handle` - Parse XML string
- `$Xml$parse_file(path: String) -> Handle` - Parse XML from file
- `$Xml$free(handle: Handle)` - Free XML document

### Navigation
- `$Xml$root(doc: Handle) -> Handle` - Get root element
- `$Xml$tag_name(doc: Handle, elem: Handle) -> String` - Get tag name
- `$Xml$text(doc: Handle, elem: Handle) -> String` - Get text content
- `$Xml$attribute(doc: Handle, elem: Handle, name: String) -> String` - Get attribute
- `$Xml$has_attribute(doc: Handle, elem: Handle, name: String) -> i32` - Check attribute
- `$Xml$attributes(doc: Handle, elem: Handle) -> Array[String]` - Get all attribute names
- `$Xml$child_count(doc: Handle, elem: Handle) -> i64` - Get number of children
- `$Xml$children(doc: Handle, elem: Handle) -> Array[Handle]` - Get child elements
- `$Xml$child_at(doc: Handle, elem: Handle, index: i64) -> Handle` - Get child by index
- `$Xml$child(doc: Handle, elem: Handle, tag: String) -> Handle` - Get first child with tag
- `$Xml$find(doc: Handle, tag: String) -> Array[Handle]` - Find all elements with tag

### Building
- `$Xml$create() -> Handle` - Create new document
- `$Xml$element(doc: Handle, tag: String) -> Handle` - Create element
- `$Xml$set_root(doc: Handle, elem: Handle)` - Set root element
- `$Xml$set_text(doc: Handle, elem: Handle, text: String)` - Set text content
- `$Xml$set_attribute(doc: Handle, elem: Handle, name: String, value: String)` - Set attribute
- `$Xml$remove_attribute(doc: Handle, elem: Handle, name: String)` - Remove attribute
- `$Xml$append_child(doc: Handle, parent: Handle, child: Handle)` - Append child

### Output
- `$Xml$to_string(doc: Handle) -> String` - Convert to XML string
- `$Xml$to_string_pretty(doc: Handle, indent: i32) -> String` - Pretty-print XML
- `$Xml$save(doc: Handle, path: String) -> i32` - Save to file

## Example

```
// Parse and navigate XML
let doc = $Xml$parse("<root><item id=\"1\">Hello</item></root>");
let root = $Xml$root(doc);
let item = $Xml$child(doc, root, "item");
let id = $Xml$attribute(doc, item, "id");
let text = $Xml$text(doc, item);
$Xml$free(doc);

// Build XML
let doc = $Xml$create();
let root = $Xml$element(doc, "root");
$Xml$set_root(doc, root);
let child = $Xml$element(doc, "child");
$Xml$set_text(doc, child, "Hello World");
$Xml$append_child(doc, root, child);
let xml = $Xml$to_string_pretty(doc, 2);
$Xml$free(doc);
```

## Dependencies

- `quick-xml` crate (0.31)
