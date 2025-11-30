//! ZRTL XML Plugin
//!
//! XML parsing and generation for templating and data exchange.
//!
//! ## Parsing
//! - `$Xml$parse` - Parse XML string into document
//! - `$Xml$parse_file` - Parse XML from file
//! - `$Xml$free` - Free XML document
//!
//! ## Navigation
//! - `$Xml$root` - Get root element
//! - `$Xml$tag_name` - Get element tag name
//! - `$Xml$text` - Get element text content
//! - `$Xml$attribute` - Get attribute value
//! - `$Xml$attributes` - Get all attribute names
//! - `$Xml$children` - Get child elements
//! - `$Xml$child` - Get first child with tag name
//! - `$Xml$find` - Find elements by tag name (recursive)
//!
//! ## Building
//! - `$Xml$create` - Create new document
//! - `$Xml$element` - Create element
//! - `$Xml$set_text` - Set element text
//! - `$Xml$set_attribute` - Set attribute
//! - `$Xml$append_child` - Append child element
//!
//! ## Output
//! - `$Xml$to_string` - Convert to XML string
//! - `$Xml$to_string_pretty` - Convert with indentation
//! - `$Xml$save` - Save to file

use std::collections::HashMap;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Mutex, OnceLock};
use quick_xml::{
    Reader, Writer,
    events::{Event, BytesStart, BytesEnd, BytesText, BytesDecl},
};
use zrtl::{
    zrtl_plugin, StringPtr, ArrayPtr,
    string_new, string_as_str, string_free,
    array_new, array_push,
};

// ============================================================================
// Handle Management
// ============================================================================

static HANDLE_COUNTER: AtomicU64 = AtomicU64::new(1);
static DOCUMENTS: OnceLock<Mutex<HashMap<u64, XmlDocument>>> = OnceLock::new();

fn get_documents() -> &'static Mutex<HashMap<u64, XmlDocument>> {
    DOCUMENTS.get_or_init(|| Mutex::new(HashMap::new()))
}

fn next_handle() -> u64 {
    HANDLE_COUNTER.fetch_add(1, Ordering::SeqCst)
}

/// XML Element representation
#[derive(Clone, Debug)]
struct XmlElement {
    tag: String,
    attributes: HashMap<String, String>,
    text: String,
    children: Vec<u64>,  // Handles to child elements
}

/// XML Document
struct XmlDocument {
    root: Option<u64>,
    elements: HashMap<u64, XmlElement>,
}

// ============================================================================
// Parsing
// ============================================================================

/// Parse XML string into document
///
/// Returns document handle on success, 0 on error.
#[no_mangle]
pub extern "C" fn xml_parse(xml_str: StringPtr) -> u64 {
    let xml = match unsafe { string_as_str(xml_str) } {
        Some(s) => s,
        None => return 0,
    };

    parse_xml_string(xml)
}

/// Parse XML from file
#[no_mangle]
pub extern "C" fn xml_parse_file(path: StringPtr) -> u64 {
    let path_str = match unsafe { string_as_str(path) } {
        Some(s) => s,
        None => return 0,
    };

    match std::fs::read_to_string(path_str) {
        Ok(content) => parse_xml_string(&content),
        Err(_) => 0,
    }
}

fn parse_xml_string(xml: &str) -> u64 {
    let mut reader = Reader::from_str(xml);
    reader.trim_text(true);

    let doc_handle = next_handle();
    let mut doc = XmlDocument {
        root: None,
        elements: HashMap::new(),
    };

    let mut element_stack: Vec<u64> = Vec::new();
    let mut buf = Vec::new();

    loop {
        match reader.read_event_into(&mut buf) {
            Ok(Event::Start(ref e)) => {
                let elem_handle = next_handle();
                let mut elem = XmlElement {
                    tag: String::from_utf8_lossy(e.name().as_ref()).to_string(),
                    attributes: HashMap::new(),
                    text: String::new(),
                    children: Vec::new(),
                };

                // Parse attributes
                for attr in e.attributes().flatten() {
                    let key = String::from_utf8_lossy(attr.key.as_ref()).to_string();
                    let value = String::from_utf8_lossy(&attr.value).to_string();
                    elem.attributes.insert(key, value);
                }

                // Add to parent's children
                if let Some(&parent_handle) = element_stack.last() {
                    if let Some(parent) = doc.elements.get_mut(&parent_handle) {
                        parent.children.push(elem_handle);
                    }
                } else {
                    doc.root = Some(elem_handle);
                }

                doc.elements.insert(elem_handle, elem);
                element_stack.push(elem_handle);
            }
            Ok(Event::Empty(ref e)) => {
                let elem_handle = next_handle();
                let mut elem = XmlElement {
                    tag: String::from_utf8_lossy(e.name().as_ref()).to_string(),
                    attributes: HashMap::new(),
                    text: String::new(),
                    children: Vec::new(),
                };

                for attr in e.attributes().flatten() {
                    let key = String::from_utf8_lossy(attr.key.as_ref()).to_string();
                    let value = String::from_utf8_lossy(&attr.value).to_string();
                    elem.attributes.insert(key, value);
                }

                if let Some(&parent_handle) = element_stack.last() {
                    if let Some(parent) = doc.elements.get_mut(&parent_handle) {
                        parent.children.push(elem_handle);
                    }
                } else {
                    doc.root = Some(elem_handle);
                }

                doc.elements.insert(elem_handle, elem);
            }
            Ok(Event::Text(ref e)) => {
                if let Some(&current_handle) = element_stack.last() {
                    if let Some(elem) = doc.elements.get_mut(&current_handle) {
                        elem.text = String::from_utf8_lossy(e.as_ref()).to_string();
                    }
                }
            }
            Ok(Event::End(_)) => {
                element_stack.pop();
            }
            Ok(Event::Eof) => break,
            Err(_) => return 0,
            _ => {}
        }
        buf.clear();
    }

    if let Ok(mut docs) = get_documents().lock() {
        docs.insert(doc_handle, doc);
    }

    doc_handle
}

/// Free XML document
#[no_mangle]
pub extern "C" fn xml_free(handle: u64) {
    if let Ok(mut docs) = get_documents().lock() {
        docs.remove(&handle);
    }
}

// ============================================================================
// Navigation
// ============================================================================

/// Get root element handle
#[no_mangle]
pub extern "C" fn xml_root(doc_handle: u64) -> u64 {
    if let Ok(docs) = get_documents().lock() {
        if let Some(doc) = docs.get(&doc_handle) {
            return doc.root.unwrap_or(0);
        }
    }
    0
}

/// Get element tag name
#[no_mangle]
pub extern "C" fn xml_tag_name(doc_handle: u64, elem_handle: u64) -> StringPtr {
    if let Ok(docs) = get_documents().lock() {
        if let Some(doc) = docs.get(&doc_handle) {
            if let Some(elem) = doc.elements.get(&elem_handle) {
                return string_new(&elem.tag);
            }
        }
    }
    string_new("")
}

/// Get element text content
#[no_mangle]
pub extern "C" fn xml_text(doc_handle: u64, elem_handle: u64) -> StringPtr {
    if let Ok(docs) = get_documents().lock() {
        if let Some(doc) = docs.get(&doc_handle) {
            if let Some(elem) = doc.elements.get(&elem_handle) {
                return string_new(&elem.text);
            }
        }
    }
    string_new("")
}

/// Get attribute value
#[no_mangle]
pub extern "C" fn xml_attribute(doc_handle: u64, elem_handle: u64, name: StringPtr) -> StringPtr {
    let attr_name = match unsafe { string_as_str(name) } {
        Some(s) => s,
        None => return string_new(""),
    };

    if let Ok(docs) = get_documents().lock() {
        if let Some(doc) = docs.get(&doc_handle) {
            if let Some(elem) = doc.elements.get(&elem_handle) {
                if let Some(value) = elem.attributes.get(attr_name) {
                    return string_new(value);
                }
            }
        }
    }
    string_new("")
}

/// Check if element has attribute
#[no_mangle]
pub extern "C" fn xml_has_attribute(doc_handle: u64, elem_handle: u64, name: StringPtr) -> i32 {
    let attr_name = match unsafe { string_as_str(name) } {
        Some(s) => s,
        None => return 0,
    };

    if let Ok(docs) = get_documents().lock() {
        if let Some(doc) = docs.get(&doc_handle) {
            if let Some(elem) = doc.elements.get(&elem_handle) {
                if elem.attributes.contains_key(attr_name) {
                    return 1;
                }
            }
        }
    }
    0
}

/// Get all attribute names as array of strings
#[no_mangle]
pub extern "C" fn xml_attributes(doc_handle: u64, elem_handle: u64) -> ArrayPtr {
    if let Ok(docs) = get_documents().lock() {
        if let Some(doc) = docs.get(&doc_handle) {
            if let Some(elem) = doc.elements.get(&elem_handle) {
                let arr = array_new::<StringPtr>(elem.attributes.len());
                if !arr.is_null() {
                    unsafe {
                        for key in elem.attributes.keys() {
                            array_push(arr, string_new(key));
                        }
                    }
                    return arr;
                }
            }
        }
    }
    std::ptr::null_mut()
}

/// Get number of child elements
#[no_mangle]
pub extern "C" fn xml_child_count(doc_handle: u64, elem_handle: u64) -> i64 {
    if let Ok(docs) = get_documents().lock() {
        if let Some(doc) = docs.get(&doc_handle) {
            if let Some(elem) = doc.elements.get(&elem_handle) {
                return elem.children.len() as i64;
            }
        }
    }
    0
}

/// Get child element handles as array
#[no_mangle]
pub extern "C" fn xml_children(doc_handle: u64, elem_handle: u64) -> ArrayPtr {
    if let Ok(docs) = get_documents().lock() {
        if let Some(doc) = docs.get(&doc_handle) {
            if let Some(elem) = doc.elements.get(&elem_handle) {
                let arr = array_new::<u64>(elem.children.len());
                if !arr.is_null() {
                    unsafe {
                        for &child in &elem.children {
                            array_push(arr, child);
                        }
                    }
                    return arr;
                }
            }
        }
    }
    std::ptr::null_mut()
}

/// Get child element by index
#[no_mangle]
pub extern "C" fn xml_child_at(doc_handle: u64, elem_handle: u64, index: i64) -> u64 {
    if index < 0 {
        return 0;
    }

    if let Ok(docs) = get_documents().lock() {
        if let Some(doc) = docs.get(&doc_handle) {
            if let Some(elem) = doc.elements.get(&elem_handle) {
                if (index as usize) < elem.children.len() {
                    return elem.children[index as usize];
                }
            }
        }
    }
    0
}

/// Get first child element with given tag name
#[no_mangle]
pub extern "C" fn xml_child(doc_handle: u64, elem_handle: u64, tag_name: StringPtr) -> u64 {
    let tag = match unsafe { string_as_str(tag_name) } {
        Some(s) => s,
        None => return 0,
    };

    if let Ok(docs) = get_documents().lock() {
        if let Some(doc) = docs.get(&doc_handle) {
            if let Some(elem) = doc.elements.get(&elem_handle) {
                for &child_handle in &elem.children {
                    if let Some(child) = doc.elements.get(&child_handle) {
                        if child.tag == tag {
                            return child_handle;
                        }
                    }
                }
            }
        }
    }
    0
}

/// Find all elements with tag name (recursive search)
#[no_mangle]
pub extern "C" fn xml_find(doc_handle: u64, tag_name: StringPtr) -> ArrayPtr {
    let tag = match unsafe { string_as_str(tag_name) } {
        Some(s) => s.to_string(),
        None => return std::ptr::null_mut(),
    };

    let mut results = Vec::new();

    if let Ok(docs) = get_documents().lock() {
        if let Some(doc) = docs.get(&doc_handle) {
            fn find_recursive(doc: &XmlDocument, elem_handle: u64, tag: &str, results: &mut Vec<u64>) {
                if let Some(elem) = doc.elements.get(&elem_handle) {
                    if elem.tag == tag {
                        results.push(elem_handle);
                    }
                    for &child in &elem.children {
                        find_recursive(doc, child, tag, results);
                    }
                }
            }

            if let Some(root) = doc.root {
                find_recursive(doc, root, &tag, &mut results);
            }
        }
    }

    let arr = array_new::<u64>(results.len());
    if !arr.is_null() {
        unsafe {
            for handle in results {
                array_push(arr, handle);
            }
        }
    }
    arr
}

// ============================================================================
// Building
// ============================================================================

/// Create new empty document
#[no_mangle]
pub extern "C" fn xml_create() -> u64 {
    let doc_handle = next_handle();
    let doc = XmlDocument {
        root: None,
        elements: HashMap::new(),
    };

    if let Ok(mut docs) = get_documents().lock() {
        docs.insert(doc_handle, doc);
    }

    doc_handle
}

/// Create new element and add to document
#[no_mangle]
pub extern "C" fn xml_element(doc_handle: u64, tag_name: StringPtr) -> u64 {
    let tag = match unsafe { string_as_str(tag_name) } {
        Some(s) => s.to_string(),
        None => return 0,
    };

    let elem_handle = next_handle();
    let elem = XmlElement {
        tag,
        attributes: HashMap::new(),
        text: String::new(),
        children: Vec::new(),
    };

    if let Ok(mut docs) = get_documents().lock() {
        if let Some(doc) = docs.get_mut(&doc_handle) {
            doc.elements.insert(elem_handle, elem);
            return elem_handle;
        }
    }
    0
}

/// Set document root element
#[no_mangle]
pub extern "C" fn xml_set_root(doc_handle: u64, elem_handle: u64) {
    if let Ok(mut docs) = get_documents().lock() {
        if let Some(doc) = docs.get_mut(&doc_handle) {
            doc.root = Some(elem_handle);
        }
    }
}

/// Set element text content
#[no_mangle]
pub extern "C" fn xml_set_text(doc_handle: u64, elem_handle: u64, text: StringPtr) {
    let text_str = match unsafe { string_as_str(text) } {
        Some(s) => s.to_string(),
        None => return,
    };

    if let Ok(mut docs) = get_documents().lock() {
        if let Some(doc) = docs.get_mut(&doc_handle) {
            if let Some(elem) = doc.elements.get_mut(&elem_handle) {
                elem.text = text_str;
            }
        }
    }
}

/// Set element attribute
#[no_mangle]
pub extern "C" fn xml_set_attribute(doc_handle: u64, elem_handle: u64, name: StringPtr, value: StringPtr) {
    let attr_name = match unsafe { string_as_str(name) } {
        Some(s) => s.to_string(),
        None => return,
    };
    let attr_value = match unsafe { string_as_str(value) } {
        Some(s) => s.to_string(),
        None => return,
    };

    if let Ok(mut docs) = get_documents().lock() {
        if let Some(doc) = docs.get_mut(&doc_handle) {
            if let Some(elem) = doc.elements.get_mut(&elem_handle) {
                elem.attributes.insert(attr_name, attr_value);
            }
        }
    }
}

/// Remove element attribute
#[no_mangle]
pub extern "C" fn xml_remove_attribute(doc_handle: u64, elem_handle: u64, name: StringPtr) {
    let attr_name = match unsafe { string_as_str(name) } {
        Some(s) => s,
        None => return,
    };

    if let Ok(mut docs) = get_documents().lock() {
        if let Some(doc) = docs.get_mut(&doc_handle) {
            if let Some(elem) = doc.elements.get_mut(&elem_handle) {
                elem.attributes.remove(attr_name);
            }
        }
    }
}

/// Append child element
#[no_mangle]
pub extern "C" fn xml_append_child(doc_handle: u64, parent_handle: u64, child_handle: u64) {
    if let Ok(mut docs) = get_documents().lock() {
        if let Some(doc) = docs.get_mut(&doc_handle) {
            if let Some(parent) = doc.elements.get_mut(&parent_handle) {
                parent.children.push(child_handle);
            }
        }
    }
}

// ============================================================================
// Output
// ============================================================================

fn element_to_xml(doc: &XmlDocument, elem_handle: u64, writer: &mut Writer<Vec<u8>>) {
    if let Some(elem) = doc.elements.get(&elem_handle) {
        let mut start = BytesStart::new(&elem.tag);

        for (key, value) in &elem.attributes {
            start.push_attribute((key.as_str(), value.as_str()));
        }

        if elem.children.is_empty() && elem.text.is_empty() {
            let _ = writer.write_event(Event::Empty(start));
        } else {
            let _ = writer.write_event(Event::Start(start));

            if !elem.text.is_empty() {
                let _ = writer.write_event(Event::Text(BytesText::new(&elem.text)));
            }

            for &child in &elem.children {
                element_to_xml(doc, child, writer);
            }

            let _ = writer.write_event(Event::End(BytesEnd::new(&elem.tag)));
        }
    }
}

/// Convert document to XML string
#[no_mangle]
pub extern "C" fn xml_to_string(doc_handle: u64) -> StringPtr {
    if let Ok(docs) = get_documents().lock() {
        if let Some(doc) = docs.get(&doc_handle) {
            let mut writer = Writer::new(Vec::new());

            // XML declaration
            let _ = writer.write_event(Event::Decl(BytesDecl::new("1.0", Some("UTF-8"), None)));

            if let Some(root) = doc.root {
                element_to_xml(doc, root, &mut writer);
            }

            let result = writer.into_inner();
            return string_new(&String::from_utf8_lossy(&result));
        }
    }
    string_new("")
}

/// Convert document to pretty-printed XML string
#[no_mangle]
pub extern "C" fn xml_to_string_pretty(doc_handle: u64, indent: i32) -> StringPtr {
    if let Ok(docs) = get_documents().lock() {
        if let Some(doc) = docs.get(&doc_handle) {
            let mut writer = Writer::new_with_indent(Vec::new(), b' ', indent as usize);

            // XML declaration
            let _ = writer.write_event(Event::Decl(BytesDecl::new("1.0", Some("UTF-8"), None)));

            if let Some(root) = doc.root {
                element_to_xml(doc, root, &mut writer);
            }

            let result = writer.into_inner();
            return string_new(&String::from_utf8_lossy(&result));
        }
    }
    string_new("")
}

/// Save document to file
#[no_mangle]
pub extern "C" fn xml_save(doc_handle: u64, path: StringPtr) -> i32 {
    let path_str = match unsafe { string_as_str(path) } {
        Some(s) => s,
        None => return -1,
    };

    let xml_string = xml_to_string_pretty(doc_handle, 2);
    let content = match unsafe { string_as_str(xml_string) } {
        Some(s) => s.to_string(),
        None => return -1,
    };
    unsafe { string_free(xml_string) };

    match std::fs::write(path_str, content) {
        Ok(()) => 0,
        Err(_) => -1,
    }
}

// ============================================================================
// Plugin Export
// ============================================================================

zrtl_plugin! {
    name: "zrtl_xml",
    symbols: [
        // Parsing
        ("$Xml$parse", xml_parse),
        ("$Xml$parse_file", xml_parse_file),
        ("$Xml$free", xml_free),

        // Navigation
        ("$Xml$root", xml_root),
        ("$Xml$tag_name", xml_tag_name),
        ("$Xml$text", xml_text),
        ("$Xml$attribute", xml_attribute),
        ("$Xml$has_attribute", xml_has_attribute),
        ("$Xml$attributes", xml_attributes),
        ("$Xml$child_count", xml_child_count),
        ("$Xml$children", xml_children),
        ("$Xml$child_at", xml_child_at),
        ("$Xml$child", xml_child),
        ("$Xml$find", xml_find),

        // Building
        ("$Xml$create", xml_create),
        ("$Xml$element", xml_element),
        ("$Xml$set_root", xml_set_root),
        ("$Xml$set_text", xml_set_text),
        ("$Xml$set_attribute", xml_set_attribute),
        ("$Xml$remove_attribute", xml_remove_attribute),
        ("$Xml$append_child", xml_append_child),

        // Output
        ("$Xml$to_string", xml_to_string),
        ("$Xml$to_string_pretty", xml_to_string_pretty),
        ("$Xml$save", xml_save),
    ]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple() {
        let xml = "<root><child>Hello</child></root>";
        let doc = xml_parse(string_new(xml));
        assert!(doc > 0);

        let root = xml_root(doc);
        assert!(root > 0);

        let tag = xml_tag_name(doc, root);
        assert_eq!(unsafe { string_as_str(tag) }, Some("root"));

        xml_free(doc);
    }

    #[test]
    fn test_parse_with_attributes() {
        let xml = r#"<element id="test" class="example">Content</element>"#;
        let doc = xml_parse(string_new(xml));
        assert!(doc > 0);

        let root = xml_root(doc);
        let id = xml_attribute(doc, root, string_new("id"));
        assert_eq!(unsafe { string_as_str(id) }, Some("test"));

        let class = xml_attribute(doc, root, string_new("class"));
        assert_eq!(unsafe { string_as_str(class) }, Some("example"));

        xml_free(doc);
    }

    #[test]
    fn test_build_document() {
        let doc = xml_create();

        let root = xml_element(doc, string_new("root"));
        xml_set_root(doc, root);

        let child = xml_element(doc, string_new("child"));
        xml_set_text(doc, child, string_new("Hello World"));
        xml_set_attribute(doc, child, string_new("id"), string_new("1"));
        xml_append_child(doc, root, child);

        let output = xml_to_string(doc);
        let output_str = unsafe { string_as_str(output) }.unwrap();
        assert!(output_str.contains("<root>"));
        assert!(output_str.contains("<child"));
        assert!(output_str.contains("Hello World"));

        xml_free(doc);
    }
}
