/*
 * ZRTL (Zyntax Runtime Library) SDK
 *
 * This header provides the interface for creating ZRTL plugins
 * that can be loaded by the Zyntax compiler at runtime.
 *
 * Similar to HashLink's HDLL format, ZRTL files are native dynamic
 * libraries that export runtime symbols for the JIT/AOT compiler.
 *
 * Example usage:
 *
 *   #include "zrtl.h"
 *
 *   // Define your runtime functions
 *   int32_t my_add(int32_t a, int32_t b) {
 *       return a + b;
 *   }
 *
 *   void my_print(const char* msg) {
 *       printf("%s\n", msg);
 *   }
 *
 *   // Export symbol table (required)
 *   ZRTL_SYMBOLS_BEGIN
 *       ZRTL_SYMBOL("$MyRuntime$add", my_add),
 *       ZRTL_SYMBOL("$MyRuntime$print", my_print),
 *   ZRTL_SYMBOLS_END
 *
 *   // Export plugin info (required)
 *   ZRTL_PLUGIN_INFO("my_runtime")
 *
 * Build commands:
 *
 *   Linux:   gcc -shared -fPIC -o my_runtime.zrtl my_runtime.c
 *   macOS:   clang -shared -fPIC -o my_runtime.zrtl my_runtime.c
 *   Windows: cl /LD my_runtime.c /Fe:my_runtime.zrtl
 *
 * Load in zyntax:
 *
 *   zyntax compile --runtime my_runtime.zrtl --source input.hx
 */

#ifndef ZRTL_H
#define ZRTL_H

#include <stdint.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ZRTL format version - must match the compiler's expected version */
#define ZRTL_VERSION 1

/* Platform-specific export macro */
#ifdef _WIN32
    #define ZRTL_EXPORT __declspec(dllexport)
#else
    #define ZRTL_EXPORT __attribute__((visibility("default")))
#endif

/* Symbol entry structure */
typedef struct {
    const char* name;   /* Symbol name, e.g., "$Array$push" */
    void* ptr;          /* Function pointer */
} ZrtlSymbol;

/* Plugin info structure */
typedef struct {
    uint32_t version;   /* Must be ZRTL_VERSION */
    const char* name;   /* Plugin name */
} ZrtlInfo;

/*
 * Macros for defining the symbol table
 *
 * Usage:
 *   ZRTL_SYMBOLS_BEGIN
 *       ZRTL_SYMBOL("$Type$method", function_ptr),
 *       ZRTL_SYMBOL("$Type$other", other_ptr),
 *   ZRTL_SYMBOLS_END
 */
#define ZRTL_SYMBOLS_BEGIN \
    ZRTL_EXPORT ZrtlSymbol _zrtl_symbols[] = {

#define ZRTL_SYMBOL(name, func) \
    { name, (void*)(func) }

#define ZRTL_SYMBOLS_END \
        { NULL, NULL } \
    };

/*
 * Macro for defining plugin info
 *
 * Usage:
 *   ZRTL_PLUGIN_INFO("my_plugin_name")
 */
#define ZRTL_PLUGIN_INFO(plugin_name) \
    ZRTL_EXPORT ZrtlInfo _zrtl_info = { \
        .version = ZRTL_VERSION, \
        .name = plugin_name \
    };

/* ============================================================
 * Type Tags for Dynamic Type Identification
 * ============================================================
 *
 * Type tags identify the runtime type of opaque values passed between
 * Zyntax and native code. This enables type-safe interop with boxed values.
 *
 * Tag layout (32-bit):
 *   Bits 0-7:   Category (primitive, struct, array, function, etc.)
 *   Bits 8-23:  Type ID within category (registered at runtime)
 *   Bits 24-31: Flags (nullable, mutable, etc.)
 */

/* Type categories */
typedef enum {
    ZRTL_CAT_VOID       = 0x00,  /* void / unit type */
    ZRTL_CAT_BOOL       = 0x01,  /* boolean */
    ZRTL_CAT_INT        = 0x02,  /* signed integers (i8, i16, i32, i64) */
    ZRTL_CAT_UINT       = 0x03,  /* unsigned integers (u8, u16, u32, u64) */
    ZRTL_CAT_FLOAT      = 0x04,  /* floating point (f32, f64) */
    ZRTL_CAT_STRING     = 0x05,  /* string */
    ZRTL_CAT_ARRAY      = 0x06,  /* array/vector */
    ZRTL_CAT_MAP        = 0x07,  /* hashmap/dictionary */
    ZRTL_CAT_STRUCT     = 0x08,  /* struct/record */
    ZRTL_CAT_CLASS      = 0x09,  /* class instance */
    ZRTL_CAT_ENUM       = 0x0A,  /* enum variant */
    ZRTL_CAT_UNION      = 0x0B,  /* tagged union */
    ZRTL_CAT_FUNCTION   = 0x0C,  /* function pointer/closure */
    ZRTL_CAT_POINTER    = 0x0D,  /* raw pointer */
    ZRTL_CAT_OPTIONAL   = 0x0E,  /* optional/nullable */
    ZRTL_CAT_RESULT     = 0x0F,  /* result/error type */
    ZRTL_CAT_TUPLE      = 0x10,  /* tuple */
    ZRTL_CAT_TRAIT_OBJ  = 0x11,  /* trait object / dyn */
    ZRTL_CAT_OPAQUE     = 0x12,  /* opaque/foreign type */
    ZRTL_CAT_CUSTOM     = 0xFF,  /* custom user-defined category */
} ZrtlTypeCategory;

/* Type flags */
typedef enum {
    ZRTL_FLAG_NONE      = 0x00,
    ZRTL_FLAG_NULLABLE  = 0x01,  /* Type can be null */
    ZRTL_FLAG_MUTABLE   = 0x02,  /* Value is mutable */
    ZRTL_FLAG_BOXED     = 0x04,  /* Value is heap-allocated */
    ZRTL_FLAG_ARC       = 0x08,  /* Value uses ARC */
    ZRTL_FLAG_WEAK      = 0x10,  /* Weak reference */
    ZRTL_FLAG_PINNED    = 0x20,  /* Value cannot be moved */
} ZrtlTypeFlags;

/* Primitive type IDs (within INT/UINT/FLOAT categories) */
typedef enum {
    ZRTL_PRIM_8   = 0x01,  /* 8-bit (i8, u8) */
    ZRTL_PRIM_16  = 0x02,  /* 16-bit (i16, u16) */
    ZRTL_PRIM_32  = 0x03,  /* 32-bit (i32, u32, f32) */
    ZRTL_PRIM_64  = 0x04,  /* 64-bit (i64, u64, f64) */
    ZRTL_PRIM_PTR = 0x05,  /* pointer-sized (isize, usize) */
} ZrtlPrimitiveSize;

/* Type tag - 32-bit packed type identifier */
typedef uint32_t ZrtlTypeTag;

/* Construct a type tag */
#define ZRTL_MAKE_TAG(category, type_id, flags) \
    ((ZrtlTypeTag)(((flags) << 24) | ((type_id) << 8) | (category)))

/* Extract components from a type tag */
#define ZRTL_TAG_CATEGORY(tag) ((ZrtlTypeCategory)((tag) & 0xFF))
#define ZRTL_TAG_TYPE_ID(tag)  ((uint16_t)(((tag) >> 8) & 0xFFFF))
#define ZRTL_TAG_FLAGS(tag)    ((ZrtlTypeFlags)(((tag) >> 24) & 0xFF))

/* Pre-defined type tags for primitives */
#define ZRTL_TAG_VOID    ZRTL_MAKE_TAG(ZRTL_CAT_VOID, 0, 0)
#define ZRTL_TAG_BOOL    ZRTL_MAKE_TAG(ZRTL_CAT_BOOL, 0, 0)
#define ZRTL_TAG_I8      ZRTL_MAKE_TAG(ZRTL_CAT_INT, ZRTL_PRIM_8, 0)
#define ZRTL_TAG_I16     ZRTL_MAKE_TAG(ZRTL_CAT_INT, ZRTL_PRIM_16, 0)
#define ZRTL_TAG_I32     ZRTL_MAKE_TAG(ZRTL_CAT_INT, ZRTL_PRIM_32, 0)
#define ZRTL_TAG_I64     ZRTL_MAKE_TAG(ZRTL_CAT_INT, ZRTL_PRIM_64, 0)
#define ZRTL_TAG_ISIZE   ZRTL_MAKE_TAG(ZRTL_CAT_INT, ZRTL_PRIM_PTR, 0)
#define ZRTL_TAG_U8      ZRTL_MAKE_TAG(ZRTL_CAT_UINT, ZRTL_PRIM_8, 0)
#define ZRTL_TAG_U16     ZRTL_MAKE_TAG(ZRTL_CAT_UINT, ZRTL_PRIM_16, 0)
#define ZRTL_TAG_U32     ZRTL_MAKE_TAG(ZRTL_CAT_UINT, ZRTL_PRIM_32, 0)
#define ZRTL_TAG_U64     ZRTL_MAKE_TAG(ZRTL_CAT_UINT, ZRTL_PRIM_64, 0)
#define ZRTL_TAG_USIZE   ZRTL_MAKE_TAG(ZRTL_CAT_UINT, ZRTL_PRIM_PTR, 0)
#define ZRTL_TAG_F32     ZRTL_MAKE_TAG(ZRTL_CAT_FLOAT, ZRTL_PRIM_32, 0)
#define ZRTL_TAG_F64     ZRTL_MAKE_TAG(ZRTL_CAT_FLOAT, ZRTL_PRIM_64, 0)
#define ZRTL_TAG_STRING  ZRTL_MAKE_TAG(ZRTL_CAT_STRING, 0, 0)

/* ============================================================
 * DynamicBox - Runtime Boxed Value
 * ============================================================
 *
 * DynamicBox is the standard way to pass opaque/polymorphic values
 * between Zyntax code and native runtime functions.
 *
 * Layout (24 bytes on 64-bit):
 *   - tag:     Type tag identifying the contained type
 *   - size:    Size of the data in bytes
 *   - data:    Pointer to the actual data
 *   - dropper: Optional destructor function
 *
 * Usage in native code:
 *
 *   void process_value(ZrtlDynamicBox* box) {
 *       switch (ZRTL_TAG_CATEGORY(box->tag)) {
 *           case ZRTL_CAT_INT:
 *               // Handle integer
 *               int64_t value = zrtl_box_as_i64(box);
 *               break;
 *           case ZRTL_CAT_STRING:
 *               // Handle string
 *               ZrtlString* str = (ZrtlString*)box->data;
 *               break;
 *           case ZRTL_CAT_STRUCT:
 *               // Handle struct by type ID
 *               if (ZRTL_TAG_TYPE_ID(box->tag) == MY_POINT_TYPE_ID) {
 *                   MyPoint* pt = (MyPoint*)box->data;
 *               }
 *               break;
 *       }
 *   }
 */

/* Destructor function type for boxed values */
typedef void (*ZrtlDropFn)(void* data);

/* Dynamic boxed value */
typedef struct {
    ZrtlTypeTag tag;       /* Type tag identifying the type */
    uint32_t size;         /* Size of data in bytes */
    void* data;            /* Pointer to the data */
    ZrtlDropFn dropper;    /* Optional destructor (NULL if no cleanup needed) */
} ZrtlDynamicBox;

/* Check if a box is null/empty */
#define ZRTL_BOX_IS_NULL(box) ((box)->data == NULL)

/* Check box type category */
#define ZRTL_BOX_IS_CATEGORY(box, cat) (ZRTL_TAG_CATEGORY((box)->tag) == (cat))

/* Type-safe accessors for primitive types */
static inline int8_t zrtl_box_as_i8(const ZrtlDynamicBox* box) {
    return box->data ? *(int8_t*)box->data : 0;
}

static inline int16_t zrtl_box_as_i16(const ZrtlDynamicBox* box) {
    return box->data ? *(int16_t*)box->data : 0;
}

static inline int32_t zrtl_box_as_i32(const ZrtlDynamicBox* box) {
    return box->data ? *(int32_t*)box->data : 0;
}

static inline int64_t zrtl_box_as_i64(const ZrtlDynamicBox* box) {
    return box->data ? *(int64_t*)box->data : 0;
}

static inline uint8_t zrtl_box_as_u8(const ZrtlDynamicBox* box) {
    return box->data ? *(uint8_t*)box->data : 0;
}

static inline uint16_t zrtl_box_as_u16(const ZrtlDynamicBox* box) {
    return box->data ? *(uint16_t*)box->data : 0;
}

static inline uint32_t zrtl_box_as_u32(const ZrtlDynamicBox* box) {
    return box->data ? *(uint32_t*)box->data : 0;
}

static inline uint64_t zrtl_box_as_u64(const ZrtlDynamicBox* box) {
    return box->data ? *(uint64_t*)box->data : 0;
}

static inline float zrtl_box_as_f32(const ZrtlDynamicBox* box) {
    return box->data ? *(float*)box->data : 0.0f;
}

static inline double zrtl_box_as_f64(const ZrtlDynamicBox* box) {
    return box->data ? *(double*)box->data : 0.0;
}

static inline uint8_t zrtl_box_as_bool(const ZrtlDynamicBox* box) {
    return box->data ? *(uint8_t*)box->data : 0;
}

/* Get data as typed pointer (caller must verify tag first) */
#define ZRTL_BOX_AS(box, type) ((type*)((box)->data))

/* Create a box for primitive values (stack-allocated data) */
static inline ZrtlDynamicBox zrtl_box_i32(int32_t* value) {
    ZrtlDynamicBox box = { ZRTL_TAG_I32, sizeof(int32_t), value, NULL };
    return box;
}

static inline ZrtlDynamicBox zrtl_box_i64(int64_t* value) {
    ZrtlDynamicBox box = { ZRTL_TAG_I64, sizeof(int64_t), value, NULL };
    return box;
}

static inline ZrtlDynamicBox zrtl_box_f32(float* value) {
    ZrtlDynamicBox box = { ZRTL_TAG_F32, sizeof(float), value, NULL };
    return box;
}

static inline ZrtlDynamicBox zrtl_box_f64(double* value) {
    ZrtlDynamicBox box = { ZRTL_TAG_F64, sizeof(double), value, NULL };
    return box;
}

static inline ZrtlDynamicBox zrtl_box_bool(uint8_t* value) {
    ZrtlDynamicBox box = { ZRTL_TAG_BOOL, sizeof(uint8_t), value, NULL };
    return box;
}

/* Create a heap-allocated box */
static inline ZrtlDynamicBox zrtl_box_alloc(ZrtlTypeTag tag, uint32_t size) {
    ZrtlDynamicBox box;
    box.tag = tag;
    box.size = size;
    box.data = zrtl_alloc(size);
    box.dropper = zrtl_free;
    return box;
}

/* Free a boxed value */
static inline void zrtl_box_free(ZrtlDynamicBox* box) {
    if (box->data && box->dropper) {
        box->dropper(box->data);
    }
    box->data = NULL;
    box->size = 0;
    box->tag = ZRTL_TAG_VOID;
    box->dropper = NULL;
}

/* Clone a box (deep copy) */
static inline ZrtlDynamicBox zrtl_box_clone(const ZrtlDynamicBox* box) {
    extern void* memcpy(void*, const void*, size_t);
    ZrtlDynamicBox clone;
    clone.tag = box->tag;
    clone.size = box->size;
    if (box->data && box->size > 0) {
        clone.data = zrtl_alloc(box->size);
        memcpy(clone.data, box->data, box->size);
        clone.dropper = zrtl_free;
    } else {
        clone.data = NULL;
        clone.dropper = NULL;
    }
    return clone;
}

/* ============================================================
 * Generic Type Support
 * ============================================================
 *
 * For generic types like Array<T>, Map<K,V>, Optional<T>, etc.,
 * we need to track the type arguments. This is done via a
 * GenericTypeArgs structure that accompanies the DynamicBox.
 *
 * Example: Array<Int32> would have:
 *   - tag: ZRTL_CAT_ARRAY
 *   - type_args: [ZRTL_TAG_I32]
 *
 * Example: Map<String, Array<Int32>> would have:
 *   - tag: ZRTL_CAT_MAP
 *   - type_args: [ZRTL_TAG_STRING, ZRTL_MAKE_TAG(ZRTL_CAT_ARRAY, ...)]
 *   - nested GenericTypeArgs for the Array<Int32>
 */

/* Maximum type arguments for a generic type */
#define ZRTL_MAX_TYPE_ARGS 8

/* Generic type arguments */
typedef struct ZrtlGenericTypeArgs {
    uint8_t count;                                /* Number of type arguments */
    ZrtlTypeTag args[ZRTL_MAX_TYPE_ARGS];         /* Type tags for each argument */
    struct ZrtlGenericTypeArgs* nested[ZRTL_MAX_TYPE_ARGS]; /* Nested generics (NULL if not generic) */
} ZrtlGenericTypeArgs;

/* Extended DynamicBox with generic type info */
typedef struct {
    ZrtlDynamicBox base;                          /* Base box (tag, size, data, dropper) */
    ZrtlGenericTypeArgs* type_args;               /* Generic type arguments (NULL if non-generic) */
} ZrtlGenericBox;

/* Check if a box is a generic type */
#define ZRTL_BOX_IS_GENERIC(gbox) ((gbox)->type_args != NULL && (gbox)->type_args->count > 0)

/* Get type argument at index */
static inline ZrtlTypeTag zrtl_gbox_type_arg(const ZrtlGenericBox* gbox, uint8_t index) {
    if (!gbox->type_args || index >= gbox->type_args->count) {
        return ZRTL_TAG_VOID;
    }
    return gbox->type_args->args[index];
}

/* Create generic type args (caller owns memory) */
static inline ZrtlGenericTypeArgs* zrtl_type_args_new(uint8_t count) {
    extern void* memset(void*, int, size_t);
    ZrtlGenericTypeArgs* args = (ZrtlGenericTypeArgs*)zrtl_alloc(sizeof(ZrtlGenericTypeArgs));
    memset(args, 0, sizeof(ZrtlGenericTypeArgs));
    args->count = count;
    return args;
}

/* Free generic type args (including nested) */
static inline void zrtl_type_args_free(ZrtlGenericTypeArgs* args) {
    if (!args) return;
    for (uint8_t i = 0; i < args->count; i++) {
        if (args->nested[i]) {
            zrtl_type_args_free(args->nested[i]);
        }
    }
    zrtl_free(args);
}

/* Create a generic box for Array<T> */
static inline ZrtlGenericBox zrtl_gbox_array(void* data, uint32_t size, ZrtlTypeTag element_type) {
    ZrtlGenericBox gbox;
    gbox.base.tag = ZRTL_MAKE_TAG(ZRTL_CAT_ARRAY, 0, 0);
    gbox.base.size = size;
    gbox.base.data = data;
    gbox.base.dropper = NULL;
    gbox.type_args = zrtl_type_args_new(1);
    gbox.type_args->args[0] = element_type;
    return gbox;
}

/* Create a generic box for Map<K, V> */
static inline ZrtlGenericBox zrtl_gbox_map(void* data, uint32_t size, ZrtlTypeTag key_type, ZrtlTypeTag value_type) {
    ZrtlGenericBox gbox;
    gbox.base.tag = ZRTL_MAKE_TAG(ZRTL_CAT_MAP, 0, 0);
    gbox.base.size = size;
    gbox.base.data = data;
    gbox.base.dropper = NULL;
    gbox.type_args = zrtl_type_args_new(2);
    gbox.type_args->args[0] = key_type;
    gbox.type_args->args[1] = value_type;
    return gbox;
}

/* Create a generic box for Optional<T> */
static inline ZrtlGenericBox zrtl_gbox_optional(void* data, uint32_t size, ZrtlTypeTag inner_type) {
    ZrtlGenericBox gbox;
    gbox.base.tag = ZRTL_MAKE_TAG(ZRTL_CAT_OPTIONAL, 0, 0);
    gbox.base.size = size;
    gbox.base.data = data;
    gbox.base.dropper = NULL;
    gbox.type_args = zrtl_type_args_new(1);
    gbox.type_args->args[0] = inner_type;
    return gbox;
}

/* Create a generic box for Result<T, E> */
static inline ZrtlGenericBox zrtl_gbox_result(void* data, uint32_t size, ZrtlTypeTag ok_type, ZrtlTypeTag err_type) {
    ZrtlGenericBox gbox;
    gbox.base.tag = ZRTL_MAKE_TAG(ZRTL_CAT_RESULT, 0, 0);
    gbox.base.size = size;
    gbox.base.data = data;
    gbox.base.dropper = NULL;
    gbox.type_args = zrtl_type_args_new(2);
    gbox.type_args->args[0] = ok_type;
    gbox.type_args->args[1] = err_type;
    return gbox;
}

/* Free a generic box */
static inline void zrtl_gbox_free(ZrtlGenericBox* gbox) {
    zrtl_box_free(&gbox->base);
    if (gbox->type_args) {
        zrtl_type_args_free(gbox->type_args);
        gbox->type_args = NULL;
    }
}

/* ============================================================
 * Type Descriptor - Full Type Information
 * ============================================================
 *
 * For complex types, we may need more than just a tag.
 * TypeDescriptor provides full structural type information.
 */

/* Forward declaration */
struct ZrtlTypeDescriptor;

/* Field descriptor for struct types */
typedef struct {
    const char* name;                       /* Field name */
    uint32_t offset;                        /* Byte offset within struct */
    struct ZrtlTypeDescriptor* type;        /* Field type descriptor */
} ZrtlFieldDescriptor;

/* Type descriptor - complete type information */
typedef struct ZrtlTypeDescriptor {
    ZrtlTypeTag tag;                        /* Base type tag */
    const char* name;                       /* Type name (e.g., "Array", "MyStruct") */
    uint32_t size;                          /* Size in bytes */
    uint32_t alignment;                     /* Alignment requirement */
    ZrtlDropFn dropper;                     /* Destructor */

    /* For generic types */
    uint8_t type_param_count;               /* Number of type parameters */
    struct ZrtlTypeDescriptor** type_params;/* Type parameter descriptors */

    /* For struct/class types */
    uint16_t field_count;                   /* Number of fields */
    ZrtlFieldDescriptor* fields;            /* Field descriptors */

    /* For enum types */
    uint16_t variant_count;                 /* Number of variants */
    const char** variant_names;             /* Variant names */

    /* For function types */
    struct ZrtlTypeDescriptor* return_type; /* Return type */
    uint8_t param_count;                    /* Number of parameters */
    struct ZrtlTypeDescriptor** param_types;/* Parameter types */
} ZrtlTypeDescriptor;

/* ============================================================
 * Type Registry - For Registering Custom Types
 * ============================================================
 *
 * Plugins can register custom types to get unique type IDs.
 * This allows type-safe handling of opaque struct types.
 */

/* Type info for registered custom types */
typedef struct {
    const char* name;          /* Type name (e.g., "MyPoint") */
    uint32_t size;             /* Size of the type in bytes */
    uint32_t alignment;        /* Alignment requirement */
    ZrtlDropFn dropper;        /* Optional destructor */
    ZrtlTypeCategory category; /* Type category */
} ZrtlTypeInfo;

/* Maximum number of custom types per plugin */
#define ZRTL_MAX_CUSTOM_TYPES 256

/* Register a custom type (returns type ID, or 0 on failure) */
/* Note: Implementation provided by the Zyntax runtime */
typedef uint16_t (*ZrtlRegisterTypeFn)(const ZrtlTypeInfo* info);

/* Create a tag for a registered custom type */
#define ZRTL_CUSTOM_TAG(type_id, flags) \
    ZRTL_MAKE_TAG(ZRTL_CAT_STRUCT, type_id, flags)

/* Macro to define a custom struct type */
#define ZRTL_DEFINE_TYPE(type_name, struct_type) \
    static ZrtlTypeInfo type_name##_type_info = { \
        .name = #type_name, \
        .size = sizeof(struct_type), \
        .alignment = _Alignof(struct_type), \
        .dropper = NULL, \
        .category = ZRTL_CAT_STRUCT \
    }

/* Macro to define a custom type with destructor */
#define ZRTL_DEFINE_TYPE_WITH_DROP(type_name, struct_type, drop_fn) \
    static ZrtlTypeInfo type_name##_type_info = { \
        .name = #type_name, \
        .size = sizeof(struct_type), \
        .alignment = _Alignof(struct_type), \
        .dropper = (ZrtlDropFn)(drop_fn), \
        .category = ZRTL_CAT_STRUCT \
    }

/* ============================================================
 * Common type definitions for runtime functions
 * ============================================================ */

/* Zyntax string type (UTF-8, length-prefixed) */
typedef struct {
    int32_t length;     /* Length in bytes (not including null terminator) */
    char* data;         /* UTF-8 encoded string data */
} ZrtlString;

/* Zyntax array type */
typedef struct {
    int32_t length;     /* Number of elements */
    int32_t capacity;   /* Allocated capacity */
    void* data;         /* Pointer to element data */
} ZrtlArray;

/* Zyntax optional type */
typedef struct {
    uint8_t has_value;  /* 0 = none, 1 = some */
    void* value;        /* Pointer to value if has_value == 1 */
} ZrtlOptional;

/* ============================================================
 * Helper macros for common patterns
 * ============================================================ */

/* Define a method with standard naming convention */
#define ZRTL_METHOD(type, name) \
    ZRTL_SYMBOL("$" #type "$" #name, type##_##name)

/* Define multiple methods for a type */
#define ZRTL_TYPE_BEGIN(type) /* Start type definition */
#define ZRTL_TYPE_END /* End type definition */

/* ============================================================
 * Memory management helpers
 * ============================================================ */

/* Allocate memory (use platform malloc) */
static inline void* zrtl_alloc(size_t size) {
    extern void* malloc(size_t);
    return malloc(size);
}

/* Free memory (use platform free) */
static inline void zrtl_free(void* ptr) {
    extern void free(void*);
    free(ptr);
}

/* Reallocate memory */
static inline void* zrtl_realloc(void* ptr, size_t size) {
    extern void* realloc(void*, size_t);
    return realloc(ptr, size);
}

/* ============================================================
 * String helpers
 * ============================================================ */

/* Create a ZrtlString from a C string */
static inline ZrtlString zrtl_string_from_cstr(const char* cstr) {
    extern size_t strlen(const char*);
    extern void* memcpy(void*, const void*, size_t);

    ZrtlString str;
    str.length = (int32_t)strlen(cstr);
    str.data = (char*)zrtl_alloc(str.length + 1);
    memcpy(str.data, cstr, str.length + 1);
    return str;
}

/* Free a ZrtlString */
static inline void zrtl_string_free(ZrtlString* str) {
    if (str->data) {
        zrtl_free(str->data);
        str->data = NULL;
        str->length = 0;
    }
}

/* ============================================================
 * Array helpers
 * ============================================================ */

/* Create a new array with given element size */
static inline ZrtlArray zrtl_array_new(size_t element_size, int32_t initial_capacity) {
    ZrtlArray arr;
    arr.length = 0;
    arr.capacity = initial_capacity > 0 ? initial_capacity : 8;
    arr.data = zrtl_alloc(element_size * arr.capacity);
    return arr;
}

/* Free an array */
static inline void zrtl_array_free(ZrtlArray* arr) {
    if (arr->data) {
        zrtl_free(arr->data);
        arr->data = NULL;
        arr->length = 0;
        arr->capacity = 0;
    }
}

#ifdef __cplusplus
}
#endif

#endif /* ZRTL_H */
