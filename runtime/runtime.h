
#include <stdint.h>
#include <stdlib.h>

typedef void*(*WTMalloc)(size_t);
typedef void(*WTFree)(void*);

#define WT_NO_GC 0xFFFFFFFF

typedef enum WTObjectType {
    WT_BOXED_F64 = -44,
    WT_BOXED_U64 = -33,
    WT_BOXED_I64 = -32,
    WT_CLASS_OBJECT = -20,
    WT_ARRAY = -16,
    WT_CLASS_OBJECT_META = -4,
    WT_LAMBDA = -3,
    WT_SYMBOL = -2,
    WT_STRING = -1,
    WT_NULL = 0,
    WT_I32 = 1,
    WT_F32 = 2,
    WT_BOOL = 3,
} WTObjectType;

#define WT_OBJ_HEADER WTObjectHeader header;

typedef struct WTObjectHeader {
    uint32_t     count;
} WTObjectHeader;

typedef struct WTObject {
    WT_OBJ_HEADER
} WTObject;

// #if INTPTR_MAX >= INT64_MAX
// #define WT_PTR64
// #endif

#ifdef WT_PTR64

// in 64bit mode, WTValue is 128bit
// int64_t and double are encoded in the value
typedef struct WTValue {
    WTObjectType type;
    union {
        int32_t   i32_val;  // bool
        float     f32_val;
        WTObject* ptr_val;
        double    f64_val;
        int64_t   i64_val;
    };
} WTValue;

#else

// in 64bit mode, WTValue is 128bit
// int64_t and double are encoded in the value
typedef struct WTValue {
    WTObjectType type;
    union {
        int       int_val;  // bool
        float     float_val;
        WTObject* ptr_val;
    };
} WTValue;

#define MK_NULL (WTValue) { WT_NULL, { .int_val = 0 } }

#endif

typedef struct WTString {
    WT_OBJ_HEADER
    uint32_t length;
    uint32_t hash;
    unsigned char content[];
} WTString;

typedef struct WTSymbolBucket {
    WTValue content;
    struct WTSymbolBucket* next;
} WTSymbolBucket;

typedef struct WTBoxedI64 {
    WT_OBJ_HEADER
    int64_t value;
} WTBoxedI64;

typedef struct WTBoxedU64 {
    WT_OBJ_HEADER
    int64_t value;
} WTBoxedU64;

typedef struct WTBoxedF64 {
    WT_OBJ_HEADER
    double value;
} WTBoxedF64;

typedef struct WTRuntime {
    WTMalloc malloc_method;
    WTFree free_method;
    uint32_t seed;
    WTSymbolBucket* symbol_buckets;
    uint32_t symbol_bucket_size;
    uint32_t symbol_len;
    WTValue* i64_pool;
} WTRuntime;

typedef struct WTProgram {
    WTRuntime* runtime;
} WTProgram;

WTRuntime* WTNewRuntime();
void WTFreeRuntime(WTRuntime* rt);

void WTRetain(WTValue obj);
void WTRelease(WTRuntime* rt, WTValue obj);

typedef struct WTLambda {
    WT_OBJ_HEADER
    void*   c_fun;
    size_t  captured_values_size;
    WTValue captured_values[];
} WTLambda;

WTValue WTNewStringFromCStringLen(WTRuntime* rt, const unsigned char* content, uint32_t len);
WTValue WTNewStringFromCString(WTRuntime* rt, const unsigned char* content);

WTValue WTNewSymbolLen(WTRuntime* rt, const char* content, uint32_t len);
WTValue WTNewSymbol(WTRuntime* rt, const char* content);

typedef struct WTArray {
    WT_OBJ_HEADER
    uint32_t len;
    uint32_t capacity;
    WTValue* data;
} WTArray;

// A global unique ID for class method
typedef uint64_t WTClassObjectMethodID;

typedef struct WTClassObjectMethod {
    WTClassObjectMethodID id;
    const char* name;
    void* fun_ptr;
    struct WTClassObjectMethod* next;
} WTClassObjectMethod;

typedef struct WTClassObjectMeta {
    WT_OBJ_HEADER
    const char* name;  // class name
    WTClassObjectMethod* methods;
} WTClassObjectMeta;

// ClassClassObjectMethodString("hello")  // slow
// ClassClassObjectMethodID(123)  // quick
typedef struct WTClassObject {
    WT_OBJ_HEADER
    WTClassObjectMeta* meta;
    size_t    properties_size;
    WTValue   properties[];
} WTClassObject;

WTClassObject* WTNewClassObject(WTRuntime* rt, WTClassObjectMeta* meta, uint32_t slot_count);
