
#include <stdint.h>
#include <stdlib.h>

typedef void*(*WTMalloc)(size_t);
typedef void(*WTFree)(void*);

#define WT_NO_GC 0xFFFFFFFF

typedef enum WTObjectType {
    WT_TY_BOOL = -3,
    WT_TY_F32 = -2,
    WT_TY_I32 = -1,
    WT_TY_NULL = 0,
    WT_TY_STRING = 1,
    WT_TY_SYMBOL,
    WT_TY_LAMBDA,
    WT_TY_CLASS_OBJECT_META,
    WT_TY_ARRAY = 16,
    WT_TY_CLASS_OBJECT,
    WT_TY_BOXED_I64 = 24,
    WT_TY_BOXED_U64,
    WT_TY_BOXED_F64,
    WT_TY_MAX = 63,
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
    union {
        int       int_val;  // bool
        float     float_val;
        WTObject* ptr_val;
    };
    WTObjectType type;
} WTValue;

static WTValue WTTrue = { { .int_val = 1 }, WT_TY_BOOL };
static WTValue WTFalse = { { .int_val = 0 }, WT_TY_BOOL };

#define MK_NULL() (WTValue) { { .int_val = 0 }, WT_TY_NULL }
#define MK_I32(v) (WTValue) { { .int_val = v }, WT_TY_I32 }
#define MK_F32(v) (WTValue) { { .float_val = v }, WT_TY_F32 }
#define MK_BOOL(v) (WTValue) { { .int_val = v }, WT_TY_BOOL }
#define MK_VARIANT_CLOSED(v) (WTValue) { { .int_val = v }, (WT_TY_MAX + (v << 6)) }
#define WT_I32_EQ(l, r) MK_BOOL(l.int_val == r.int_val)
#define WT_I32_NOT_EQ(l, r) MK_BOOL(l.int_val != r.int_val)
#define WT_I32_LT(l, r) MK_BOOL(l.int_val < r.int_val)
#define WT_I32_LTEQ(l, r) MK_BOOL(l.int_val <= r.int_val)
#define WT_I32_GT(l, r) MK_BOOL(l.int_val > r.int_val)
#define WT_I32_GTEQ(l, r) MK_BOOL(l.int_val >= r.int_val)
#define WT_I32_PLUS(l, r) MK_I32(l.int_val + r.int_val)
#define WT_I32_MINUS(l, r) MK_I32(l.int_val - r.int_val)
#define WT_I32_MULT(l, r) MK_I32(l.int_val * r.int_val)
#define WT_I32_DIV(l, r) MK_I32(l.int_val / r.int_val)
#define WT_I32_LEFT_SHIFT(l, r) MK_I32(l.int_val << r.int_val)
#define WT_I32_RIGHT_SHIFT(l, r) MK_I32(l.int_val >> r.int_val)
#define WT_I32_BIT_OR(l, r) MK_I32(l.int_val | r.int_val)
#define WT_I32_BIT_AND(l, r) MK_I32(l.int_val & r.int_val)

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

typedef struct WTBox64 {
    WT_OBJ_HEADER
    union {
        int64_t  i64;
        uint64_t u64;
        double   f64;
    };
} WTBox64;

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
