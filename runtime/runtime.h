
#include <stdint.h>
#include <stdlib.h>

#define LC_NO_GC 0xFFFFFFFF

typedef enum LCObjectType {
    LC_TY_BOOL = -3,
    LC_TY_F32 = -2,
    LC_TY_I32 = -1,
    LC_TY_NULL = 0,
    LC_TY_STRING = 1,
    LC_TY_SYMBOL,
    LC_TY_LAMBDA,
    LC_TY_CLASS_OBJECT_META,
    LC_TY_ARRAY = 16,
    LC_TY_CLASS_OBJECT,
    LC_TY_BOXED_I64 = 24,
    LC_TY_BOXED_U64,
    LC_TY_BOXED_F64,
    LC_TY_MAX = 63,
} LCObjectType;

#define LC_OBJ_HEADER LCObjectHeader header;

typedef struct LCObjectHeader {
    uint32_t     count;
} LCObjectHeader;

typedef struct LCObject {
    LC_OBJ_HEADER
} LCObject;

// #if INTPTR_MAX >= INT64_MAX
// #define LC_PTR64
// #endif

#ifdef LC_PTR64

// in 64bit mode, LCValue is 128bit
// int64_t and double are encoded in the value
typedef struct LCValue {
    LCObjectType type;
    union {
        int32_t   i32_val;  // bool
        float     f32_val;
        LCObject* ptr_val;
        double    f64_val;
        int64_t   i64_val;
    };
} LCValue;

#else

// in 64bit mode, LCValue is 128bit
// int64_t and double are encoded in the value
typedef struct LCValue {
    union {
        int       int_val;  // bool
        float     float_val;
        LCObject* ptr_val;
    };
    LCObjectType type;
} LCValue;

static LCValue LCTrue = { { .int_val = 1 }, LC_TY_BOOL };
static LCValue LCFalse = { { .int_val = 0 }, LC_TY_BOOL };

#define MK_NULL() ((LCValue) { { .int_val = 0 }, LC_TY_NULL })
#define MK_I32(v) ((LCValue) { { .int_val = v }, LC_TY_I32 })
#define MK_F32(v) ((LCValue) { { .float_val = v }, LC_TY_F32 })
#define MK_BOOL(v) ((LCValue) { { .int_val = v }, LC_TY_BOOL })
#define MK_VARIANT_CLOSED(v) (LCValue) { { .int_val = v }, (LC_TY_MAX + (v << 6)) }
#define LC_I32_EQ(l, r) MK_BOOL((l).int_val == (r).int_val)
#define LC_I32_NOT_EQ(l, r) MK_BOOL((l).int_val != (r).int_val)
#define LC_I32_LT(l, r) MK_BOOL((l).int_val < (r).int_val)
#define LC_I32_LTEQ(l, r) MK_BOOL((l).int_val <= (r).int_val)
#define LC_I32_GT(l, r) MK_BOOL((l).int_val > (r).int_val)
#define LC_I32_GTEQ(l, r) MK_BOOL((l).int_val >= (r).int_val)
#define LC_I32_PLUS(l, r) MK_I32((l).int_val + (r).int_val)
#define LC_I32_MINUS(l, r) MK_I32((l).int_val - (r).int_val)
#define LC_I32_MULT(l, r) MK_I32((l).int_val * (r).int_val)
#define LC_I32_DIV(l, r) MK_I32((l).int_val / (r).int_val)
#define LC_I32_LEFT_SHIFT(l, r) MK_I32((l).int_val << (r).int_val)
#define LC_I32_RIGHT_SHIFT(l, r) MK_I32((l).int_val >> (r).int_val)
#define LC_I32_BIT_OR(l, r) MK_I32((l).int_val | (r).int_val)
#define LC_I32_BIT_AND(l, r) MK_I32((l).int_val & (r).int_val)

#endif

typedef struct LCString {
    LC_OBJ_HEADER
    uint32_t length;
    uint32_t hash;
    unsigned char content[];
} LCString;

typedef struct LCSymbolBucket {
    LCValue content;
    struct LCSymbolBucket* next;
} LCSymbolBucket;

typedef struct LCBox64 {
    LC_OBJ_HEADER
    union {
        int64_t  i64;
        uint64_t u64;
        double   f64;
    };
} LCBox64;

typedef struct LCMallocState {
    size_t malloc_count;
    size_t malloc_size;
    size_t malloc_limit;
} LCMallocState;

typedef struct LCRuntime LCRuntime;

typedef LCValue (*LCCFunction)(LCRuntime* rt, LCValue this, int32_t arg_len, LCValue* args);

typedef struct LCProgram {
    LCRuntime* runtime;
    LCCFunction main_fun;
} LCProgram;

LCValue LCRunMain(LCProgram* program);

LCRuntime* LCNewRuntime();
void LCFreeRuntime(LCRuntime* rt);

void* lc_malloc(LCRuntime* rt, size_t size);
void* lc_mallocz(LCRuntime* rt, size_t size);
void lc_free(LCRuntime* rt, void *);

void LCRetain(LCValue obj);
void LCRelease(LCRuntime* rt, LCValue obj);

typedef struct LCLambda {
    LC_OBJ_HEADER
    void*   c_fun;
    size_t  captured_values_size;
    LCValue captured_values[];
} LCLambda;

LCValue LCNewStringFromCStringLen(LCRuntime* rt, const unsigned char* content, uint32_t len);
LCValue LCNewStringFromCString(LCRuntime* rt, const unsigned char* content);

LCValue LCNewSymbolLen(LCRuntime* rt, const char* content, uint32_t len);
LCValue LCNewSymbol(LCRuntime* rt, const char* content);

typedef struct LCArray {
    LC_OBJ_HEADER
    uint32_t len;
    uint32_t capacity;
    LCValue* data;
} LCArray;

// A global unique ID for class method
typedef uint64_t LCClassObjectMethodID;

typedef struct LCClassObjectMethod {
    LCClassObjectMethodID id;
    const char* name;
    void* fun_ptr;
    struct LCClassObjectMethod* next;
} LCClassObjectMethod;

typedef struct LCClassObjectMeta {
    LC_OBJ_HEADER
    const char* name;  // class name
    LCClassObjectMethod* methods;
} LCClassObjectMeta;

// ClassClassObjectMethodString("hello")  // slow
// ClassClassObjectMethodID(123)  // quick
typedef struct LCClassObject {
    LC_OBJ_HEADER
    LCClassObjectMeta* meta;
    size_t    properties_size;
    LCValue   properties[];
} LCClassObject;

LCClassObject* LCNewClassObject(LCRuntime* rt, LCClassObjectMeta* meta, uint32_t slot_count);

LCValue lc_std_print(LCRuntime* rt, LCValue this, int arg_len, LCValue* args);
