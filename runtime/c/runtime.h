
#include <stdint.h>
#include <stdlib.h>

#define likely(x)       __builtin_expect(!!(x), 1)
#define unlikely(x)     __builtin_expect(!!(x), 0)
#define force_inline inline __attribute__((always_inline))
#define no_inline __attribute__((noinline))

#define LC_NO_GC 0xFFFFFFFF
#ifndef countof
#define countof(x) (sizeof(x) / sizeof((x)[0]))
#endif

typedef enum LCObjectType {
    LC_TY_I32 = -4,
    LC_TY_F32 = -3,
    LC_TY_BOOL = -2,
    LC_TY_UNION = -1,
    LC_TY_NULL = 0,
    LC_TY_UNION_OBJECT = 1,
    LC_TY_STRING,
    LC_TY_SYMBOL,
    LC_TY_REFCELL,
    LC_TY_LAMBDA,
    LC_TY_CLASS_OBJECT_META,
    LC_TY_ARRAY,
    LC_TY_CLASS_OBJECT,
    LC_TY_BOXED_I64,
    LC_TY_BOXED_U64,
    LC_TY_BOXED_F64,
    LC_TY_MAX = 127,
} LCObjectType;

typedef enum LCArithmeticType {
    LC_ARTH_PLUS = 1,
    LC_ARTH_MINUS,
    LC_ARTH_MULT,
    LC_ARTH_DIV,
    LC_ARTH_MOD,
    LC_ARTH_LSHIFT,
    LC_ARTH_RSHIFT,
    LC_ARTH_BIT_OR,
    LC_ARTH_BIT_XOR,
    LC_ARTH_BIT_AND,
} LCArithmeticType;

#define LC_OBJ_HEADER LCObjectHeader header;

typedef struct LCObjectHeader {
    uint32_t     count;
    uint32_t     class_id;
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
    union {
        int32_t   i32_val;  // bool
        float     f32_val;
        LCObject* ptr_val;
        double    f64_val;
        int64_t   i64_val;
    };
    int32_t tag;
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
    int64_t tag;
} LCValue;

static LCValue LCTrue = { { .int_val = 1 }, LC_TY_BOOL };
static LCValue LCFalse = { { .int_val = 0 }, LC_TY_BOOL };

#define MK_NULL() ((LCValue) { { .int_val = 0 }, LC_TY_NULL })
#define MK_I32(v) ((LCValue) { { .int_val = v }, LC_TY_I32 })
#define MK_F32(v) ((LCValue) { { .float_val = v }, LC_TY_F32 })
#define MK_BOOL(v) ((LCValue) { { .int_val = (v) }, LC_TY_BOOL })
#define MK_VARIANT_CLOSED(v) ((LCValue) { { .int_val = v }, (LC_TY_MAX + (v << 6)) })
#define MK_CLASS_OBJ(obj) ((LCValue){ { .ptr_val = (LCObject*)obj }, LC_TY_CLASS_OBJECT })
#define MK_UNION(v) ((LCValue) { { .int_val = v }, LC_TY_UNION })
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
#define LC_I32_MOD(l, r) MK_I32((l).int_val % (r).int_val)
#define LC_I32_LEFT_SHIFT(l, r) MK_I32((l).int_val << (r).int_val)
#define LC_I32_RIGHT_SHIFT(l, r) MK_I32((l).int_val >> (r).int_val)
#define LC_I32_BIT_OR(l, r) MK_I32((l).int_val | (r).int_val)
#define LC_I32_BIT_AND(l, r) MK_I32((l).int_val & (r).int_val)
#define LCCast(v, CNAME) ((CNAME)((v).ptr_val))

#endif

typedef struct LCString {
    LC_OBJ_HEADER
    uint32_t length: 31;
    uint8_t is_wide_char : 1;

    uint32_t hash;
    union {
        uint8_t str8[0];
        uint16_t str16[0];
    } u;
} LCString;

typedef struct LCSymbolBucket {
    LCValue content;
    struct LCSymbolBucket* next;
} LCSymbolBucket;

typedef struct LCRefCell {
    LC_OBJ_HEADER
    LCValue value;
} LCRefCell;

typedef struct LCUnionObject {
    LC_OBJ_HEADER
    int tag;
    size_t size;
    LCValue value[];
} LCUnionObject;

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
typedef struct LCArray LCArray;

typedef LCValue (*LCCFunction)(LCRuntime* rt, LCValue this, int32_t arg_len, LCValue* args);
typedef void (*LCFinalizer)(LCRuntime* rt, LCValue obj);

typedef struct LCProgram {
    LCRuntime* runtime;
    LCCFunction main_fun;
} LCProgram;

LCValue LCRunMain(LCProgram* program);

LCRuntime* LCNewRuntime();
void LCFreeRuntime(LCRuntime* rt);

void* lc_malloc(LCRuntime* rt, size_t size);
void* lc_mallocz(LCRuntime* rt, size_t size);
void* lc_realloc(LCRuntime* rt, void*, size_t size);
void* lc_realloc2(LCRuntime *ctx, void *ptr, size_t size, size_t *pslack);
void lc_free(LCRuntime* rt, void *);

void LCUpdateValue(LCArithmeticType op, LCValue* left, LCValue right);

void LCRetain(LCValue obj);
void LCRelease(LCRuntime* rt, LCValue obj);
typedef struct LCLambda {
    LC_OBJ_HEADER
    LCCFunction c_fun;
    size_t      captured_values_size;
    LCValue     captured_values[];
} LCLambda;

LCValue LCNewStringFromCStringLen(LCRuntime* rt, const unsigned char* content, uint32_t len);
LCValue LCNewStringFromCString(LCRuntime* rt, const unsigned char* content);

LCValue LCNewRefCell(LCRuntime* rt, LCValue value);
void LCRefCellSetValue(LCRuntime* rt, LCValue cell, LCValue value);
LCValue LCRefCellGetValue(LCValue cell);

LCValue LCNewUnionObject(LCRuntime* rt, int tag, int size, LCValue* args);
int LCUnionGetType(LCValue);

LCValue LCNewLambda(LCRuntime* rt, LCCFunction c_fun, int argc, LCValue* args);
LCValue LCLambdaGetValue(LCRuntime* rt, LCValue lambda, int index);
LCValue* LCLambdaGetValuePointer(LCRuntime* rt, LCValue lambda, int index);
LCValue LCLambdaGetRefValue(LCRuntime* rt, LCValue lambda, int index);
void LCLambdaSetValue(LCRuntime* rt, LCValue lambda, int index, LCValue value);
void LCLambdaSetRefValue(LCRuntime* rt, LCValue lambda, int index, LCValue value);

LCValue LCNewArray(LCRuntime* rt);
LCValue LCNewArrayLen(LCRuntime* rt, size_t size);
LCValue LCArrayGetValue(LCRuntime* rt, LCValue this, int index);
void LCArraySetValue(LCRuntime* rt, LCValue this, int index, LCValue value);

LCValue LCNewSymbolLen(LCRuntime* rt, const char* content, uint32_t len);
LCValue LCNewSymbol(LCRuntime* rt, const char* content);

typedef struct LCClassMethodDef {
    const char* name;
    int flag;
    LCCFunction fun_ptr;
} LCClassMethodDef;

typedef struct LCClassDef {
    const char* name;  // class name
    LCFinalizer finalizer;
} LCClassDef;

typedef uint32_t LCClassID;

LCClassID LCDefineClass(LCRuntime* rt, LCClassDef* cls_def);
void LCDefineClassMethod(LCRuntime* rt, LCClassID cls_id, LCClassMethodDef* cls_method, size_t size);

// dynamic dispatch by str
LCValue LCInvokeStr(LCRuntime* rt, LCValue this, const char* content, int arg_len, LCValue* args);
LCValue LCEvalLambda(LCRuntime* rt, LCValue this, int argc, LCValue* args);
// TODO: dynamic dispatch by ATOM

LCValue lc_std_print(LCRuntime* rt, LCValue this, int arg_len, LCValue* args);
void lc_init_object(LCRuntime* rt, LCClassID cls_id, LCObject* obj);

LCValue lc_std_array_get_length(LCRuntime* rt, LCValue this, int arg_len, LCValue* args);
LCValue lc_std_array_push(LCRuntime* rt, LCValue this, int arg_len, LCValue* args);

LCValue lc_std_string_concat(LCRuntime* rt, LCValue this, int arg_len, LCValue* args);
LCValue lc_std_string_get_length(LCRuntime* rt, LCValue this, int arg_len, LCValue* args);
