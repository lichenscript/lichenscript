
#include <stdint.h>
#include <stdlib.h>

typedef void*(*WTLMalloc)(size_t);
typedef void(*WTLFree)(void*);

#define WTL_NO_GC 0xFFFFFFFF

typedef enum WTLObjectType {
    WTL_STRING = 1,
    WTL_SYMBOL,
    WTL_CLASS_OBJECT_META,
    WTL_CLASS_OBJECT,
    WTL_ARRAY8 = 16,
    WTL_ARRAY16,
    WTL_ARRAY32,
    WTL_ARRAY64,
} WTLObjectType;

#define WTL_OBJ_HEADER WTLObjectHeader header;

typedef struct WTLObjectHeader {
    uint32_t      count;
    WTLObjectType type;
} WTLObjectHeader;

typedef struct WTLObject {
    WTL_OBJ_HEADER
} WTLObject;

typedef struct WTLString {
    WTL_OBJ_HEADER
    uint32_t length;
    uint32_t hash;
    unsigned char content[];
} WTLString;

typedef struct WTLSymbolBucket {
    WTLString* content;
    struct WTLSymbolBucket* next;
} WTLSymbolBucket;

typedef struct WTLRuntime {
    WTLMalloc malloc_method;
    WTLFree free_method;
    uint32_t seed;
    WTLSymbolBucket* symbol_buckets;
    uint32_t symbol_bucket_size;
    uint32_t symbol_len;
} WTLRuntime;

typedef struct WTLProgram {
    WTLRuntime* runtime;
} WTLProgram;

WTLRuntime* WTLNewRuntime();

void WTLRetain(WTLObject* obj);
void WTLRelease(WTLRuntime* rt, WTLObject* obj);

WTLString* WTLNewStringFromCStringLen(WTLRuntime* rt, const unsigned char* content, uint32_t len);
WTLString* WTLNewStringFromCString(WTLRuntime* rt, const unsigned char* content);

WTLString* WTLNewSymbolLen(WTLRuntime* rt, const char* content, uint32_t len);
WTLString* WTLNewSymbol(WTLRuntime* rt, const char* content);

typedef struct WTLArray8 {
    WTL_OBJ_HEADER
    uint32_t len;
    uint32_t capacity;
    uint8_t  data[];
} WTLArray8;

typedef struct WTLArray32 {
    WTL_OBJ_HEADER
    uint32_t len;
    uint32_t capacity;
    uint32_t data[];
} WTLArray32;

typedef struct WTLArray64 {
    WTL_OBJ_HEADER
    uint32_t len;
    uint32_t capacity;
    uint64_t data[];
} WTLArray64;

// A global unique ID for class method
typedef uint64_t WTLClassObjectMethodID;

typedef struct WTLClassObjectMethod {
    WTLClassObjectMethodID id;
    const char* name;
    void* fun_ptr;
    struct WTLClassObjectMethod* next;
} WTLClassObjectMethod;

typedef struct WTLClassObjectMeta {
    WTL_OBJ_HEADER
    const char* name;  // class name
    WTLClassObjectMethod* methods;
} WTLClassObjectMeta;

// ClassClassObjectMethodString("hello")  // slow
// ClassClassObjectMethodID(123)  // quick
typedef struct WTLClassObject {
    WTL_OBJ_HEADER
    WTLClassObjectMeta* meta;
    uint64_t properties[];
} WTLClassObject;

WTLClassObject* WTLNewClassObject(WTLRuntime* rt, WTLClassObjectMeta* meta, uint32_t slot_count);
