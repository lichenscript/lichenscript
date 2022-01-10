
#include "runtime.h"
#include "string.h"
#include "time.h"
#include "stdio.h"

#define LC_INIT_SYMBOL_BUCKET_SIZE 128
#define I64_POOL_SIZE 1024

static inline uint32_t hash_string8(const uint8_t *str, size_t len, uint32_t h)
{
    size_t i;

    for(i = 0; i < len; i++)
        h = h * 263 + str[i];
    return h;
}

static inline uint32_t hash_string16(const uint16_t *str,
                                     size_t len, uint32_t h)
{
    size_t i;

    for(i = 0; i < len; i++)
        h = h * 263 + str[i];
    return h;
}

// -511 - 512 is the range in the pool
static LCValue* InitI64Pool(LCMalloc malloc_method) {
    LCValue* result = malloc_method(sizeof(LCBox64) * I64_POOL_SIZE);

    int i;
    for (i = 0; i < I64_POOL_SIZE; i++) {
        int val = I64_POOL_SIZE / 2 - i;
        LCBox64* ptr = malloc_method(sizeof(LCBox64));
        ptr->i64 = val;
        ptr->header.count = LC_NO_GC;
        result[i].type = LC_TY_BOXED_I64;
        result[i].ptr_val = (LCObject*)ptr;
    }

    return result;
}

static void FreeObject(LCRuntime* rt, LCValue val);

static void FreeLambda(LCRuntime* rt, LCLambda* obj) {
    size_t i;
    for (i = 0; i < obj->captured_values_size; i++) {
        LCRelease(rt, obj->captured_values[i]);
    }

    rt->free_method(obj);
}

static void FreeClassObject(LCRuntime* rt, LCClassObject* clsObj) {
    size_t i;
    for (i = 0; i < clsObj->properties_size; i++) {
        LCRelease(rt, clsObj->properties[i]);
    }

    rt->free_method(clsObj);
}

static void FreeClassObjectMeta(LCRuntime* rt, LCClassObjectMeta* meta) {

    rt->free_method(meta);
}

static void FreeArray(LCRuntime* rt, LCArray* arr) {
    uint32_t i;
    for (i = 0; i < arr->len; i++) {
        LCRelease(rt, arr->data[i]);
    }
}

static void FreeObject(LCRuntime* rt, LCValue val) {
    switch (val.type) {
    case LC_TY_LAMBDA:
        FreeLambda(rt, (LCLambda*)val.ptr_val);
        break;

    case LC_TY_CLASS_OBJECT:
        FreeClassObject(rt, (LCClassObject*)val.ptr_val);
        break;

    case LC_TY_ARRAY:
        FreeArray(rt, (LCArray*)val.ptr_val);
        break;
        
    case LC_TY_STRING:
    case LC_TY_SYMBOL:
    case LC_TY_CLASS_OBJECT_META:
    case LC_TY_BOXED_I64:
    case LC_TY_BOXED_U64:
    case LC_TY_BOXED_F64:
        rt->free_method(val.ptr_val);
        break;
    
    default:
        if (val.type >= LC_TY_MAX) {
            // variant
        } else {
            printf("[waterlang] internal error, unkown type: %d\n", val.type);
            abort();
        }
        break;
    }
}

LCRuntime* LCNewRuntime() {
    LCRuntime* runtime = (LCRuntime*)malloc(sizeof(LCRuntime));
    runtime->malloc_method = malloc;
    runtime->free_method = free;

    runtime->seed = time(NULL);

    size_t bucket_size = sizeof(LCSymbolBucket) * LC_INIT_SYMBOL_BUCKET_SIZE;
    LCSymbolBucket* buckets = runtime->malloc_method(bucket_size);
    memset(buckets, 0, bucket_size);

    runtime->symbol_buckets = buckets;
    runtime->symbol_bucket_size = LC_INIT_SYMBOL_BUCKET_SIZE;
    runtime->symbol_len = 0;

    runtime->i64_pool = InitI64Pool(runtime->malloc_method);

    return runtime;
}

void LCFreeRuntime(LCRuntime* rt) {
    uint32_t i;
    LCSymbolBucket* bucket_ptr;
    LCSymbolBucket* next;
    for (i = 0; i < rt->symbol_bucket_size; i++) {
        bucket_ptr = rt->symbol_buckets[i].next;
        while (bucket_ptr != NULL) {
            next = bucket_ptr->next;
            FreeObject(rt, bucket_ptr->content);
            bucket_ptr = next;
        }
    }

    free(rt->symbol_buckets);

    free(rt->i64_pool);

    free(rt);
}

void LCRetain(LCValue val) {
    if (val.type >= 0) {
        return;
    }
    if (val.ptr_val->header.count == LC_NO_GC) {
        return;
    }
    val.ptr_val->header.count++;
}

void LCRelease(LCRuntime* rt, LCValue val) {
    if (val.type <= 0) {
        return;
    }
    if (val.ptr_val->header.count == LC_NO_GC) {
        return;
    }
    if (--val.ptr_val->header.count == 0) {
        FreeObject(rt, val);
    }
}

void LCInitObject(LCObjectHeader* header, LCObjectType obj_type) {
    header->count = 1;
}

LCValue LCNewStringFromCStringLen(LCRuntime* rt, const unsigned char* content, uint32_t len) {
    uint32_t acquire_len = sizeof(LCString) + len + 1;
    LCString* result = rt->malloc_method(acquire_len);
    memset(result, 0, acquire_len);

    LCInitObject(&result->header, LC_TY_STRING);

    memcpy(result->content, content, len);
    
    return (LCValue){ { .ptr_val = (LCObject*)result }, LC_TY_STRING };
}

LCValue LCNewStringFromCString(LCRuntime* rt, const unsigned char* content) {
    return LCNewStringFromCStringLen(rt, content, strlen((const char*)content));
}

LCValue LCNewSymbolLen(LCRuntime* rt, const char* content, uint32_t len) {
    LCValue result = MK_NULL();
    // LCString* result = NULL;
    uint32_t symbol_hash = hash_string8((const uint8_t*)content, len, rt->seed);
    uint32_t symbol_bucket_index = symbol_hash % rt->symbol_bucket_size;

    LCSymbolBucket* bucket_at_index = &rt->symbol_buckets[symbol_bucket_index];
    LCSymbolBucket* new_bucket = NULL;
    LCString* str_ptr = NULL;

    while (1) {
        if (bucket_at_index->content.type == LC_TY_NULL) {
            break;
        }

        str_ptr = (LCString*)bucket_at_index->content.ptr_val;
        if (strcmp((const char *)str_ptr->content, content) == 0) {
            result = bucket_at_index->content;
            break;
        }

        if (bucket_at_index->next == NULL) {
            break;
        }
        bucket_at_index = bucket_at_index->next;
    }

    // symbol not found
    if (result.type == LC_TY_NULL) {
        result = LCNewStringFromCStringLen(rt, (const unsigned char*)content, len);
        result.type = LC_TY_SYMBOL;
        str_ptr = (LCString*)result.ptr_val;
        str_ptr->header.count = LC_NO_GC;
        str_ptr->hash = hash_string8((const unsigned char*)content, len, rt->seed);

        if (bucket_at_index->content.type == LC_TY_NULL) {
            bucket_at_index->content = result;
        } else {
            new_bucket = malloc(sizeof(LCSymbolBucket));
            new_bucket->content = result;
            new_bucket->next = NULL;

            bucket_at_index->next = new_bucket;
        }
        rt->symbol_len++;
    }

    // TODO: enlarge symbol map

    return result;
}

LCValue LCNewSymbol(LCRuntime* rt, const char* content) {
    return LCNewSymbolLen(rt, content, strlen(content));
}

LCClassObject* LCNewClassObject(LCRuntime* rt, LCClassObjectMeta* meta, uint32_t slot_count) {
    uint32_t acquire_len = sizeof(LCClassObject) + sizeof(uint64_t) * slot_count;
    LCClassObject* result = rt->malloc_method(sizeof(LCClassObject));
    memset(result, 0, acquire_len);

    LCInitObject(&result->header, LC_TY_CLASS_OBJECT);
    
    return result;
}

LCValue LCRunMain(LCProgram* program) {
    if (program->main_fun == NULL) {
        return MK_NULL();
    }
    return program->main_fun(program->runtime, MK_NULL(), 0, NULL);
}

static void std_print_string(LCString* str) {
    printf("%s", str->content);
}

static void std_print_val(LCRuntime* rt, LCValue val) {
    switch (val.type)
    {
    case LC_TY_BOOL:
        if (val.int_val) {
            printf("true");
        } else {
            printf("false");
        }
        break;

    case LC_TY_F32:
        printf("%f", val.float_val);
        break;

    case LC_TY_I32:
        printf("%d", val.int_val);
        break;

    case LC_TY_NULL:
        printf("()");
        break;

    case LC_TY_STRING:
        std_print_string((LCString*)val.ptr_val);
        break;
    
    default:
        break;
    }

}

LCValue lc_std_print(LCRuntime* rt, LCValue this, int arg_len, LCValue* args) {
    for (int i = 0; i < arg_len; i++) {
        std_print_val(rt, args[i]);
    }
    printf("\n");
    return MK_NULL();
}
