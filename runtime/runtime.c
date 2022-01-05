
#include "runtime.h"
#include "string.h"
#include "time.h"
#include "stdio.h"

#define WT_INIT_SYMBOL_BUCKET_SIZE 128
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
static WTValue* InitI64Pool(WTMalloc malloc_method) {
    WTValue* result = malloc_method(sizeof(WTBox64) * I64_POOL_SIZE);

    int i;
    for (i = 0; i < I64_POOL_SIZE; i++) {
        int val = I64_POOL_SIZE / 2 - i;
        WTBox64* ptr = malloc_method(sizeof(WTBox64));
        ptr->i64 = val;
        ptr->header.count = WT_NO_GC;
        result[i].type = WT_TY_BOXED_I64;
        result[i].ptr_val = (WTObject*)ptr;
    }

    return result;
}

static void FreeObject(WTRuntime* rt, WTValue val);

static void FreeLambda(WTRuntime* rt, WTLambda* obj) {
    size_t i;
    for (i = 0; i < obj->captured_values_size; i++) {
        WTRelease(rt, obj->captured_values[i]);
    }

    rt->free_method(obj);
}

static void FreeClassObject(WTRuntime* rt, WTClassObject* clsObj) {
    size_t i;
    for (i = 0; i < clsObj->properties_size; i++) {
        WTRelease(rt, clsObj->properties[i]);
    }

    rt->free_method(clsObj);
}

static void FreeClassObjectMeta(WTRuntime* rt, WTClassObjectMeta* meta) {

    rt->free_method(meta);
}

static void FreeArray(WTRuntime* rt, WTArray* arr) {
    uint32_t i;
    for (i = 0; i < arr->len; i++) {
        WTRelease(rt, arr->data[i]);
    }
}

static void FreeObject(WTRuntime* rt, WTValue val) {
    switch (val.type) {
    case WT_TY_LAMBDA:
        FreeLambda(rt, (WTLambda*)val.ptr_val);
        break;

    case WT_TY_CLASS_OBJECT:
        FreeClassObject(rt, (WTClassObject*)val.ptr_val);
        break;

    case WT_TY_ARRAY:
        FreeArray(rt, (WTArray*)val.ptr_val);
        break;
        
    case WT_TY_STRING:
    case WT_TY_SYMBOL:
    case WT_TY_CLASS_OBJECT_META:
    case WT_TY_BOXED_I64:
    case WT_TY_BOXED_U64:
    case WT_TY_BOXED_F64:
        rt->free_method(val.ptr_val);
        break;
    
    default:
        if (val.type >= WT_TY_MAX) {
            // variant
        } else {
            printf("[waterlang] internal error, unkown type: %d\n", val.type);
            abort();
        }
        break;
    }
}

WTRuntime* WTNewRuntime() {
    WTRuntime* runtime = (WTRuntime*)malloc(sizeof(WTRuntime));
    runtime->malloc_method = malloc;
    runtime->free_method = free;

    runtime->seed = time(NULL);

    size_t bucket_size = sizeof(WTSymbolBucket) * WT_INIT_SYMBOL_BUCKET_SIZE;
    WTSymbolBucket* buckets = runtime->malloc_method(bucket_size);
    memset(buckets, 0, bucket_size);

    runtime->symbol_buckets = buckets;
    runtime->symbol_bucket_size = WT_INIT_SYMBOL_BUCKET_SIZE;
    runtime->symbol_len = 0;

    runtime->i64_pool = InitI64Pool(runtime->malloc_method);

    return runtime;
}

void WTFreeRuntime(WTRuntime* rt) {
    uint32_t i;
    WTSymbolBucket* bucket_ptr;
    WTSymbolBucket* next;
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

void WTRetain(WTValue val) {
    if (val.type >= 0) {
        return;
    }
    if (val.ptr_val->header.count == WT_NO_GC) {
        return;
    }
    val.ptr_val->header.count++;
}

void WTRelease(WTRuntime* rt, WTValue val) {
    if (val.type <= 0) {
        return;
    }
    if (val.ptr_val->header.count == WT_NO_GC) {
        return;
    }
    if (--val.ptr_val->header.count == 0) {
        FreeObject(rt, val);
    }
}

void WTInitObject(WTObjectHeader* header, WTObjectType obj_type) {
    header->count = 1;
}

WTValue WTNewStringFromCStringLen(WTRuntime* rt, const unsigned char* content, uint32_t len) {
    uint32_t acquire_len = sizeof(WTString) + len + 1;
    WTString* result = rt->malloc_method(acquire_len);
    memset(result, 0, acquire_len);

    WTInitObject(&result->header, WT_TY_STRING);

    memcpy(result->content, content, len);
    
    return (WTValue){ { .ptr_val = (WTObject*)result }, WT_TY_STRING };
}

WTValue WTNewStringFromCString(WTRuntime* rt, const unsigned char* content) {
    return WTNewStringFromCStringLen(rt, content, strlen((const char*)content));
}

WTValue WTNewSymbolLen(WTRuntime* rt, const char* content, uint32_t len) {
    WTValue result = MK_NULL();
    // WTString* result = NULL;
    uint32_t symbol_hash = hash_string8((const uint8_t*)content, len, rt->seed);
    uint32_t symbol_bucket_index = symbol_hash % rt->symbol_bucket_size;

    WTSymbolBucket* bucket_at_index = &rt->symbol_buckets[symbol_bucket_index];
    WTSymbolBucket* new_bucket = NULL;
    WTString* str_ptr = NULL;

    while (1) {
        if (bucket_at_index->content.type == WT_TY_NULL) {
            break;
        }

        str_ptr = (WTString*)bucket_at_index->content.ptr_val;
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
    if (result.type == WT_TY_NULL) {
        result = WTNewStringFromCStringLen(rt, (const unsigned char*)content, len);
        result.type = WT_TY_SYMBOL;
        str_ptr = (WTString*)result.ptr_val;
        str_ptr->header.count = WT_NO_GC;
        str_ptr->hash = hash_string8((const unsigned char*)content, len, rt->seed);

        if (bucket_at_index->content.type == WT_TY_NULL) {
            bucket_at_index->content = result;
        } else {
            new_bucket = malloc(sizeof(WTSymbolBucket));
            new_bucket->content = result;
            new_bucket->next = NULL;

            bucket_at_index->next = new_bucket;
        }
        rt->symbol_len++;
    }

    // TODO: enlarge symbol map

    return result;
}

WTValue WTNewSymbol(WTRuntime* rt, const char* content) {
    return WTNewSymbolLen(rt, content, strlen(content));
}

WTClassObject* WTNewClassObject(WTRuntime* rt, WTClassObjectMeta* meta, uint32_t slot_count) {
    uint32_t acquire_len = sizeof(WTClassObject) + sizeof(uint64_t) * slot_count;
    WTClassObject* result = rt->malloc_method(sizeof(WTClassObject));
    memset(result, 0, acquire_len);

    WTInitObject(&result->header, WT_TY_CLASS_OBJECT);
    
    return result;
}

void WTRunMain(WTProgram* program) {
    if (program->main_fun == NULL) {
        return;
    }
    program->main_fun(program->runtime, MK_NULL(), 0, NULL);
}

static void std_print_string(WTString* str) {
    printf("%s", str->content);
}

static void std_print_val(WTRuntime* rt, WTValue val) {
    switch (val.type)
    {
    case WT_TY_BOOL:
        if (val.int_val) {
            printf("true");
        } else {
            printf("false");
        }
        break;

    case WT_TY_F32:
        printf("%f", val.float_val);
        break;

    case WT_TY_I32:
        printf("%d", val.int_val);
        break;

    case WT_TY_NULL:
        printf("()");
        break;

    case WT_TY_STRING:
        std_print_string((WTString*)val.ptr_val);
        break;
    
    default:
        break;
    }

}

WTValue wt_std_print(WTRuntime* rt, WTValue this, uint32_t arg_len, WTValue* args) {
    for (uint32_t i = 0; i < arg_len; i++) {
        std_print_val(rt, args[i]);
    }
    printf("\n");
    return MK_NULL();
}
