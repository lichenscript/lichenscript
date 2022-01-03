
#include "runtime.h"
#include "string.h"
#include "time.h"

#define WTL_INIT_SYMBOL_BUCKET_SIZE 128

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

WTLRuntime* WTLNewRuntime() {
    WTLRuntime* runtime = (WTLRuntime*)malloc(sizeof(WTLRuntime));
    runtime->malloc_method = malloc;
    runtime->free_method = free;

    runtime->seed = time(NULL);

    size_t bucket_size = sizeof(WTLSymbolBucket) * WTL_INIT_SYMBOL_BUCKET_SIZE;
    WTLSymbolBucket* buckets = runtime->malloc_method(bucket_size);
    memset(buckets, 0, bucket_size);

    runtime->symbol_buckets = buckets;
    runtime->symbol_bucket_size = WTL_INIT_SYMBOL_BUCKET_SIZE;
    runtime->symbol_len = 0;

    return runtime;
}

void WTLRetain(WTLObject* obj) {
    if (obj->header.count == WTL_NO_GC) {
        return;
    }
    obj->header.count++;
}

void WTLRelease(WTLRuntime* rt, WTLObject* obj) {
    if (obj->header.count == WTL_NO_GC) {
        return;
    }
    if (--obj->header.count == 0) {
        rt->free_method(obj);
    }
}

void WTLInitObject(WTLObjectHeader* header, WTLObjectType obj_type) {
    header->count = 1;
    header->type = obj_type;
}

WTLString* WTLNewStringFromCStringLen(WTLRuntime* rt, const unsigned char* content, uint32_t len) {
    uint32_t acquire_len = sizeof(WTLString) + len + 1;
    WTLString* result = rt->malloc_method(acquire_len);
    memset(result, 0, acquire_len);

    WTLInitObject(&result->header, WTL_STRING);

    memcpy(result->content, content, len);
    
    return result;
}

WTLString* WTLNewStringFromCString(WTLRuntime* rt, const unsigned char* content) {
    return WTLNewStringFromCStringLen(rt, content, strlen((const char*)content));
}

WTLString* WTLNewSymbolLen(WTLRuntime* rt, const char* content, uint32_t len) {
    WTLString* result = NULL;
    uint32_t symbol_hash = hash_string8((const uint8_t*)content, len, rt->seed);
    uint32_t symbol_bucket_index = symbol_hash % rt->symbol_bucket_size;

    WTLSymbolBucket* bucket_at_index = &rt->symbol_buckets[symbol_bucket_index];
    WTLSymbolBucket* new_bucket = NULL;

    while (1) {
        if (bucket_at_index->content == NULL) {
            break;
        }

        if (strcmp((const char *)bucket_at_index->content->content, content) == 0) {
            result = bucket_at_index->content;
            break;
        }

        if (bucket_at_index->next == NULL) {
            break;
        }
        bucket_at_index = bucket_at_index->next;
    }

    // symbol not found
    if (result == NULL) {
        result = WTLNewStringFromCStringLen(rt, (const unsigned char*)content, len);
        result->header.count = WTL_NO_GC;
        result->header.type = WTL_STRING;
        result->hash = hash_string8((const unsigned char*)content, len, rt->seed);

        if (bucket_at_index->content == NULL) {
            bucket_at_index->content = result;
        } else {
            new_bucket = malloc(sizeof(WTLSymbolBucket));
            new_bucket->content = result;
            new_bucket->next = NULL;

            bucket_at_index->next = new_bucket;
        }
        rt->symbol_len++;
    }

    // TODO: enlarge symbol map

    return result;
}

WTLString* WTLNewSymbol(WTLRuntime* rt, const char* content) {
    return WTLNewSymbolLen(rt, content, strlen(content));
}

WTLClassObject* WTLNewClassObject(WTLRuntime* rt, WTLClassObjectMeta* meta, uint32_t slot_count) {
    uint32_t acquire_len = sizeof(WTLClassObject) + sizeof(uint64_t) * slot_count;
    WTLClassObject* result = rt->malloc_method(sizeof(WTLClassObject));
    memset(result, 0, acquire_len);

    WTLInitObject(&result->header, WTL_CLASS_OBJECT);
    
    return result;
}
