
#include "runtime.h"
#include "string.h"
#include "time.h"
#include "stdio.h"
#include "math.h"

#if defined(__APPLE__)
#include <malloc/malloc.h>
#endif

#define LC_INIT_SYMBOL_BUCKET_SIZE 128
#define LC_INIT_CLASS_META_CAP 8
#define I64_POOL_SIZE 1024

#define lc_raw_malloc malloc
#define lc_raw_realloc realloc
#define lc_raw_free free

static inline int max_int(int a, int b)
{
    if (a > b)
        return a;
    else
        return b;
}

static inline int min_int(int a, int b)
{
    if (a < b)
        return a;
    else
        return b;
}

/* Note: at most 31 bits are encoded. At most UTF8_CHAR_LEN_MAX bytes
   are output. */
int unicode_to_utf8(uint8_t *buf, unsigned int c)
{
    uint8_t *q = buf;

    if (c < 0x80) {
        *q++ = c;
    } else {
        if (c < 0x800) {
            *q++ = (c >> 6) | 0xc0;
        } else {
            if (c < 0x10000) {
                *q++ = (c >> 12) | 0xe0;
            } else {
                if (c < 0x00200000) {
                    *q++ = (c >> 18) | 0xf0;
                } else {
                    if (c < 0x04000000) {
                        *q++ = (c >> 24) | 0xf8;
                    } else if (c < 0x80000000) {
                        *q++ = (c >> 30) | 0xfc;
                        *q++ = ((c >> 24) & 0x3f) | 0x80;
                    } else {
                        return 0;
                    }
                    *q++ = ((c >> 18) & 0x3f) | 0x80;
                }
                *q++ = ((c >> 12) & 0x3f) | 0x80;
            }
            *q++ = ((c >> 6) & 0x3f) | 0x80;
        }
        *q++ = (c & 0x3f) | 0x80;
    }
    return q - buf;
}

static const unsigned int utf8_min_code[5] = {
    0x80, 0x800, 0x10000, 0x00200000, 0x04000000,
};

static const unsigned char utf8_first_code_mask[5] = {
    0x1f, 0xf, 0x7, 0x3, 0x1,
};

/* return -1 if error. *pp is not updated in this case. max_len must
   be >= 1. The maximum length for a UTF8 byte sequence is 6 bytes. */
int unicode_from_utf8(const uint8_t *p, int max_len, const uint8_t **pp)
{
    int l, c, b, i;

    c = *p++;
    if (c < 0x80) {
        *pp = p;
        return c;
    }
    switch(c) {
    case 0xc0: case 0xc1: case 0xc2: case 0xc3:
    case 0xc4: case 0xc5: case 0xc6: case 0xc7:
    case 0xc8: case 0xc9: case 0xca: case 0xcb:
    case 0xcc: case 0xcd: case 0xce: case 0xcf:
    case 0xd0: case 0xd1: case 0xd2: case 0xd3:
    case 0xd4: case 0xd5: case 0xd6: case 0xd7:
    case 0xd8: case 0xd9: case 0xda: case 0xdb:
    case 0xdc: case 0xdd: case 0xde: case 0xdf:
        l = 1;
        break;
    case 0xe0: case 0xe1: case 0xe2: case 0xe3:
    case 0xe4: case 0xe5: case 0xe6: case 0xe7:
    case 0xe8: case 0xe9: case 0xea: case 0xeb:
    case 0xec: case 0xed: case 0xee: case 0xef:
        l = 2;
        break;
    case 0xf0: case 0xf1: case 0xf2: case 0xf3:
    case 0xf4: case 0xf5: case 0xf6: case 0xf7:
        l = 3;
        break;
    case 0xf8: case 0xf9: case 0xfa: case 0xfb:
        l = 4;
        break;
    case 0xfc: case 0xfd:
        l = 5;
        break;
    default:
        return -1;
    }
    /* check that we have enough characters */
    if (l > (max_len - 1))
        return -1;
    c &= utf8_first_code_mask[l - 1];
    for(i = 0; i < l; i++) {
        b = *p++;
        if (b < 0x80 || b >= 0xc0)
            return -1;
        c = (c << 6) | (b & 0x3f);
    }
    if (c < utf8_min_code[l - 1])
        return -1;
    *pp = p;
    return c;
}

typedef struct LCClassMeta {
    LCClassDef* cls_def;
    LCClassMethodDef* cls_method;
    size_t cls_method_size;
} LCClassMeta;

typedef struct LCRuntime {
    LCMallocState malloc_state;
    uint32_t seed;
    LCSymbolBucket* symbol_buckets;
    uint32_t symbol_bucket_size;
    uint32_t symbol_len;
    LCValue* i64_pool;
    LCBox64* i64_pool_space;
    uint32_t cls_meta_cap;
    uint32_t cls_meta_size;
    LCClassMeta* cls_meta_data;
} LCRuntime;

typedef struct LCArray {
    LC_OBJ_HEADER
    uint32_t len;
    uint32_t capacity;
    LCValue* data;
} LCArray;

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
static LCValue* init_i64_pool(LCRuntime* rt) {
    LCValue* result = (LCValue*)lc_malloc(rt, sizeof(LCValue) * I64_POOL_SIZE);

    rt->i64_pool_space = (LCBox64*)lc_malloc(rt, sizeof (LCBox64) * I64_POOL_SIZE);

    int i;
    for (i = 0; i < I64_POOL_SIZE; i++) {
        int val = I64_POOL_SIZE / 2 - i;
        LCBox64* ptr = rt->i64_pool_space + i;
        ptr->i64 = val;
        ptr->header.count = LC_NO_GC;
        result[i].tag = LC_TY_BOXED_I64;
        result[i].ptr_val = (LCObject*)ptr;
    }

    return result;
}

static void free_i64_pool(LCRuntime* rt) {
    lc_free(rt, rt->i64_pool_space);
    lc_free(rt, rt->i64_pool);
}

static void LCFreeObject(LCRuntime* rt, LCValue val);

static inline void LCFreeLambda(LCRuntime* rt, LCValue val) {
    LCLambda* lambda = (LCLambda*)val.ptr_val; 
    size_t i;
    for (i = 0; i < lambda->captured_values_size; i++) {
        LCRelease(rt, lambda->captured_values[i]);
    }

    lc_free(rt, lambda);
}

/**
 * extract the finalizer from the class definition
 */
static void LCFreeClassObject(LCRuntime* rt, LCValue val) {
    LCObject* clsObj = (LCObject*)val.ptr_val;
    LCClassID cls_id = clsObj->header.class_id;
    LCClassMeta* meta = &rt->cls_meta_data[cls_id];
    LCFinalizer finalizer = meta->cls_def->finalizer;
    if (finalizer) {
        finalizer(rt, val);
    }
    lc_free(rt, val.ptr_val);
}

static inline void LCFreeArray(LCRuntime* rt, LCValue val) {
    LCArray* arr = (LCArray*)val.ptr_val;
    uint32_t i;
    for (i = 0; i < arr->len; i++) {
        LCRelease(rt, arr->data[i]);
    }
    if (arr->data != NULL) {
        lc_free(rt, arr->data);
        arr->data = NULL;
    }
    lc_free(rt, arr);
}

static inline void LCFreeRefCell(LCRuntime* rt, LCValue val) {
    LCRefCell* cell = (LCRefCell*)val.ptr_val;
    LCRelease(rt, cell->value);
    lc_free(rt, cell);
}

static inline void LCFreeUnionObject(LCRuntime* rt, LCValue val) {
    LCUnionObject* union_obj = (LCUnionObject*)val.ptr_val;

    int i;
    for (i = 0; i < union_obj->size; i++) {
        LCRelease(rt, union_obj->value[i]);
    }

    lc_free(rt, union_obj);
}

static void LCFreeObject(LCRuntime* rt, LCValue val) {
    switch (val.tag) {
    case LC_TY_UNION_OBJECT:
        LCFreeUnionObject(rt, val);
        break;

    case LC_TY_REFCELL:
        LCFreeRefCell(rt, val);
        break;

    case LC_TY_LAMBDA:
        LCFreeLambda(rt, val);
        break;

    case LC_TY_CLASS_OBJECT:
        LCFreeClassObject(rt, val);
        break;

    case LC_TY_ARRAY:
        LCFreeArray(rt, val);
        break;
        
    case LC_TY_STRING:
    case LC_TY_SYMBOL:
    case LC_TY_CLASS_OBJECT_META:
    case LC_TY_BOXED_I64:
    case LC_TY_BOXED_U64:
    case LC_TY_BOXED_F64:
        lc_free(rt, val.ptr_val);
        break;
    
    default:
        fprintf(stderr, "[LichenScript] internal error, unkown tag: %lld\n", val.tag);
        abort();

    }
}

void* lc_malloc(LCRuntime* rt, size_t size) {
    void* ptr = lc_raw_malloc(size);
    if (ptr == NULL) {
        return NULL;
    }
    rt->malloc_state.malloc_count++;
    return ptr;
}

void* lc_mallocz(LCRuntime* rt, size_t size) {
    void* ptr = lc_malloc(rt, size);
    if (ptr == NULL) {
        return NULL;
    }
    memset(ptr, 0, size);
    return ptr;
}

void* lc_realloc(LCRuntime* rt, void* ptr, size_t size) {
    return lc_raw_realloc(ptr, size);
}

size_t lc_malloc_usable_size_platform(const void *ptr) {
#if defined(__APPLE__)
    return malloc_size(ptr);
#endif
    return 0;
}

size_t lc_malloc_usable_size(LCRuntime *rt, const void *ptr) {
    return lc_malloc_usable_size_platform(ptr);
}

void *lc_realloc2(LCRuntime *rt, void *ptr, size_t size, size_t *pslack)
{
    void *ret;
    ret = lc_realloc(rt, ptr, size);
    if (unlikely(!ret && size != 0)) {
        return NULL;
    }
    if (pslack) {
        size_t new_size = lc_malloc_usable_size(rt, ret);
        *pslack = (new_size > size) ? new_size - size : 0;
    }
    return ret;
}

void lc_free(LCRuntime* rt, void* ptr) {
    rt->malloc_state.malloc_count--;
    lc_raw_free(ptr);
}

static LCClassDef Object_def = {
    "Object",
    NULL,
};

static LCValue LC_Object_toString(LCRuntime* rt, LCValue this, int argc, LCValue* args) {
    return LCNewStringFromCString(rt, (const unsigned char*)"Object");;
}

static LCClassMethodDef Object_method_def[] = {
    { "toString", 0, LC_Object_toString }
};

LCRuntime* LCNewRuntime() {
    LCRuntime* runtime = (LCRuntime*)lc_raw_malloc(sizeof(LCRuntime));
    memset(runtime, 0, sizeof(LCRuntime));
    runtime->malloc_state.malloc_count = 1;

    runtime->seed = time(NULL);

    size_t bucket_size = sizeof(LCSymbolBucket) * LC_INIT_SYMBOL_BUCKET_SIZE;
    LCSymbolBucket* buckets = lc_mallocz(runtime, bucket_size);

    runtime->symbol_buckets = buckets;
    runtime->symbol_bucket_size = LC_INIT_SYMBOL_BUCKET_SIZE;
    runtime->symbol_len = 0;

    runtime->i64_pool = init_i64_pool(runtime);

    runtime->cls_meta_cap = LC_INIT_CLASS_META_CAP;
    runtime->cls_meta_size = 0;
    runtime->cls_meta_data = lc_malloc(runtime, sizeof(LCClassMeta) * runtime->cls_meta_cap);

    // the ancester of all classes
    LCClassID object_cls_id = LCDefineClass(runtime, &Object_def);
    LCDefineClassMethod(runtime, object_cls_id, Object_method_def, countof(Object_method_def));

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
            LCFreeObject(rt, bucket_ptr->content);
            bucket_ptr = next;
        }
    }

    lc_free(rt, rt->symbol_buckets);

    free_i64_pool(rt);

    lc_free(rt, rt->cls_meta_data);

    if (rt->malloc_state.malloc_count != 1) {
        fprintf(stderr, "[LichenScript] memory leaks %zu\n", rt->malloc_state.malloc_count);
        lc_raw_free(rt);
        exit(1);
    }

    lc_raw_free(rt);
}

void LCUpdateValue(LCArithmeticType op, LCValue* left, LCValue right) {
    LCValue* this = left;
    int ty = this->tag;
    if (ty == LC_TY_REFCELL) {
        LCRefCell* ref_cell = (LCRefCell*)left->ptr_val;
        this = &ref_cell->value;
        ty = this->tag;
    }

    if (ty == LC_TY_I32) {
        switch (op) {
            case LC_ARTH_PLUS:
                this->int_val += right.int_val;
                break;

            case LC_ARTH_MINUS:
                this->int_val -= right.int_val;
                break;

            case LC_ARTH_MULT:
                this->int_val *= right.int_val;
                break;

            case LC_ARTH_DIV:
                this->int_val /= right.int_val;
                break;

            case LC_ARTH_MOD:
                this->int_val %= right.int_val;
                break;

            case LC_ARTH_LSHIFT:
                this->int_val <<= right.int_val;
                break;

            case LC_ARTH_RSHIFT:
                this->int_val >>= right.int_val;
                break;

            case LC_ARTH_BIT_OR:
                this->int_val |= right.int_val;
                break;

            case LC_ARTH_BIT_XOR:
                this->int_val ^= right.int_val;
                break;

            case LC_ARTH_BIT_AND:
                this->int_val &= right.int_val;
                break;

        }
    } else if (ty == LC_TY_F32) {
        switch (op) {
            case LC_ARTH_PLUS:
                this->float_val += right.float_val;
                break;

            case LC_ARTH_MINUS:
                this->float_val -= right.float_val;
                break;

            case LC_ARTH_MULT:
                this->float_val *= right.float_val;
                break;

            case LC_ARTH_DIV:
                this->float_val /= right.float_val;
                break;

            default: {
                fprintf(stderr, "[LichenScript] Can not apply op: %d for type: %d", op, ty);
                abort();
            }

        }
    }
}

void LCRetain(LCValue val) {
    if (val.tag <= 0) {
        return;
    }
    if (val.ptr_val->header.count == LC_NO_GC) {
        return;
    }
    val.ptr_val->header.count++;
}

void LCRelease(LCRuntime* rt, LCValue val) {
    if (val.tag <= 0) {
        return;
    }
    if (val.ptr_val->header.count == LC_NO_GC) {
        return;
    }
    if (--val.ptr_val->header.count == 0) {
        LCFreeObject(rt, val);
    }
}

void LCInitObject(LCObjectHeader* header, LCObjectType obj_type) {
    header->count = 1;
}

/* Note: the string contents are uninitialized */
static LCString *lc_alloc_string_rt(LCRuntime *rt, int max_len, int is_wide_char)
{
    LCString *str;
    str = lc_malloc(rt, sizeof(LCString) + (max_len << is_wide_char) + 1 - is_wide_char);
    if (unlikely(!str)) {
        return NULL;
    }
    str->header.count = 1;
    str->is_wide_char = is_wide_char;
    str->length = max_len;
    str->hash = 0;          /* optional but costless */
    return str;
}

typedef struct StringBuffer {
    LCRuntime *rt;
    LCString *str;
    int len;
    int size;
    int is_wide_char;
    int error_status;
} StringBuffer;

/* It is valid to call string_buffer_end() and all string_buffer functions even
   if string_buffer_init() or another string_buffer function returns an error.
   If the error_status is set, string_buffer_end() returns JS_EXCEPTION.
 */
static int string_buffer_init2(LCRuntime* rt, StringBuffer *s, int size,
                               int is_wide)
{
    s->rt = rt;
    s->size = size;
    s->len = 0;
    s->is_wide_char = is_wide;
    s->error_status = 0;
    s->str = lc_alloc_string_rt(rt, size, is_wide);
    if (unlikely(!s->str)) {
        s->size = 0;
        return s->error_status = -1;
    }
    return 0;
}

static inline int string_buffer_init(LCRuntime* rt, StringBuffer *s, int size)
{
    return string_buffer_init2(rt, s, size, 0);
}

static void string_buffer_free(StringBuffer *s)
{
    lc_free(s->rt, s->str);
    s->str = NULL;
}

static no_inline int string_buffer_widen(StringBuffer *s, int size)
{
    LCString *str;
    size_t slack;
    int i;

    if (s->error_status)
        return -1;

    str = lc_realloc2(s->rt, s->str, sizeof(LCString) + (size << 1), &slack);
    // if (!str)
    //     return string_buffer_set_error(s);
    size += slack >> 1;
    for(i = s->len; i-- > 0;) {
        str->u.str16[i] = str->u.str8[i];
    }
    s->is_wide_char = 1;
    s->size = size;
    s->str = str;
    return 0;
}

#define LC_STRING_LEN_MAX ((1 << 30) - 1)

static no_inline int string_buffer_realloc(StringBuffer *s, int new_len, int c)
{
    LCString *new_str;
    int new_size;
    size_t new_size_bytes, slack;

    if (s->error_status)
        return -1;

    if (new_len > LC_STRING_LEN_MAX) {
        fprintf(stderr, "string too long");
        abort();
    }
    new_size = min_int(max_int(new_len, s->size * 3 / 2), LC_STRING_LEN_MAX);
    if (!s->is_wide_char && c >= 0x100) {
        return string_buffer_widen(s, new_size);
    }
    new_size_bytes = sizeof(LCString) + (new_size << s->is_wide_char) + 1 - s->is_wide_char;
    new_str = lc_realloc2(s->rt, s->str, new_size_bytes, &slack);
    if (!new_str) {
        fprintf(stderr, "malloc memory for string failed");
        abort();
    }
    new_size = min_int(new_size + (slack >> s->is_wide_char), LC_STRING_LEN_MAX);
    s->size = new_size;
    s->str = new_str;
    return 0;
}

static no_inline int string_buffer_putc_slow(StringBuffer *s, uint32_t c)
{
    if (unlikely(s->len >= s->size)) {
        if (string_buffer_realloc(s, s->len + 1, c))
            return -1;
    }
    if (s->is_wide_char) {
        s->str->u.str16[s->len++] = c;
    } else if (c < 0x100) {
        s->str->u.str8[s->len++] = c;
    } else {
        if (string_buffer_widen(s, s->size))
            return -1;
        s->str->u.str16[s->len++] = c;
    }
    return 0;
}

/* 0 <= c <= 0xff */
static int string_buffer_putc8(StringBuffer *s, uint32_t c)
{
    if (unlikely(s->len >= s->size)) {
        if (string_buffer_realloc(s, s->len + 1, c))
            return -1;
    }
    if (s->is_wide_char) {
        s->str->u.str16[s->len++] = c;
    } else {
        s->str->u.str8[s->len++] = c;
    }
    return 0;
}

/* 0 <= c <= 0xffff */
static int string_buffer_putc16(StringBuffer *s, uint32_t c)
{
    if (likely(s->len < s->size)) {
        if (s->is_wide_char) {
            s->str->u.str16[s->len++] = c;
            return 0;
        } else if (c < 0x100) {
            s->str->u.str8[s->len++] = c;
            return 0;
        }
    }
    return string_buffer_putc_slow(s, c);
}

/* 0 <= c <= 0x10ffff */
static int string_buffer_putc(StringBuffer *s, uint32_t c)
{
    if (unlikely(c >= 0x10000)) {
        /* surrogate pair */
        c -= 0x10000;
        if (string_buffer_putc16(s, (c >> 10) + 0xd800))
            return -1;
        c = (c & 0x3ff) + 0xdc00;
    }
    return string_buffer_putc16(s, c);
}

static LCValue lc_new_string8(LCRuntime* rt, const unsigned char* buf, uint32_t buf_len) {
    uint32_t acquire_len = sizeof(LCString) + buf_len + 1;
    LCString* result = lc_mallocz(rt, acquire_len);
    memset(result, 0, acquire_len);

    LCInitObject(&result->header, LC_TY_STRING);

    if (buf_len > 0) {
        memcpy(result->u.str8, buf, buf_len);
    }
    result->is_wide_char = 0;
    result->length = buf_len;
    
    return (LCValue){ { .ptr_val = (LCObject*)result }, LC_TY_STRING };
}

static int string_buffer_write8(StringBuffer *s, const uint8_t *p, int len)
{
    int i;

    if (s->len + len > s->size) {
        if (string_buffer_realloc(s, s->len + len, 0))
            return -1;
    }
    if (s->is_wide_char) {
        for (i = 0; i < len; i++) {
            s->str->u.str16[s->len + i] = p[i];
        }
        s->len += len;
    } else {
        memcpy(&s->str->u.str8[s->len], p, len);
        s->len += len;
    }
    return 0;
}

static LCValue string_buffer_end(StringBuffer *s) {
    LCString *str;
    str = s->str;
    if (s->len == 0) {
        lc_free(s->rt, str);
        s->str = NULL;
        return lc_new_string8(s->rt, NULL, 0);
    }
    if (s->len < s->size) {
        /* smaller size so js_realloc should not fail, but OK if it does */
        /* XXX: should add some slack to avoid unnecessary calls */
        /* XXX: might need to use malloc+free to ensure smaller size */
        str = lc_realloc(s->rt, str, sizeof(LCString) +
                            (s->len << s->is_wide_char) + 1 - s->is_wide_char);
        if (str == NULL)
            str = s->str;
        s->str = str;
    }
    if (!s->is_wide_char) {
        str->u.str8[s->len] = 0;
    }
    str->is_wide_char = s->is_wide_char;
    str->length = s->len;
    s->str = NULL;
    return (LCValue){ { .ptr_val = (LCObject*)str }, LC_TY_STRING };
}

LCValue LCNewStringFromCStringLen(LCRuntime* rt, const unsigned char* buf, uint32_t buf_len) {
    const unsigned char* buf_end = buf + buf_len;
    const unsigned char* p = buf;
    const unsigned char* p_next;
    uint32_t c;
    size_t len;
    StringBuffer sb;

    while (p < buf_end && *p < 128) {
        p++;
    }
    len = p - buf;

    if (p == buf_end) {  /* ANSCII string */
        return lc_new_string8(rt, buf, buf_len);
    }
    if (string_buffer_init(rt, &sb, buf_len)) {
        abort();
    }
    string_buffer_write8(&sb, buf, len);
    while (p < buf_end) {
        if (*p < 128) {
            string_buffer_putc8(&sb, *p++);
        } else {
            /* parse utf-8 sequence, return 0xFFFFFFFF for error */
            c = unicode_from_utf8(p, buf_end - p, &p_next);
            if (c < 0x10000) {
                p = p_next;
            } else if (c <= 0x10FFFF) {
                p = p_next;
                /* surrogate pair */
                c -= 0x10000;
                string_buffer_putc16(&sb, (c >> 10) + 0xd800);
                c = (c & 0x3ff) + 0xdc00;
            } else {
                /* invalid char */
                c = 0xfffd;
                /* skip the invalid chars */
                /* XXX: seems incorrect. Why not just use c = *p++; ? */
                while (p < buf_end && (*p >= 0x80 && *p < 0xc0))
                    p++;
                if (p < buf_end) {
                    p++;
                    while (p < buf_end && (*p >= 0x80 && *p < 0xc0))
                        p++;
                }
            }
            string_buffer_putc16(&sb, c);
        }
    }

    return string_buffer_end(&sb);
}

LCValue LCNewStringFromCString(LCRuntime* rt, const unsigned char* content) {
    return LCNewStringFromCStringLen(rt, content, strlen((const char*)content));
}

LCValue LCNewRefCell(LCRuntime* rt, LCValue value) {
    LCRefCell* cell = (LCRefCell*)lc_mallocz(rt, sizeof(LCRefCell));
    cell->header.count = 1;
    LCRetain(value);
    cell->value = value;
    return (LCValue){ { .ptr_val = (LCObject*)cell }, LC_TY_REFCELL };
}

void LCRefCellSetValue(LCRuntime* rt, LCValue cell, LCValue value) {
    LCRefCell* ref =(LCRefCell*)cell.ptr_val;
    LCRelease(rt, ref->value);
    LCRetain(value);
    ref->value = value;
}

LCValue LCRefCellGetValue(LCValue cell) {
    LCRefCell* ref =(LCRefCell*)cell.ptr_val;
    return ref->value;
}

LCValue LCNewUnionObject(LCRuntime* rt, int tag, int size, LCValue* args) {
    size_t malloc_size = sizeof(LCUnionObject) + size * sizeof(LCValue);
    LCUnionObject* union_obj = (LCUnionObject*)lc_mallocz(rt, malloc_size);

    LCInitObject(&union_obj->header, LC_TY_STRING);

    union_obj->tag = tag;
    union_obj->size = size;

    int i;
    for (i = 0; i < size; i++) {
        LCRetain(args[i]);
        union_obj->value[i] = args[i];
    }
    
    return (LCValue){ { .ptr_val = (LCObject*)union_obj }, LC_TY_UNION_OBJECT };
}

int LCUnionGetType(LCValue val) {
    if (val.tag == LC_TY_UNION) {
        return val.int_val;
    }

    LCUnionObject* union_obj = (LCUnionObject*)val.ptr_val;
    return union_obj->tag;
}

LCValue LCNewLambda(LCRuntime* rt, LCCFunction c_fun, int argc, LCValue* args) {
    size_t size = sizeof(LCLambda) + argc * sizeof(LCValue);
    LCLambda* lambda = (LCLambda*)lc_mallocz(rt, size);
    lambda->header.count = 1;
    lambda->c_fun = c_fun;
    lambda->captured_values_size = argc;

    size_t i;
    for (i = 0; i < argc; i++) {
        LCRetain(args[i]);
        lambda->captured_values[i] = args[i];
    }

    return (LCValue){ { .ptr_val = (LCObject*)lambda }, LC_TY_LAMBDA };
}

LCValue LCLambdaGetValue(LCRuntime* rt, LCValue lambda_val, int index) {
    LCLambda* lambda = (LCLambda*)lambda_val.ptr_val;
    LCValue ret = lambda->captured_values[index];
    return ret;
}

LCValue* LCLambdaGetValuePointer(LCRuntime* rt, LCValue lambda_val, int index) {
    LCLambda* lambda = (LCLambda*)lambda_val.ptr_val;
    return &lambda->captured_values[index];
}

LCValue LCLambdaGetRefValue(LCRuntime* rt, LCValue lambda_val, int index) {
    LCLambda* lambda = (LCLambda*)lambda_val.ptr_val;
    LCValue ret = lambda->captured_values[index];
    if (ret.tag != LC_TY_REFCELL) {  // TODO: do NOT check in release
        fprintf(stderr, "[LichenScript] value is not a ref\n");
        abort();
    }
    return LCRefCellGetValue(ret);
}

void LCLambdaSetValue(LCRuntime* rt, LCValue lambda_val, int index, LCValue value) {
    LCLambda* lambda = (LCLambda*)lambda_val.ptr_val;
    LCRelease(rt, lambda->captured_values[index]);
    LCRetain(value);
    lambda->captured_values[index] = value;
}

void LCLambdaSetRefValue(LCRuntime* rt, LCValue lambda_val, int index, LCValue value) {
    LCLambda* lambda = (LCLambda*)lambda_val.ptr_val;
    LCValue ref = lambda->captured_values[index];
    if (ref.tag != LC_TY_REFCELL) {  // TODO: do NOT check in release
        fprintf(stderr, "[LichenScript] value is not a ref\n");
        abort();
    }
    LCRefCellSetValue(rt, ref, value);
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
        if (bucket_at_index->content.tag == LC_TY_NULL) {
            break;
        }

        str_ptr = (LCString*)bucket_at_index->content.ptr_val;
        if (strcmp((const char *)str_ptr->u.str8, content) == 0) {
            result = bucket_at_index->content;
            break;
        }

        if (bucket_at_index->next == NULL) {
            break;
        }
        bucket_at_index = bucket_at_index->next;
    }

    // symbol not found
    if (result.tag == LC_TY_NULL) {
        result = LCNewStringFromCStringLen(rt, (const unsigned char*)content, len);
        result.tag = LC_TY_SYMBOL;
        str_ptr = (LCString*)result.ptr_val;
        str_ptr->header.count = LC_NO_GC;
        str_ptr->hash = hash_string8((const unsigned char*)content, len, rt->seed);

        if (bucket_at_index->content.tag == LC_TY_NULL) {
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

LCArray* LCNewArrayWithCap(LCRuntime* rt, size_t cap) {
    LCArray* result = (LCArray*)lc_malloc(rt, sizeof(LCArray));
    result->header.count = 1;
    result->header.class_id = 0;
    result->capacity = cap;
    result->len = 0;
    result->data = (LCValue*)lc_mallocz(rt, sizeof(LCValue) * cap);
    return result;
}

LCValue LCNewArray(LCRuntime* rt) {
    LCArray* arr = LCNewArrayWithCap(rt, 8);
    return (LCValue) { { .ptr_val = (LCObject*)arr },  LC_TY_ARRAY };
}

LCValue LCNewArrayLen(LCRuntime* rt, size_t size) {
    size_t cap = size;
    if (cap > 2 &&  cap % 2 != 0) {
        cap += 1;
    }
    LCArray* arr = LCNewArrayWithCap(rt, cap);
    arr->len = size;
    return (LCValue) { { .ptr_val = (LCObject*)arr },  LC_TY_ARRAY };
}

LCValue LCArrayGetValue(LCRuntime* rt, LCValue this, int index) {
    LCArray* arr = (LCArray*)this.ptr_val;
    if (unlikely(index >= arr->len)) {
        fprintf(stderr, "[LichenScript] index %d out of range, size: %d\n", index, arr->len);
        abort();
    }
    return arr->data[index];
}

void LCArraySetValue(LCRuntime* rt, LCValue this, int index, LCValue value) {
    LCArray* arr = (LCArray*)this.ptr_val;
    if (unlikely(index >= arr->len)) {
        fprintf(stderr, "[LichenScript] index %d out of range, size: %d\n", index, arr->len);
        abort();
    }
    LCRelease(rt, arr->data[index]);
    LCRetain(value);
    arr->data[index] = value;
}

LCValue LCNewSymbol(LCRuntime* rt, const char* content) {
    return LCNewSymbolLen(rt, content, strlen(content));
}

LCValue LCRunMain(LCProgram* program) {
    if (program->main_fun == NULL) {
        return MK_NULL();
    }
    return program->main_fun(program->runtime, MK_NULL(), 0, NULL);
}

static void std_print_string(LCString* str) {
    printf("%s", str->u.str8);
}

static void std_print_val(LCRuntime* rt, LCValue val) {
    switch (val.tag)
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

LCClassID LCDefineClass(LCRuntime* rt, LCClassDef* cls_def) {
    LCClassID id = rt->cls_meta_size++;

    if (rt->cls_meta_size >= rt->cls_meta_cap) {
        rt->cls_meta_cap *= 2;
        rt->cls_meta_data = lc_realloc(rt, rt->cls_meta_data, sizeof(LCClassMeta) * rt->cls_meta_cap);
    }

    rt->cls_meta_data[id].cls_def = cls_def;

    return id;
}

void LCDefineClassMethod(LCRuntime* rt, LCClassID cls_id, LCClassMethodDef* cls_method, size_t size) {
    LCClassMeta* meta = rt->cls_meta_data + cls_id;
    meta->cls_method = cls_method;
    meta->cls_method_size = size;
}

LCValue LCInvokeStr(LCRuntime* rt, LCValue this, const char* content, int arg_len, LCValue* args) {
    if (this.tag <= 0) {
        fprintf(stderr, "[LichenScript] try to invoke on primitive type\n");
        abort();
    }

    LCObject* obj = (LCObject*)this.ptr_val;
    LCClassID class_id = obj->header.class_id;
    LCClassMeta* meta = rt->cls_meta_data + class_id;

    size_t i;
    LCClassMethodDef* method_def;
    for (i = 0; i < meta->cls_method_size; i++) {
        method_def = &meta->cls_method[i];
        if (strcmp(method_def->name, content) == 0) {  // this is it
            return method_def->fun_ptr(rt, this, arg_len, args);
        }
    }

    fprintf(stderr, "[LichenScript] Can not find method \"%s\" of class, id: %u\n", content, class_id);
    abort();
    return MK_NULL();
}

LCValue LCEvalLambda(LCRuntime* rt, LCValue this, int argc, LCValue* args) {
    LCLambda* lambda = (LCLambda*)this.ptr_val;
    return lambda->c_fun(rt, this, argc, args);
}

void lc_init_object(LCRuntime* rt, LCClassID cls_id, LCObject* obj) {
    obj->header.count = 1;
    obj->header.class_id = cls_id;
}

LCValue lc_std_print(LCRuntime* rt, LCValue this, int arg_len, LCValue* args) {
    for (int i = 0; i < arg_len; i++) {
        std_print_val(rt, args[i]);
    }
    printf("\n");
    return MK_NULL();
}

LCValue lc_std_array_get_length(LCRuntime* rt, LCValue this, int arg_len, LCValue* args) {
    LCArray* arr = (LCArray*)this.ptr_val;
    return MK_I32(arr->len);
}

LCValue lc_std_array_push(LCRuntime* rt, LCValue this, int arg_len, LCValue* args) {
    LCArray* arr = (LCArray*)this.ptr_val;

    if (arr->len == arr->capacity) {
        arr->capacity *= 2;
        arr->data = lc_realloc(rt, arr->data, arr->capacity * sizeof(LCValue));
    }

    LCRetain(args[0]);
    arr->data[arr->len++] = args[0];

    return MK_NULL();
}

static void copy_str16(uint16_t *dst, const LCString *p, int offset, int len)
{
    if (p->is_wide_char) {
        memcpy(dst, p->u.str16 + offset, len * 2);
    } else {
        const uint8_t *src1 = p->u.str8 + offset;
        int i;

        for(i = 0; i < len; i++)
            dst[i] = src1[i];
    }
}

LCValue lc_std_string_concat(LCRuntime* rt, LCValue this, int arg_len, LCValue* args) {
    LCString *p;
    int is_wide_char;

    LCString* s1 = (LCString*)(args[0].ptr_val);
    LCString* s2 = (LCString*)(args[1].ptr_val);
    is_wide_char = s1->is_wide_char | s2->is_wide_char;

    uint32_t len = s1->length + s2->length;

    p = lc_alloc_string_rt(rt, len, is_wide_char);
    if (!is_wide_char) {
        memcpy(p->u.str8, s1->u.str8, s1->length);
        memcpy(p->u.str8 + s1->length, s2->u.str8, s2->length);
        p->u.str8[len] = '\0';
    } else {
        copy_str16(p->u.str16, s1, 0, s1->length);
        copy_str16(p->u.str16 + s1->length, s2, 0, s2->length);
    }

    return (LCValue){ { .ptr_val = (LCObject*)p }, LC_TY_STRING };
}

LCValue lc_std_string_get_length(LCRuntime* rt, LCValue this, int arg_len, LCValue* args) {
    LCString* str = (LCString*)this.ptr_val;
    return MK_I32(str->length);
}
