/**
 * Copyright 2022 Vincent Chan
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#include "runtime.h"
#include "string.h"
#include "time.h"
#include "stdio.h"
#include "math.h"

#if defined(__APPLE__)
#include <malloc/malloc.h>
#include <execinfo.h>
#endif

#define LC_INIT_SYMBOL_BUCKET_SIZE 128
#define LC_INIT_CLASS_META_CAP 8
#define LC_SMALL_MAP_THRESHOLD 8
#define I64_POOL_SIZE 1024

#define lc_raw_malloc malloc
#define lc_raw_realloc realloc
#define lc_raw_free free

#define MK_STRING(v) (LCValue){ { .ptr_val = (LCObject*)v }, LC_TY_STRING }

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

static inline uint32_t hash_int(int i, uint32_t seed) {
    return seed * 263 + i;
}

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
        ptr->u.i64 = val;
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

static force_inline void lc_panic_internal() {
#if defined(__APPLE__)
    fprintf(stderr, "[LichenScript] Panic stack:\n");
    void* callstack[128];
    int i, frames = backtrace(callstack, 128);
    char** strs = backtrace_symbols(callstack, frames);
    for (i = 0; i < frames; ++i) {
        fprintf(stderr, "%s\n", strs[i]);
    }
    free(strs);
#endif

    exit(2);
}

static void LCFreeObject(LCRuntime* rt, LCValue val);

static inline void LCFreeLambda(LCRuntime* rt, LCValue val) {
    LCLambda* lambda = (LCLambda*)val.ptr_val; 
    size_t i;

    LCRelease(rt, lambda->captured_this);

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

static inline void LCFreeTuple(LCRuntime* rt, LCValue val) {
    size_t i;
    LCTuple* tuple = (LCTuple*)val.ptr_val;

    for (i = 0; i < tuple->len; i++) {
        LCRelease(rt, tuple->data[i]);
    }

    lc_free(rt, tuple);
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

void lc_std_map_free(LCRuntime* rt, LCValue val);

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

    case LC_TY_TUPLE:
        LCFreeTuple(rt, val);
        break;

    case LC_TY_ARRAY:
        LCFreeArray(rt, val);
        break;

    case LC_TY_MAP:
        lc_std_map_free(rt, val);
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
        lc_panic_internal();

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

static char* LCStringToUTF8(LCRuntime* rt, LCString* str) {
    char* space = lc_malloc(rt, str->length * 2 + 1);
    int idx = 0;
    int len;
    uint8_t* buf = (uint8_t*)space;

    while (idx < str->length) {
        len = unicode_to_utf8(buf, str->u.str16[idx]);
        buf += len;
        idx++;
    }

    *buf = 0;

    return space;
}

int LCStringEqUtf8(LCRuntime* rt, LCValue this, const char* cmp_str, size_t len) {
    int result;
    LCString* str = (LCString*)this.ptr_val;

    if (!str->is_wide_char) {
        if (str->length != len) {
            return 0;
        }

        return memcmp(str->u.str8, cmp_str, len) == 0;
    }

    char* utf8_str = LCStringToUTF8(rt, str);

    result = strcmp(utf8_str, cmp_str);

    lc_free(rt, utf8_str);
    return result;
}

static LCClassMethodDef Object_method_def[] = {
    { "toString", 0, LC_Object_toString }
};

LCRuntime* LCNewRuntime() {
    LCRuntime* runtime = (LCRuntime*)lc_raw_malloc(sizeof(LCRuntime));
    memset(runtime, 0, sizeof(LCRuntime));
    runtime->malloc_state.malloc_count = 1;

    runtime->seed = time(NULL);

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

    free_i64_pool(rt);

    lc_free(rt, rt->cls_meta_data);

#ifdef LSC_DEBUG
    if (rt->malloc_state.malloc_count != 1) {
        fprintf(stderr, "[LichenScript] memory leaks %zu\n", rt->malloc_state.malloc_count);
        lc_raw_free(rt);
        exit(1);
    }
#endif

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
                lc_panic_internal();
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
        lc_panic_internal();
    }
    new_size = min_int(max_int(new_len, s->size * 3 / 2), LC_STRING_LEN_MAX);
    if (!s->is_wide_char && c >= 0x100) {
        return string_buffer_widen(s, new_size);
    }
    new_size_bytes = sizeof(LCString) + (new_size << s->is_wide_char) + 1 - s->is_wide_char;
    new_str = lc_realloc2(s->rt, s->str, new_size_bytes, &slack);
    if (!new_str) {
        fprintf(stderr, "malloc memory for string failed");
        lc_panic_internal();
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

    LCInitObject(&result->header, LC_TY_STRING);

    if (buf_len > 0) {
        memcpy(result->u.str8, buf, buf_len);
    }
    result->is_wide_char = 0;
    result->length = buf_len;
    result->hash = 0;
    
    return MK_STRING(result);
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
    str->hash = 0;
    s->str = NULL;
    return MK_STRING(str);
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
        lc_panic_internal();
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

LCValue LCUnionObjectGet(LCRuntime* rt, LCValue this, int index) {
    LCUnionObject* obj = (LCUnionObject*)this.ptr_val;
    LCValue result = obj->value[index];
    LCRetain(result);
    return result;
}

int LCUnionGetType(LCValue val) {
    if (val.tag == LC_TY_UNION) {
        return val.int_val;
    }

    LCUnionObject* union_obj = (LCUnionObject*)val.ptr_val;
    return union_obj->tag;
}

LCValue LCNewLambda(LCRuntime* rt, LCCFunction c_fun, LCValue this, int argc, LCValue* args) {
    size_t size = sizeof(LCLambda) + argc * sizeof(LCValue);
    LCLambda* lambda = (LCLambda*)lc_mallocz(rt, size);
    lambda->header.count = 1;
    lambda->c_fun = c_fun;
    lambda->captured_values_size = argc;

    LCRetain(this);
    lambda->captured_this = this;

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
        lc_panic_internal();
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
        lc_panic_internal();
    }
    LCRefCellSetValue(rt, ref, value);
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
    if (size == 0) {
        cap = 2;
    } else if (cap > 2 &&  cap % 2 != 0) {
        cap += 1;
    }
    LCArray* arr = LCNewArrayWithCap(rt, cap);
    arr->len = size;
    return (LCValue) { { .ptr_val = (LCObject*)arr },  LC_TY_ARRAY };
}

LCValue LCArrayGetValue(LCRuntime* rt, LCValue this, int index) {
    LCValue item;
    LCArray* arr = (LCArray*)this.ptr_val;
    if (unlikely(index < 0 || index >= arr->len)) {
        fprintf(stderr, "[LichenScript] Panic: index %d out of range, size: %d\n", index, arr->len);
        lc_panic_internal();
    }
    item = arr->data[index];
    LCRetain(item);
    return item;
}

void LCArraySetValue(LCRuntime* rt, LCValue this, int argc, LCValue* args) {
    int index = args[0].int_val;
    LCArray* arr = (LCArray*)this.ptr_val;
    if (unlikely(index >= arr->len)) {
        fprintf(stderr, "[LichenScript] index %d out of range, size: %d\n", index, arr->len);
        lc_panic_internal();
    }
    LCRelease(rt, arr->data[index]);
    LCRetain(args[1]);
    arr->data[index] = args[1];
}

LCValue LCNewTuple(LCRuntime* rt, LCValue this, int32_t arg_len, LCValue* args) {
    int32_t i;
    size_t size = sizeof(LCTuple) + sizeof(LCValue) * arg_len;

    LCTuple* tuple = (LCTuple*)lc_malloc(rt, size);

    tuple->header.count = 1;
    tuple->header.class_id = 0;
    tuple->len = arg_len;

    for (i = 0; i < arg_len; i++) {
        LCRetain(args[i]);
        tuple->data[i] = args[i];
    }

    return (LCValue) { { .ptr_val = (LCObject*)tuple }, LC_TY_TUPLE };
}

LCValue LCNewI64(LCRuntime* rt, int64_t val) {
    LCBox64* ptr = (LCBox64*)lc_malloc(rt, sizeof(LCBox64));

    ptr->header.class_id = 0;
    ptr->header.count = 1;
    ptr->u.i64 = val;

    return (LCValue) { { .ptr_val = (LCObject*)ptr }, LC_TY_BOXED_I64 };
}

LCValue LCI64Binary(LCRuntime* rt, LCArithmeticType op, LCValue left, LCValue right) {
    LCBox64* left_ptr = (LCBox64*)left.ptr_val;
    LCBox64* right_ptr = (LCBox64*)right.ptr_val;
    int64_t result = 0;

    switch (op) {
        case LC_ARTH_PLUS:
            result = left_ptr->u.i64 + right_ptr->u.i64;
            break;

        case LC_ARTH_MINUS:
            result = left_ptr->u.i64 - right_ptr->u.i64;
            break;

        case LC_ARTH_MULT:
            result = left_ptr->u.i64 * right_ptr->u.i64;
            break;

        case LC_ARTH_DIV:
            result = left_ptr->u.i64 / right_ptr->u.i64;
            break;

        case LC_ARTH_MOD:
            result = left_ptr->u.i64 % right_ptr->u.i64;
            break;

        case LC_ARTH_LSHIFT:
            result = left_ptr->u.i64 << right_ptr->u.i64;
            break;

        case LC_ARTH_RSHIFT:
            result = left_ptr->u.i64 >> right_ptr->u.i64;
            break;

        case LC_ARTH_BIT_OR:
            result = left_ptr->u.i64 | right_ptr->u.i64;
            break;

        case LC_ARTH_BIT_XOR:
            result = left_ptr->u.i64 ^ right_ptr->u.i64;
            break;

        case LC_ARTH_BIT_AND:
            result = left_ptr->u.i64 & right_ptr->u.i64;
            break;

    }

    return LCNewI64(rt, result);
}

LCValue LCNewF64(LCRuntime* rt, double val) {
    LCBox64* ptr = (LCBox64*)lc_malloc(rt, sizeof(LCBox64));

    ptr->header.class_id = 0;
    ptr->header.count = 1;
    ptr->u.f64 = val;

    return (LCValue) { { .ptr_val = (LCObject*)ptr }, LC_TY_BOXED_I64 };
}

LCValue LCF64Binary(LCRuntime* rt, LCArithmeticType op, LCValue left, LCValue right) {
    LCBox64* left_ptr = (LCBox64*)left.ptr_val;
    LCBox64* right_ptr = (LCBox64*)right.ptr_val;
    double result = 0;

    switch (op) {
        case LC_ARTH_PLUS:
            result = left_ptr->u.f64 + right_ptr->u.f64;
            break;

        case LC_ARTH_MINUS:
            result = left_ptr->u.f64 - right_ptr->u.f64;
            break;

        case LC_ARTH_MULT:
            result = left_ptr->u.f64 * right_ptr->u.f64;
            break;

        case LC_ARTH_DIV:
            result = left_ptr->u.f64 / right_ptr->u.f64;
            break;

        default:
            break;

    }

    return LCNewF64(rt, result);
}

LCValue LCRunMain(LCProgram* program) {
    if (program->main_fun == NULL) {
        return MK_NULL();
    }
    return program->main_fun(program->runtime, MK_NULL(), 0, NULL);
}

static void std_print_string(LCRuntime* rt, LCString* str) {
    if (!str->is_wide_char) {
        printf("%s", str->u.str8);
        return;
    }

    char* space = LCStringToUTF8(rt, str);
    printf("%s", space);
    lc_free(rt, space);
}

void std_print_array(LCRuntime* rt, LCValue val);
void std_print_tuple(LCRuntime* rt, LCValue val);

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
        printf("unit");
        break;

    case LC_TY_CHAR:
        printf("%c", val.int_val);
        break;

    case LC_TY_STRING:
        std_print_string(rt, (LCString*)val.ptr_val);
        break;

    case LC_TY_BOXED_I64:
        printf("%lld", ((LCBox64*)val.ptr_val)->u.i64);
        break;

    case LC_TY_BOXED_F64:
        printf("%lf", ((LCBox64*)val.ptr_val)->u.f64);
        break;

    case LC_TY_TUPLE:
        std_print_tuple(rt, val);
        break;

    case LC_TY_ARRAY:
        std_print_array(rt, val);
        break;

    case LC_TY_MAP:
        printf("Map");
        break;
    
    default:
        break;
    }

}

void std_print_tuple(LCRuntime* rt, LCValue val) {
    size_t i;
    LCTuple* tuple = (LCTuple*)val.ptr_val;
    printf("(");
    for (i = 0; i < tuple->len; i++) {
        std_print_val(rt, tuple->data[i]);
        if (i < tuple->len - 1) {
            printf (", ");
        }
    }
    printf(")");
}

void std_print_array(LCRuntime* rt, LCValue val) {
    LCArray* arr = (LCArray*)val.ptr_val;
    uint32_t i;

    printf("[");

    for (i = 0; i < arr->len; i++) {
        std_print_val(rt, arr->data[i]);
        if (i < arr->len - 1) {
            printf(", ");
        }
    }

    printf("]");
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
        lc_panic_internal();
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
    lc_panic_internal();
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

LCValue lc_std_array_resize(LCRuntime* rt, LCValue this, int arg_len, LCValue* args) {
    LCArray* arr = (LCArray*)this.ptr_val;
    int new_len = args[0].int_val;
    int i;
    size_t new_cap;

    if (new_len == arr->len) {
        return MK_NULL();
    }

    if (new_len == 0) {
        for (i = 0; i < arr->len; i++) {
            LCRelease(rt, arr->data[i]);
            arr->data[i] = MK_NULL();
        }
        arr->len = 0;

        new_cap = 2;
        arr->data = lc_realloc(rt, arr->data, sizeof(LCValue) * new_cap);
        arr->capacity = new_cap;

        return MK_NULL();
    }

    if (new_len < arr->len) {
        for (i = new_len; i < arr->len; i++) {
            LCRelease(rt, arr->data[i]);
            arr->data[i] = MK_NULL();
        }
        arr->len = new_len;
        return MK_NULL();
    }

    // assert(new_len > arr->len)
    new_cap = arr->capacity;
    while (new_cap < new_len) {
        new_cap *= 2;
    }

    if (new_cap != arr->capacity) {
        arr->data = lc_realloc(rt, arr->data, new_cap * sizeof(LCValue));
        arr->capacity = new_cap;
    }

    for (i = arr->len; i < new_len; i++) {
        LCRetain(args[1]);
        arr->data[i] = args[1];
    }

    arr->len = new_len;
    return MK_NULL();
}

typedef struct lc_sort_ctx {
    LCRuntime* rt;
    LCValue lambda;
} lc_sort_ctx;

typedef void (*exchange_f)(void *a, void *b, size_t size);
typedef int (*cmp_f)(const void *, const void *, void *opaque);

static void exchange_bytes(void *a, void *b, size_t size) {
    uint8_t *ap = (uint8_t *)a;
    uint8_t *bp = (uint8_t *)b;

    while (size-- != 0) {
        uint8_t t = *ap;
        *ap++ = *bp;
        *bp++ = t;
    }
}

static void exchange_one_byte(void *a, void *b, size_t size) {
    uint8_t *ap = (uint8_t *)a;
    uint8_t *bp = (uint8_t *)b;
    uint8_t t = *ap;
    *ap = *bp;
    *bp = t;
}

static void exchange_int16s(void *a, void *b, size_t size) {
    uint16_t *ap = (uint16_t *)a;
    uint16_t *bp = (uint16_t *)b;

    for (size /= sizeof(uint16_t); size-- != 0;) {
        uint16_t t = *ap;
        *ap++ = *bp;
        *bp++ = t;
    }
}

static void exchange_one_int16(void *a, void *b, size_t size) {
    uint16_t *ap = (uint16_t *)a;
    uint16_t *bp = (uint16_t *)b;
    uint16_t t = *ap;
    *ap = *bp;
    *bp = t;
}

static void exchange_int32s(void *a, void *b, size_t size) {
    uint32_t *ap = (uint32_t *)a;
    uint32_t *bp = (uint32_t *)b;

    for (size /= sizeof(uint32_t); size-- != 0;) {
        uint32_t t = *ap;
        *ap++ = *bp;
        *bp++ = t;
    }
}

static void exchange_one_int32(void *a, void *b, size_t size) {
    uint32_t *ap = (uint32_t *)a;
    uint32_t *bp = (uint32_t *)b;
    uint32_t t = *ap;
    *ap = *bp;
    *bp = t;
}

static void exchange_int64s(void *a, void *b, size_t size) {
    uint64_t *ap = (uint64_t *)a;
    uint64_t *bp = (uint64_t *)b;

    for (size /= sizeof(uint64_t); size-- != 0;) {
        uint64_t t = *ap;
        *ap++ = *bp;
        *bp++ = t;
    }
}

static void exchange_one_int64(void *a, void *b, size_t size) {
    uint64_t *ap = (uint64_t *)a;
    uint64_t *bp = (uint64_t *)b;
    uint64_t t = *ap;
    *ap = *bp;
    *bp = t;
}

static void exchange_int128s(void *a, void *b, size_t size) {
    uint64_t *ap = (uint64_t *)a;
    uint64_t *bp = (uint64_t *)b;

    for (size /= sizeof(uint64_t) * 2; size-- != 0; ap += 2, bp += 2) {
        uint64_t t = ap[0];
        uint64_t u = ap[1];
        ap[0] = bp[0];
        ap[1] = bp[1];
        bp[0] = t;
        bp[1] = u;
    }
}

static void exchange_one_int128(void *a, void *b, size_t size) {
    uint64_t *ap = (uint64_t *)a;
    uint64_t *bp = (uint64_t *)b;
    uint64_t t = ap[0];
    uint64_t u = ap[1];
    ap[0] = bp[0];
    ap[1] = bp[1];
    bp[0] = t;
    bp[1] = u;
}

static inline exchange_f exchange_func(const void *base, size_t size) {
    switch (((uintptr_t)base | (uintptr_t)size) & 15) {
    case 0:
        if (size == sizeof(uint64_t) * 2)
            return exchange_one_int128;
        else
            return exchange_int128s;
    case 8:
        if (size == sizeof(uint64_t))
            return exchange_one_int64;
        else
            return exchange_int64s;
    case 4:
    case 12:
        if (size == sizeof(uint32_t))
            return exchange_one_int32;
        else
            return exchange_int32s;
    case 2:
    case 6:
    case 10:
    case 14:
        if (size == sizeof(uint16_t))
            return exchange_one_int16;
        else
            return exchange_int16s;
    default:
        if (size == 1)
            return exchange_one_byte;
        else
            return exchange_bytes;
    }
}

static void heapsortx(void *base, size_t nmemb, size_t size, cmp_f cmp, void *opaque)
{
    uint8_t *basep = (uint8_t *)base;
    size_t i, n, c, r;
    exchange_f swap = exchange_func(base, size);

    if (nmemb > 1) {
        i = (nmemb / 2) * size;
        n = nmemb * size;

        while (i > 0) {
            i -= size;
            for (r = i; (c = r * 2 + size) < n; r = c) {
                if (c < n - size && cmp(basep + c, basep + c + size, opaque) <= 0)
                    c += size;
                if (cmp(basep + r, basep + c, opaque) > 0)
                    break;
                swap(basep + r, basep + c, size);
            }
        }
        for (i = n - size; i > 0; i -= size) {
            swap(basep, basep + i, size);

            for (r = 0; (c = r * 2 + size) < i; r = c) {
                if (c < i - size && cmp(basep + c, basep + c + size, opaque) <= 0)
                    c += size;
                if (cmp(basep + r, basep + c, opaque) > 0)
                    break;
                swap(basep + r, basep + c, size);
            }
        }
    }
}

static inline void *med3(void *a, void *b, void *c, cmp_f cmp, void *opaque)
{
    return cmp(a, b, opaque) < 0 ?
        (cmp(b, c, opaque) < 0 ? b : (cmp(a, c, opaque) < 0 ? c : a )) :
        (cmp(b, c, opaque) > 0 ? b : (cmp(a, c, opaque) < 0 ? a : c ));
}

/* pointer based version with local stack and insertion sort threshhold */
void rqsort(void *base, size_t nmemb, size_t size, cmp_f cmp, void *opaque)
{
    struct { uint8_t *base; size_t count; int depth; } stack[50], *sp = stack;
    uint8_t *ptr, *pi, *pj, *plt, *pgt, *top, *m;
    size_t m4, i, lt, gt, span, span2;
    int c, depth;
    exchange_f swap = exchange_func(base, size);
    exchange_f swap_block = exchange_func(base, size | 128);

    if (nmemb < 2 || size <= 0)
        return;

    sp->base = (uint8_t *)base;
    sp->count = nmemb;
    sp->depth = 0;
    sp++;

    while (sp > stack) {
        sp--;
        ptr = sp->base;
        nmemb = sp->count;
        depth = sp->depth;

        while (nmemb > 6) {
            if (++depth > 50) {
                /* depth check to ensure worst case logarithmic time */
                heapsortx(ptr, nmemb, size, cmp, opaque);
                nmemb = 0;
                break;
            }
            /* select median of 3 from 1/4, 1/2, 3/4 positions */
            /* should use median of 5 or 9? */
            m4 = (nmemb >> 2) * size;
            m = med3(ptr + m4, ptr + 2 * m4, ptr + 3 * m4, cmp, opaque);
            swap(ptr, m, size);  /* move the pivot to the start or the array */
            i = lt = 1;
            pi = plt = ptr + size;
            gt = nmemb;
            pj = pgt = top = ptr + nmemb * size;
            for (;;) {
                while (pi < pj && (c = cmp(ptr, pi, opaque)) >= 0) {
                    if (c == 0) {
                        swap(plt, pi, size);
                        lt++;
                        plt += size;
                    }
                    i++;
                    pi += size;
                }
                while (pi < (pj -= size) && (c = cmp(ptr, pj, opaque)) <= 0) {
                    if (c == 0) {
                        gt--;
                        pgt -= size;
                        swap(pgt, pj, size);
                    }
                }
                if (pi >= pj)
                    break;
                swap(pi, pj, size);
                i++;
                pi += size;
            }
            /* array has 4 parts:
             * from 0 to lt excluded: elements identical to pivot
             * from lt to pi excluded: elements smaller than pivot
             * from pi to gt excluded: elements greater than pivot
             * from gt to n excluded: elements identical to pivot
             */
            /* move elements identical to pivot in the middle of the array: */
            /* swap values in ranges [0..lt[ and [i-lt..i[
               swapping the smallest span between lt and i-lt is sufficient
             */
            span = plt - ptr;
            span2 = pi - plt;
            lt = i - lt;
            if (span > span2)
                span = span2;
            swap_block(ptr, pi - span, span);
            /* swap values in ranges [gt..top[ and [i..top-(top-gt)[
               swapping the smallest span between top-gt and gt-i is sufficient
             */
            span = top - pgt;
            span2 = pgt - pi;
            pgt = top - span2;
            gt = nmemb - (gt - i);
            if (span > span2)
                span = span2;
            swap_block(pi, top - span, span);

            /* now array has 3 parts:
             * from 0 to lt excluded: elements smaller than pivot
             * from lt to gt excluded: elements identical to pivot
             * from gt to n excluded: elements greater than pivot
             */
            /* stack the larger segment and keep processing the smaller one
               to minimize stack use for pathological distributions */
            if (lt > nmemb - gt) {
                sp->base = ptr;
                sp->count = lt;
                sp->depth = depth;
                sp++;
                ptr = pgt;
                nmemb -= gt;
            } else {
                sp->base = pgt;
                sp->count = nmemb - gt;
                sp->depth = depth;
                sp++;
                nmemb = lt;
            }
        }
        /* Use insertion sort for small fragments */
        for (pi = ptr + size, top = ptr + nmemb * size; pi < top; pi += size) {
            for (pj = pi; pj > ptr && cmp(pj - size, pj, opaque) > 0; pj -= size)
                swap(pj, pj - size, size);
        }
    }
}

static int lc_cmp_generic(const void* a, const void* b, void* ptr) {
    const lc_sort_ctx* ctx = (const lc_sort_ctx*)ptr;
    LCValue ret;
    LCValue* val_a = (LCValue*)a;
    LCValue* val_b = (LCValue*)b;

    ret = LCEvalLambda(ctx->rt, ctx->lambda, 0, (LCValue[]) { *val_a, *val_b });

    return ret.int_val;
}

LCValue lc_std_array_sort(LCRuntime* rt, LCValue this, int arg_len, LCValue* args) {
    lc_sort_ctx ctx = { rt, args[0] };
    LCArray* arr = (LCArray*)this.ptr_val;

    rqsort(arr->data, arr->len, sizeof(LCValue), lc_cmp_generic, &ctx);

    return MK_NULL();
}

LCValue lc_std_array_slice(LCRuntime* rt, LCValue this, int arg_len, LCValue* args) {
    int upper, lower, len, i;
    LCValue item;
    LCArray* new_arr;
    LCArray* arr = (LCArray*)this.ptr_val;
    lower = args[0].int_val;
    upper = args[1].int_val;

    lower = max_int(0, lower);
    upper = min_int(arr->len, upper);

    if (unlikely(lower >= upper)) {
        return LCNewArrayLen(rt, 0);
    }

    len = upper - lower;
    new_arr = LCNewArrayWithCap(rt, len);

    for (i = lower; i < upper; i++) {
        item = arr->data[i];
        LCRetain(item);
        new_arr->data[i - lower] = item;
    }

    new_arr->len = len;

    return (LCValue) { { .ptr_val = (LCObject*)new_arr },  LC_TY_ARRAY };
}

LCValue lc_std_array_map(LCRuntime* rt, LCValue this, int arg_len, LCValue* args) {
    uint32_t i;
    LCArray* arr = (LCArray*)this.ptr_val;
    LCArray* new_arr = LCNewArrayWithCap(rt, arr->capacity);

    for (i = 0; i < arr->len; i++) {
        new_arr->data[i] = LCEvalLambda(rt, args[0], 1, (LCValue[]) { arr->data[i] });
    }

    new_arr->len = arr->len;

    return (LCValue) { { .ptr_val = (LCObject*)new_arr },  LC_TY_ARRAY };
}

LCValue lc_std_array_filter(LCRuntime* rt, LCValue this, int arg_len, LCValue* args) {
    LCValue item, test_tmp;
    LCValue result = LCNewArray(rt);
    LCArray* arr = (LCArray*)this.ptr_val;
    uint32_t i;

    for (i = 0; i < arr->len; i++) {
        item = arr->data[i];
        test_tmp = LCEvalLambda(rt, args[0], 1, (LCValue[]) { item });
        if (test_tmp.int_val) {
            lc_std_array_push(rt, result, 1, (LCValue[]) { item });
        }
    }

    return result;
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

static force_inline uint16_t* new_widen_string(LCString* s) {
    uint16_t* r = malloc(sizeof(uint16_t) * s->length);
    for (int i = 0; i < s->length; i++) {
        r[i] = s->u.str8[i];
    }
    return r;
}

static no_inline int lc_std_string_cmp_slow(LCString* s1, LCString* s2) {
    const uint16_t* str1;
    const uint16_t* str2;
    uint16_t* r1 = NULL;
    uint16_t* r2 = NULL;

    if (s1->is_wide_char) {
        str1 = s1->u.str16;
    } else {
        r1 = new_widen_string(s1);
        str1 = r1;
    }

    if (s2->is_wide_char) {
        str2 = s2->u.str16;
    } else {
        r2 = new_widen_string(s2);
        str2 = r2;
    }

    int result = memcmp(str1, str2, s1->length * 2);

    if (r1) {
        free(r1);
    }

    if (r2) {
        free(r2);
    }

    return result;
}

LCValue lc_std_string_cmp(LCRuntime* rt, LCCmpType cmp_type, LCValue left, LCValue right) {
    int cmp_result, hash1, hash2;
    LCString* s1 = (LCString*)(left.ptr_val);
    LCString* s2 = (LCString*)(right.ptr_val);

    // quick check
    if (cmp_type == LC_CMP_EQ) {
        if (s1->length != s2->length) {
            return LCFalse;
        }

        if (s1->is_wide_char != s2->is_wide_char) {
            return LCFalse;
        }
        hash1 = s1->hash;
        hash2 = s2->hash;

        if (hash1 != 0 && hash2 != 0 && hash1 != hash2) {
            return LCFalse;
        }
    }

    cmp_result = s1->length - s2->length;
    if (cmp_result != 0) {
        goto cmp;
    }

    if (!s1->is_wide_char && !s2->is_wide_char) {
        cmp_result = strcmp((const char*)s1->u.str8, (const char *)s2->u.str8);
    } else {
        cmp_result = lc_std_string_cmp_slow(s1, s2);
    }

cmp:
    switch (cmp_type) {
    case LC_CMP_EQ:
        return cmp_result == 0 ? LCTrue : LCFalse;

    case LC_CMP_NEQ:
        return cmp_result != 0 ? LCTrue : LCFalse;

    case LC_CMP_LT:
        return cmp_result < 0 ? LCTrue : LCFalse;

    case LC_CMP_LTEQ:
        return cmp_result <= 0 ? LCTrue : LCFalse;

    case LC_CMP_GT:
        return cmp_result > 0 ? LCTrue : LCFalse;

    case LC_CMP_GTEQ:
        return cmp_result >= 0 ? LCTrue : LCFalse;

    }
    return LCFalse;
}

LCValue lc_std_string_slice(LCRuntime* rt, LCValue this, int arg_len, LCValue* args) {
    int begin = args[0].int_val;
    int end = args[1].int_val;
    LCString* s = (LCString*)(this.ptr_val);
    LCString* result;
    int max_len = s->length;
    int need_len; 
    uint32_t acquire_len;

    begin = min_int(max_int(begin, 0), max_len);
    end = min_int(max_int(end, 0), max_len);

    need_len = end - begin;

    if (begin >= end) {
        return lc_new_string8(rt, NULL, 0);
    }

    if (!s->is_wide_char) {
        acquire_len = sizeof(LCString) + need_len + 1;
        result = lc_mallocz(rt, acquire_len);

        LCInitObject(&result->header, LC_TY_STRING);

        memcpy(result->u.str8, s->u.str8 + begin, need_len);
        result->u.str8[need_len] = 0;

        result->length = need_len;
        result->is_wide_char = 0;
        result->hash = 0;

        return MK_STRING(result);
    }

    acquire_len = sizeof(LCString) + need_len * 2;
    LCInitObject(&result->header, LC_TY_STRING);
    memcpy(result->u.str16, s->u.str16 + begin, need_len * 2);
    result->length = need_len;
    result->is_wide_char = 1;
    result->hash = 0;

    return MK_STRING(result);
}

LCValue lc_std_string_get_char(LCRuntime* rt, LCValue this, int arg_len, LCValue* args) {
    int index = args[0].int_val;
    LCString* str = (LCString*)this.ptr_val;
    if (index < 0 || index >= str->length) {
        fprintf(stderr, "[LichenScript] Panic: index %d out of range, size: %d\n", index, str->length);
        lc_panic_internal();
    }
    if (str->is_wide_char) {
        return MK_CHAR(str->u.str16[index]);
    }
    return MK_CHAR(str->u.str8[index]);
}

typedef struct LCMapTuple {
    LCMapTuple* prev;
    LCMapTuple* next;
    LCValue key;
    LCValue value;
} LCMapTuple;

typedef struct LCMapBucket {
    uint32_t hash;
    struct LCMapBucket* next;
    LCMapTuple* data;
} LCMapBucket;

LCValue lc_std_map_new(LCRuntime* rt, int key_ty, int init_size) {
    LCMap* map = (LCMap*)lc_mallocz(rt, sizeof (LCMap));
    map->header.count = 1;
    map->key_ty = key_ty;
    if (init_size >= 0 && init_size < 8) {
        map->is_small = 1;
    } else {
        map->is_small = 0;
    }

    if (key_ty == LC_TY_BOOL) {
        map->is_small = 1;
    }

    map->buckets = NULL;

    return (LCValue){ { .ptr_val = (LCObject*)map }, LC_TY_MAP };
}

static uint32_t LCGetStringHash(LCRuntime*rt, LCValue val) {
    LCString* str = (LCString*)val.ptr_val;
    if (str->hash != 0) {
        return str->hash;
    }

    if (str->is_wide_char) {
        str->hash = hash_string16(str->u.str16, str->length, rt->seed);
    } else {
        str->hash = hash_string8(str->u.str8, str->length, rt->seed);
    }

    return str->hash;
}

static inline uint32_t LCValueHash(LCRuntime* rt, LCValue val) {
    switch (val.tag) {
    case LC_TY_I32:
    case LC_TY_BOOL:
    case LC_TY_CHAR:
        return hash_int(val.int_val, rt->seed);

    case LC_TY_STRING:
        return LCGetStringHash(rt, val);
    
    default:
        return 0;
    }
}

static inline int LCMapKeyEq(LCRuntime* rt, LCValue a, LCValue b) {
    switch (a.tag) {
    case LC_TY_I32:
    case LC_TY_BOOL:
    case LC_TY_CHAR:
        if (a.int_val == b.int_val) {
            return 1;
        }
        break;

    case LC_TY_STRING:
        if (lc_std_string_cmp(rt, LC_CMP_EQ, a, b).int_val != 0) {
            return 1;
        }

        break;
    
    default:
        break;
    }
    return 0;
}

static no_inline LCMapTuple* lc_std_map_find_tuple(LCRuntime*rt, LCMap* map, LCValue key) {
    LCMapTuple* t = map->head;
    LCMapTuple* tmp;
    LCMapBucket *b, *bt;
    uint32_t hash;

    if (!map->is_small) {
        hash = LCValueHash(rt, key);
        uint32_t bucket_index = hash % (uint32_t)map->bucket_size;
        b = map->buckets[bucket_index];

        while (b != NULL) {
            bt = b->next;

            if (b->hash == hash && LCMapKeyEq(rt, b->data->key, key)) {
                return b->data;
            }

            b = bt;
        }

        return NULL;
    }

    while (t != NULL) {
        tmp = t->next;

        if (LCMapKeyEq(rt, t->key, key)) {
            return t;
        }

        t = tmp;
    }

    return NULL;
}

#define LC_MAP_DEFAULT_BUCKET_SIZE 16

static void lc_std_map_construct_hashtable(LCRuntime* rt, LCMap* map) {
    LCMapTuple *t, *tmp;
    uint32_t hash;
    LCMapBucket* bucket;
    int bucket_index;
    t = map->head;

    int bucket_size = LC_MAP_DEFAULT_BUCKET_SIZE;
    LCMapBucket** buckets = lc_mallocz(rt, sizeof(LCMapBucket*) * bucket_size);

    while (t != NULL) {
        tmp = t->next;

        hash = LCValueHash(rt, t->key);
        bucket_index = hash % bucket_size;

        bucket = lc_malloc(rt, sizeof(LCMapBucket));
        bucket->data = t;
        bucket->hash = hash;
        bucket->next = buckets[bucket_index];

        buckets[bucket_index] = bucket;

        t = tmp;
    }

    map->bucket_size = bucket_size;
    map->buckets = buckets;
    map->is_small = 0;
}

LCValue lc_std_map_set(LCRuntime* rt, LCValue this, int argc, LCValue* args) {
    LCMapTuple *found_tuple, *tuple;
    LCMapBucket *bucket;
    uint32_t hash;
    int index;
    LCMap* map = (LCMap*)this.ptr_val;

    found_tuple = lc_std_map_find_tuple(rt, map, args[0]);
    if (found_tuple == NULL) { // not found, add to the linked list
        LCRetain(args[0]);
        LCRetain(args[1]);

        tuple = (LCMapTuple*)lc_malloc(rt, sizeof(LCMapTuple));
        tuple->key = args[0];
        tuple->value = args[1];

        tuple->prev = map->last;
        tuple->next = NULL;

        map->size++;
        if (map->last != NULL) {
            map->last->next = tuple;
        }
        map->last = tuple;
        if (map->head == NULL) {
            map->head = tuple;
        }

        if (map->is_small) {
            if (map->size >= LC_SMALL_MAP_THRESHOLD && (
                map->key_ty == LC_TY_STRING || map->key_ty == LC_TY_I32 || map->key_ty == LC_TY_CHAR)) {

                lc_std_map_construct_hashtable(rt, map);
            }

            return MK_NULL();
        } else {
            hash = LCValueHash(rt, args[0]);
            index = hash % map->bucket_size;

            bucket = (LCMapBucket*)lc_malloc(rt, sizeof(LCMapBucket));
            bucket->data = tuple;
            bucket->hash = hash;
            bucket->next = map->buckets[index];

            map->buckets[index] = bucket;

            return MK_NULL();
        }
    } else {  // found in hashmap, replace exist value
        LCRelease(rt, tuple->value);
        LCRetain(args[1]);
        tuple->value = args[1];
        return MK_NULL();
    }
}

static inline void lc_free_map_tuple(LCRuntime* rt, LCMapTuple* tuple) {
    LCRelease(rt, tuple->key);
    LCRelease(rt, tuple->value);
    lc_free(rt, tuple);
}

void lc_std_map_free(LCRuntime* rt, LCValue val) {
    LCMap* map = (LCMap*)val.ptr_val;
    LCMapTuple *ptr, *tmp;
    LCMapBucket *bucket, *bp;

    if (likely(map->size > 0)) {
        if (map->is_small) {
            ptr = map->head;

            while (ptr != NULL) {
                tmp = ptr->next;

                lc_free_map_tuple(rt, ptr);

                ptr = tmp;
            }

            goto clean;
        }

        for (int i = 0; i < map->bucket_size; i++) {
            bucket = map->buckets[i];

            while (bucket != NULL) {
                bp = bucket->next;

                lc_free_map_tuple(rt, bucket->data);

                lc_free(rt, bucket);

                bucket = bp;
            }
        }
        lc_free(rt, map->buckets);
    }

clean:
    lc_free(rt, map);
}

LCValue lc_std_map_get(LCRuntime* rt, LCValue this, int argc, LCValue* args) {
    LCMapTuple *t, *tmp;
    LCMap* map = (LCMap*)this.ptr_val;

    t = lc_std_map_find_tuple(rt, map, args[0]);
    if (t == NULL) {
        return /* None */MK_UNION(1);
    }

    return /* Some(result) */LCNewUnionObject(rt, 0, 1, (LCValue[]) { t->value });
}

static void lc_std_map_become_small(LCRuntime* rt, LCMap* map) {
    int i;
    LCMapBucket *bucket, *next;

    for (i = 0; i < map->bucket_size; i++) {
        bucket = map->buckets[i];

        while (bucket != NULL) {
            next = bucket->next;

            lc_free(rt, bucket);

            bucket = next;
        }
    }

    lc_free(rt, map->buckets);
    map->buckets = NULL;

    map->is_small = 1;
}

static inline LCValue lc_std_map_remove_complex(LCRuntime* rt, LCMap* map, LCValue key) {
    LCMapTuple* t;
    LCMapBucket **bucket_ref, *next_bucket;
    uint32_t hash = LCValueHash(rt, key);
    int index = hash % map->bucket_size;
    LCValue result;

    bucket_ref = &map->buckets[index];

    while ((*bucket_ref) != NULL) {

        if ((*bucket_ref)->hash == hash && LCMapKeyEq(rt, (*bucket_ref)->data->key, key)) {
            result = LCNewUnionObject(rt, 0, 1, (LCValue[]){ t->value });

            t = (*bucket_ref)->data;

            if (t->prev) {
                t->prev->next = t->next;
            } else {  // is first
                map->head = t->next;
            }

            if (t->next) {
                t->next->prev = t->prev;
            } else {  // is last
                map->last = t->prev;
            }

            lc_free_map_tuple(rt, t);

            next_bucket = (*bucket_ref)->next;
            lc_free(rt, *bucket_ref);
            (*bucket_ref) = next_bucket;

            if (--map->size < LC_SMALL_MAP_THRESHOLD) {
                lc_std_map_become_small(rt, map);
            }

            return result;
        }

        bucket_ref = &((*bucket_ref)->next);
    }

    return  /* None */MK_UNION(1);
}

LCValue lc_std_map_remove(LCRuntime* rt, LCValue this, int argc, LCValue* args) {
    LCMapTuple *t, *tmp;
    LCMap* map = (LCMap*)this.ptr_val;
    LCValue result;

    if (map->is_small) {
        t = map->head;

        while (t != NULL) {
            tmp = t->next;

            if (LCMapKeyEq(rt, t->key, args[0])) {
                result = LCNewUnionObject(rt, 0, 1, (LCValue[]){ t->value });

                if (t->prev) {
                    t->prev->next = t->next;
                } else {  // is first
                    map->head = t->next;
                }

                if (t->next) {
                    t->next->prev = t->prev;
                } else {  // is last
                    map->last = t->prev;
                }

                lc_free_map_tuple(rt, t);
                map->size--;

                return result;
            }

            t = tmp;
        }

        return  /* None */MK_UNION(1);
    }

    return lc_std_map_remove_complex(rt, map, args[0]);
}

LCValue lc_std_map_size(LCRuntime* rt, LCValue this, int argc, LCValue* args) {
    LCMap* map = (LCMap*)this.ptr_val;
    return MK_I32(map->size);
}

LCValue lc_std_exit(LCRuntime* rt, LCValue this, int argc, LCValue* args) {
    int code = args[0].int_val;
    exit(code);
}

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wreturn-type"
LCValue lc_std_panic(LCRuntime* rt, LCValue this, int argc, LCValue* args) {
    lc_std_print(rt, this, argc, args);

    lc_panic_internal();
}
#pragma GCC diagnostic pop
