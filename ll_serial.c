#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <assert.h>
#include <errno.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/custom.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include "miscmacs.h"
#include "cpp.h"

/*
 * Tools
 */

static void sys_error(char const *pref)
{
    CAMLparam0();
    CAMLlocal1(str);
    char msg[1024];
    snprintf(msg, sizeof(msg), "%s: %s", pref, strerror(errno));
    //fprintf(stderr, "!!%s!!\n", msg);
    str = caml_copy_string(msg);
    caml_raise_sys_error(str);
    CAMLreturn0;
}

static void really_write(int fd, uint8_t *buf, size_t n)
{
    while (n>0) {
        ssize_t err = write(fd, buf, n);
        if (err < 0) {
            if (errno == EINTR) continue;
            sys_error("write");
        }
        n -= err;
        buf += err;
    }
}

static size_t really_read(int fd, uint8_t *buf, size_t n)
{
    size_t ret = 0;
    while (n>0) {
        ssize_t err = read(fd, buf, n);
        if (err < 0) {
            if (errno == EINTR) continue;
            sys_error("read");
        } else if (err == 0) {
            break;
        }
        n -= err;
        buf += err;
        ret += err;
    }
    return ret;
}

/*
 * Buffered output
 */

struct obuf {
    int fd;
    uint8_t *next;
#   define OBUFLEN 16384
    uint8_t buf[OBUFLEN];
};

static void obuf_ctor(struct obuf *ob, char const *fname)
{
    ob->fd = open(fname, O_WRONLY|O_CREAT|O_APPEND|O_CLOEXEC|O_LARGEFILE, 0644);
    if (ob->fd < 0) {
        sys_error("open");
    }
    ob->next = ob->buf;
}

static void obuf_flush(struct obuf *ob)
{
    really_write(ob->fd, ob->buf, ob->next - ob->buf);
}

static void obuf_make_room(struct obuf *ob, unsigned sz)
{
    if (ob->next + sz <= ob->buf + OBUFLEN) return;
    obuf_flush(ob);
    ob->next = ob->buf;
}

static void obuf_dtor(struct obuf *ob)
{
    if (ob->fd < 0) return;
    obuf_flush(ob);
    (void)close(ob->fd);
    ob->fd = -1;
}

static void obuf_finalize(value custom)
{
    struct obuf *ob = Data_custom_val(custom);
    obuf_dtor(ob);
}

static struct custom_operations obuf_ops = {
    .identifier = "mlrrd/obuf",
    .finalize = obuf_finalize,
    .compare = custom_compare_default,
    .compare_ext = custom_compare_ext_default,
    .hash = custom_hash_default,
    .serialize = custom_serialize_default,
    .deserialize = custom_deserialize_default,
};

value obuf_open(value fname)
{
    CAMLparam1(fname);
    CAMLlocal1(custom);
    custom = caml_alloc_custom(&obuf_ops, sizeof(struct obuf), 0, 1);
    struct obuf *ob = Data_custom_val(custom);
    obuf_ctor(ob, String_val(fname));
    CAMLreturn(custom);
}

void obuf_close(value custom)
{
    CAMLparam1(custom);
    struct obuf *ob = Data_custom_val(custom);
    obuf_dtor(ob);
    CAMLreturn0;
}

/*
 * Buffered input
 */

struct ibuf {
    uint8_t *next;
    uint8_t *stop;
    int fd;
    uint8_t eof:1;
#   define IBUFLEN 16384
    uint8_t start[IBUFLEN];
};

static int ibuf_ctor(struct ibuf *ib, char const *fname)
{
    ib->fd = open(fname, O_RDONLY|O_CLOEXEC|O_LARGEFILE|O_NOATIME, 0644);
    if (ib->fd < 0) {
        sys_error("open");
    }
    ib->next = ib->stop = ib->start;
    ib->eof = 0;
    return 0;
}

static void ibuf_dtor(struct ibuf *ib)
{
    if (ib->fd < 0) return;
    (void)close(ib->fd);
    ib->fd = -1;
}

static void ibuf_refill(struct ibuf *ib)
{
    // copy what's left at the beginning
    memmove(ib->start, ib->next, ib->stop - ib->next);
    ib->stop -= ib->next - ib->start;
    ib->next = ib->start;
    if (! ib->eof) {
        // fill with fd
        size_t const sz = IBUFLEN - (ib->stop - ib->start);
        size_t const r = really_read(ib->fd, ib->stop, sz);
        if (r < sz) ib->eof = 1;
        ib->stop += r;
    }
    if (ib->stop == ib->start) {
        caml_raise_end_of_file();
    }
}

static void ibuf_make_available(struct ibuf *ib, unsigned sz)
{
//    fprintf(stderr, "ibuf_make_available sz=%u\n", sz);
    if (ib->stop - ib->next >= (ptrdiff_t)sz) return;
    ibuf_refill(ib);
    assert(ib->stop - ib->next >= (ptrdiff_t)sz);
}

static void ibuf_finalize(value custom)
{
    struct ibuf *ob = Data_custom_val(custom);
    ibuf_dtor(ob);
}

static struct custom_operations ibuf_ops = {
    .identifier = "mlrrd/ibuf",
    .finalize = ibuf_finalize,
    .compare = custom_compare_default,
    .compare_ext = custom_compare_ext_default,
    .hash = custom_hash_default,
    .serialize = custom_serialize_default,
    .deserialize = custom_deserialize_default,
};

value ibuf_open(value fname)
{
    CAMLparam1(fname);
    CAMLlocal1(custom);
    custom = caml_alloc_custom(&ibuf_ops, sizeof(struct ibuf), 0, 1);
    struct ibuf *ib = Data_custom_val(custom);
    ibuf_ctor(ib, String_val(fname));
    CAMLreturn(custom);
}

void ibuf_close(value custom)
{
    CAMLparam1(custom);
    struct ibuf *ib = Data_custom_val(custom);
    ibuf_dtor(ib);
    CAMLreturn0;
}

/*
 * Readers/Writers for primitive types
 */ 

#define UNBOXED_READ(width) \
value read##width(value custom) \
{ \
    CAMLparam1(custom); \
    struct ibuf *ib = Data_custom_val(custom); \
    ibuf_make_available(ib, width/8); \
    uint##width##_t v; \
    memcpy(&v, ib->next, width/8); \
    ib->next += width/8; \
    CAMLreturn(Val_long(v)); \
}

#define UNBOXED_WRITE(width) \
void write##width(value custom, value v_) \
{ \
    CAMLparam2(custom, v_); \
    struct obuf *ob = Data_custom_val(custom); \
    obuf_make_room(ob, width/8); \
    uint##width##_t v = Long_val(v_); /* Won't work on big endian */ \
    memcpy(ob->next, &v, width/8); \
    ob->next += width/8; \
    CAMLreturn0; \
}

#define BOXED_READ(width) \
value read##width(value custom) \
{ \
    CAMLparam1(custom); \
    CAMLlocal1(result); \
    struct ibuf *ib = Data_custom_val(custom); \
    ibuf_make_available(ib, width/8); \
    uint##width##_t v; \
    memcpy(&v, ib->next, width/8); \
    ib->next += width/8; \
    result = caml_copy_int##width(v); \
    CAMLreturn(result); \
}

#define BOXED_WRITE(width) \
void write##width(value custom, value v_) \
{ \
    CAMLparam2(custom, v_); \
    struct obuf *ob = Data_custom_val(custom); \
    obuf_make_room(ob, width/8); \
    uint##width##_t v = Int##width##_val(v_); \
    memcpy(ob->next, &v, width/8); \
    ob->next += width/8; \
    CAMLreturn0; \
}

UNBOXED_READ(8)
UNBOXED_WRITE(8)
UNBOXED_READ(16)
UNBOXED_WRITE(16)
#if SIZEOF_LONG > 4
    UNBOXED_READ(32)
    UNBOXED_WRITE(32)
#else
    BOXED_READ(32)
    BOXED_WRITE(32)
#endif
BOXED_READ(64)
BOXED_WRITE(64)

value read1(value custom)
{
    CAMLparam1(custom);
    struct ibuf *ib = Data_custom_val(custom);
    ibuf_make_available(ib, 1);
    uint8_t v = *ib->next++;
    CAMLreturn(Val_bool(v));
}

void write1(value custom, value v)
{
    CAMLparam2(custom, v);
    struct obuf *ob = Data_custom_val(custom);
    obuf_make_room(ob, 1);
    *ob->next ++ = Bool_val(v);
    CAMLreturn0;
}

value read_varint(value custom)
{
    CAMLparam1(custom);
    struct ibuf *ib = Data_custom_val(custom);
    unsigned long n = 0;
    while (1) {
        ibuf_make_available(ib, 1);
        uint8_t const b = *ib->next++;
//        fprintf(stderr, "<varint septet 0x%x ... ", b);
        if (b < 128) {
            n |= b;
            break;
        } else {
            n |= b & 0x7fU;
            n <<= 7;
        }
//        fprintf(stderr, "n = 0x%lx\n", n);
    }
//    fprintf(stderr, "n+sign = 0x%lx\n", n);
    long const nn = n >> 1U; 
//    fprintf(stderr, "<varint n = 0x%lx\n", n & 1 ? -nn : nn);
    CAMLreturn(Val_long(n & 1 ? -nn : nn));
}

void write_varint(value custom, value v_)
{
    CAMLparam2(custom, v_);
    struct obuf *ob = Data_custom_val(custom);
    obuf_make_room(ob, 1+sizeof(long int)); // at worst
    // store abs(v) lsl 1)+sign so that higher bits are 0
    long const v = Long_val(v_);
//    fprintf(stderr, ">varint 0x%lx\n", v);
    unsigned long n = (labs(v) << 1U) + (v < 0);
//    fprintf(stderr, ">varint with sign = 0x%lx\n", n);
    // We serialize higher septets first because then deserialization is faster
    uint8_t vals[(sizeof(n)*8+6)/7];  // we store septets not octets
    for (unsigned s = 0; s < NB_ELEMS(vals); s++) {
        if (n < 128) {
            // write higher septets first
//            fprintf(stderr, ">varint septet 0x%x | 0x80/0\n", (uint8_t)(n & 0xff));
            *ob->next ++ = n | (s ? 128:0);
            while (s--) {
                *ob->next ++ = vals[s] | (s ? 128:0);
//                fprintf(stderr, ">varint septet 0x%x | 0x80/0\n", vals[s]);
            }
            break;
        } else {
            vals[s] = n & 0x7f;
//            fprintf(stderr, ">varint storing 0x%x\n", vals[s]);
            n >>= 7;
        }
    }
    CAMLreturn0;
}

value read_string(value custom, value len_)
{
    CAMLparam2(custom, len_);
    CAMLlocal1(ret);
    unsigned len = Int_val(len_);
    struct ibuf *ib = Data_custom_val(custom);
    ibuf_make_available(ib, len);
    ret = caml_alloc_string(len);
    memcpy(String_val(ret), ib->next, len);
    ib->next += len;
    CAMLreturn(ret);
}

void write_string(value custom, value s)
{
    CAMLparam2(custom, s);
    struct obuf *ob = Data_custom_val(custom);
    unsigned const len = caml_string_length(s);
    obuf_make_room(ob, len);
    memcpy(ob->next, String_val(s), len);
    ob->next += len;
    CAMLreturn0;
}

