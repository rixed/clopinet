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

//#define DEBUG_OBUF
//#define DEBUG_IBUF

/*
 * Tools
 */

static void sys_error(char const *pref)
{
    CAMLparam0();
    CAMLlocal1(str);
    char msg[1024];
    snprintf(msg, sizeof(msg), "%s: %s", pref, strerror(errno));
#   if defined(DEBUG_IBUF) || defined(DEBUG_OBUF)
    fprintf(stderr, "!!%s!!\n", msg);
#   endif
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

/* FIXME: lock opened obufs (cd dbfile.ml), or even better: flush at record boundary */

struct obuf {
    int fd;
    unsigned next;
#   define OBUFLEN 16384
    uint8_t buf[OBUFLEN];
};

static void obuf_ctor(struct obuf *ob, char const *fname)
{
#   ifndef O_CLOEXEC
#       define O_CLOEXEC 0
#   endif
#   ifndef O_LARGEFILE
#       define O_LARGEFILE 0
#   endif
#   ifndef O_NOATIME
#       define O_NOATIME 0
#   endif
    ob->fd = open(fname, O_WRONLY|O_CREAT|O_APPEND|O_CLOEXEC|O_LARGEFILE, 0644);
    if (ob->fd < 0) {
        sys_error("open");
    }
    ob->next = 0;
}

static void obuf_flush(struct obuf *ob)
{
    really_write(ob->fd, ob->buf, ob->next);
}

static void obuf_make_room(struct obuf *ob, unsigned sz)
{
    if (ob->next + sz <= OBUFLEN) return;
    obuf_flush(ob);
    ob->next = 0;
}

static void obuf_dtor(struct obuf *ob)
{
#   ifdef DEBUG_OBUF
    fprintf(stderr, "Destruct obuf while fd=%d, buf=%p and next=%u\n", ob->fd, ob->buf, ob->next);
#   endif
    if (ob->fd < 0) return;
    obuf_flush(ob);
    (void)close(ob->fd);
    ob->fd = -1;
}

static void obuf_finalize(value custom)
{
    struct obuf *ob = Data_custom_val(custom);
#   ifdef DEBUG_OBUF
    fprintf(stderr, "Finalize obuf while buf=%p and next=%u\n", ob->buf, ob->next);
#   endif
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
    unsigned next;
    unsigned stop;
    int fd;
    uint8_t eof:1;
#   define IBUFLEN 16384
    uint8_t buf[IBUFLEN];
};

static int ibuf_ctor(struct ibuf *ib, char const *fname)
{
    ib->fd = open(fname, O_RDONLY|O_CLOEXEC|O_LARGEFILE|O_NOATIME, 0644);
    if (ib->fd < 0) {
        sys_error("open");
    }
    ib->next = ib->stop = 0;
    ib->eof = 0;
#   ifdef DEBUG_IBUF
    fprintf(stderr, "open %s -> %d\n", fname, ib->fd);
#   endif
    return 0;
}

static void ibuf_dtor(struct ibuf *ib)
{
    if (ib->fd < 0) return;
#   ifdef DEBUG_IBUF
    fprintf(stderr, "close %d\n", ib->fd);
#   endif
    (void)close(ib->fd);
    ib->fd = -1;
}

static void ibuf_refill(struct ibuf *ib)
{
    // copy what's left at the beginning
    memmove(ib->buf, ib->buf + ib->next, ib->stop - ib->next);
    ib->stop -= ib->next;
    ib->next = 0;
    if (! ib->eof) {
        // fill with fd
        size_t const sz = IBUFLEN - ib->stop;
        size_t const r = really_read(ib->fd, ib->buf + ib->stop, sz);
        if (r < sz) ib->eof = 1;
        ib->stop += r;
    }
    if (ib->stop == 0) {
        caml_raise_end_of_file();
    }
}

static void ibuf_make_available(struct ibuf *ib, unsigned sz)
{
    if (ib->stop - ib->next >= sz) return;
    ibuf_refill(ib);
#   ifdef DEBUG_IBUF
    fprintf(stderr, "fd %d, after ibuf_make_available for sz=%u: next=%u, stop=%u, eof=%s\n", ib->fd, sz, ib->next, ib->stop, ib->eof?"y":"n");
#   endif
    assert(ib->stop - ib->next >= sz);
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
value wrap_read##width(value custom) \
{ \
    CAMLparam1(custom); \
    struct ibuf *ib = Data_custom_val(custom); \
    ibuf_make_available(ib, width/8); \
    uint##width##_t v; \
    memcpy(&v, ib->buf + ib->next, width/8); \
    ib->next += width/8; \
    CAMLreturn(Val_long(v)); \
}

#define UNBOXED_WRITE(width) \
void wrap_write##width(value custom, value v_) \
{ \
    CAMLparam2(custom, v_); \
    struct obuf *ob = Data_custom_val(custom); \
    obuf_make_room(ob, width/8); \
    uint##width##_t v = Long_val(v_); /* Won't work on big endian */ \
    memcpy(ob->buf + ob->next, &v, width/8); \
    ob->next += width/8; \
    CAMLreturn0; \
}

#define BOXED_READ(width) \
value wrap_read##width(value custom) \
{ \
    CAMLparam1(custom); \
    CAMLlocal1(result); \
    struct ibuf *ib = Data_custom_val(custom); \
    ibuf_make_available(ib, width/8); \
    uint##width##_t v; \
    memcpy(&v, ib->buf + ib->next, width/8); \
    ib->next += width/8; \
    result = caml_copy_int##width(v); \
    CAMLreturn(result); \
}

#define BOXED_WRITE(width) \
void wrap_write##width(value custom, value v_) \
{ \
    CAMLparam2(custom, v_); \
    struct obuf *ob = Data_custom_val(custom); \
    obuf_make_room(ob, width/8); \
    uint##width##_t v = Int##width##_val(v_); \
    memcpy(ob->buf + ob->next, &v, width/8); \
    ob->next += width/8; \
    CAMLreturn0; \
}

UNBOXED_READ(8)
UNBOXED_WRITE(8)
UNBOXED_READ(16)
UNBOXED_WRITE(16)
BOXED_READ(32)
BOXED_WRITE(32)
BOXED_READ(64)
BOXED_WRITE(64)

value wrap_read1(value custom)
{
    CAMLparam1(custom);
    struct ibuf *ib = Data_custom_val(custom);
    ibuf_make_available(ib, 1);
    uint8_t v = ib->buf[ib->next++];
    CAMLreturn(Val_bool(v));
}

void wrap_write1(value custom, value v)
{
    CAMLparam2(custom, v);
    struct obuf *ob = Data_custom_val(custom);
    obuf_make_room(ob, 1);
    ob->buf[ob->next ++] = Bool_val(v);
    CAMLreturn0;
}

static unsigned long read_varuint(struct ibuf *ib)
{
    unsigned long n = 0;
    while (1) {
        ibuf_make_available(ib, 1);
        uint8_t const b = ib->buf[ib->next++];
#       ifdef DEBUG_IBUF
        fprintf(stderr, "<varuint septet 0x%x ... ", b);
#       endif
        if (b < 128) {
            n |= b;
            break;
        } else {
            n |= b & 0x7fU;
            n <<= 7;
        }
#       ifdef DEBUG_IBUF
        fprintf(stderr, "n = 0x%lx\n", n);
#       endif
    }
#   ifdef DEBUG_IBUF
    fprintf(stderr, "<varuint n = 0x%lx\n", n & 1 ? -nn : nn);
#   endif
    return n;
}

static long read_varint(struct ibuf *ib)
{
    unsigned long const n = read_varuint(ib);
    long const nn = n >> 1U; 
#   ifdef DEBUG_IBUF
    fprintf(stderr, "<varint n = 0x%lx\n", n & 1 ? -nn : nn);
#   endif
    return n & 1 ? -nn : nn;
}

value wrap_read_varint(value custom)
{
    CAMLparam1(custom);
    struct ibuf *ib = Data_custom_val(custom);
    CAMLreturn(Val_long(read_varint(ib)));
}

static void write_varuint(struct obuf *ob, unsigned long n)
{
    obuf_make_room(ob, 1+sizeof(long int)); // at worst
#   ifdef DEBUG_OBUF
    fprintf(stderr, ">varuint = 0x%lx\n", n);
#   endif
    // We serialize higher septets first because then deserialization is faster
    uint8_t vals[(sizeof(n)*8+6)/7];  // we store septets not octets
    for (unsigned s = 0; s < NB_ELEMS(vals); s++) {
        if (n < 128) {
            // write higher septets first
#           ifdef DEBUG_OBUF
            fprintf(stderr, ">varint septet 0x%x | 0x80/0\n", (uint8_t)(n & 0xff));
#           endif
            ob->buf[ob->next ++] = n | (s ? 128:0);
            while (s--) {
                ob->buf[ob->next ++] = vals[s] | (s ? 128:0);
#               ifdef DEBUG_OBUF
                fprintf(stderr, ">varint septet 0x%x | 0x80/0\n", vals[s]);
#               endif
            }
            break;
        } else {
            vals[s] = n & 0x7f;
#           ifdef DEBUG_OBUF
            fprintf(stderr, ">varint storing 0x%x\n", vals[s]);
#           endif
            n >>= 7;
        }
    }
}

static void write_varint(struct obuf *ob, long n)
{
    // store abs(v) lsl 1)+sign so that higher bits are 0
    write_varuint(ob, (labs(n) << 1U) + (n < 0));
}

void wrap_write_varint(value custom, value v_)
{
    CAMLparam2(custom, v_);
    struct obuf *ob = Data_custom_val(custom);
    write_varint(ob, Long_val(v_));
    CAMLreturn0;
}

value wrap_read_string(value custom)
{
    CAMLparam1(custom);
    CAMLlocal1(ret);
    struct ibuf *ib = Data_custom_val(custom);
    unsigned len = read_varuint(ib);
    ibuf_make_available(ib, len);
    ret = caml_alloc_string(len);
    memcpy(String_val(ret), ib->buf + ib->next, len);
    ib->next += len;
    CAMLreturn(ret);
}

void wrap_write_string(value custom, value s)
{
    CAMLparam2(custom, s);
    struct obuf *ob = Data_custom_val(custom);
    unsigned const len = caml_string_length(s);
    write_varuint(ob, len);
    obuf_make_room(ob, len);
    memcpy(ob->buf + ob->next, String_val(s), len);
    ob->next += len;
    CAMLreturn0;
}

// Read a tuple of bytes
value wrap_read_chars(value custom, value len_)
{
    CAMLparam2(custom, len_);
    CAMLlocal1(ret);
    struct ibuf *ib = Data_custom_val(custom);
    unsigned len = Long_val(len_);
    ret = caml_alloc_tuple(len);
    ibuf_make_available(ib, len);
    for (unsigned i = 0; i < len; i ++) {
        Store_field(ret, i, Val_int(ib->buf[ib->next++]));
    }
    CAMLreturn(ret);
}

void wrap_write_chars(value custom, value t)
{
    CAMLparam2(custom, t);
    struct obuf *ob = Data_custom_val(custom);
    unsigned const len = Wosize_val(t);
    obuf_make_room(ob, len);
    for (unsigned i = 0; i < len; i ++) {
        int const v = Int_val(Field(t, i));
        assert(v < 256 && v >= 0);
        ob->buf[ob->next++] = v;
    }
    CAMLreturn0;
}
