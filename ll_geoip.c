#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/socketaddr.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <GeoIP.h>
#include <GeoIPCity.h>
#include "miscmacs.h"
#include "cpp.h"

static GeoIP *geoip_handler;

value geoip_init(value database_)
{
    CAMLparam1(database_);

    geoip_handler = GeoIP_open(String_val(database_), GEOIP_INDEX_CACHE);

    if (! geoip_handler) {
        fprintf(stderr, "Error opening database %s\n", String_val(database_));
        failwith("Cannot initialize libgeoip");
    }

    CAMLreturn(Val_unit);
}

// Given an IP, return its location (longitude/latitude/country)

value geoip_location(value ip_ /* an inet_addr */)
{
    CAMLparam1(ip_);
    CAMLlocal1(ret);

    struct in_addr const *ip = &GET_INET_ADDR(ip_);
    long ipnum = ip->s_addr;

    assert(geoip_handler);
    GeoIPRecord *record = GeoIP_record_by_ipnum(geoip_handler, ipnum);
    if (record) {
        ret = caml_alloc_tuple(3);
        Store_field(ret, 0, caml_copy_string(record->country_code ? record->country_code : ""));
        Store_field(ret, 1, caml_copy_double(record->latitude));
        Store_field(ret, 2, caml_copy_double(record->longitude));
        GeoIPRecord_delete(record);
    } else {
        fprintf(stderr, "Cannot GeoIP_record_by_ipnum\n");
        caml_failwith("GeoIP_record_by_ipnum");
    }
    
    CAMLreturn(ret);
}

