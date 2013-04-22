#include <stdlib.h>
#include <stdio.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>

extern char **environ;

static char const *getenv_n(unsigned i)
{
    return environ[i];
}

value wrap_getenv_n(value n_)
{
    CAMLparam1(n_);
    CAMLlocal1(ret);
    unsigned n = Long_val(n_);
    ret = caml_copy_string(getenv_n(n));
    CAMLreturn(ret);
}

value wrap_nb_envvars(void)
{
    CAMLparam0();
    CAMLlocal1(ret);
    unsigned n;
    for (n = 0; getenv_n(n); n++) ;
    ret = Val_long(n);
    CAMLreturn(ret);
}
