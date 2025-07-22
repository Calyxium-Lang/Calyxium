/*
** $Id: cmathlib.c $
** Standard mathematical library
*/

#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <math.h>

#define UNARY(name, fun)                        \
    CAMLprim value caml_##name(value v_x) {     \
        CAMLparam1(v_x);                        \
        double x = Double_val(v_x);             \
        CAMLreturn((fun(x)));   \
    }                                           

#define BINARY(name, fun)                               \
    CAMLprim value caml_##name(value v_x, value v_y) {  \
        CAMLparam2(v_x, v_y);                           \
        double x = Double_val(v_x);                     \
        double y = Double_val(v_y);                     \
        CAMLreturn(caml_copy_double(fun(x, y)));        \
    }                                                   

UNARY(sin,  sin)
UNARY(cos,  cos)
UNARY(tan,  tan)
UNARY(asin, asin)
UNARY(acos, acos)
UNARY(atan, atan)
BINARY(atan2, atan2)
UNARY(exp,  exp)
UNARY(log,  log)
UNARY(log10, log10)
BINARY(pow,  pow)
UNARY(sqrt, sqrt)
#ifdef HAVE_CBRT
UNARY(cbrt, cbrt)
#endif
UNARY(sinh, sinh)
UNARY(cosh, cosh)
UNARY(tanh, tanh)
