#include <stdint.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>

CAMLprim value u32_add(value a, value b) {
    CAMLparam2(a,b);
    uint32_t x = Int32_val(a);
    uint32_t y = Int32_val(b);
    CAMLreturn(caml_copy_int32(x + y));
}

CAMLprim value u32_sub(value a, value b) {
    CAMLparam2(a,b);
    uint32_t x = Int32_val(a);
    uint32_t y = Int32_val(b);
    CAMLreturn(caml_copy_int32(x - y));
}

CAMLprim value u32_mul(value a, value b) {
    CAMLparam2(a,b);
    uint32_t x = Int32_val(a);
    uint32_t y = Int32_val(b);
    CAMLreturn(caml_copy_int32(x * y));
}

CAMLprim value u32_div(value a, value b) {
    CAMLparam2(a, b);
    uint32_t x = Int32_val(a);
    uint32_t y = Int32_val(b);
    if (y == 0) caml_failwith("Uint32.div: division by zero");
    CAMLreturn(caml_copy_int32(x / y));
}

CAMLprim value u32_rem(value a, value b) {
    CAMLparam2(a, b);
    uint32_t x = Int32_val(a);
    uint32_t y = Int32_val(b);
    if (y == 0) caml_failwith("Uint32.rem: division by zero");
    CAMLreturn(caml_copy_int32(x % y));
}

CAMLprim value u32_and(value a, value b) {
    CAMLparam2(a,b);
    uint32_t x = Int32_val(a);
    uint32_t y = Int32_val(b);
    CAMLreturn(caml_copy_int32(x & y));
}

CAMLprim value u32_or(value a, value b) {
    CAMLparam2(a,b);
    uint32_t x = Int32_val(a);
    uint32_t y = Int32_val(b);
    CAMLreturn(caml_copy_int32(x | y));
}

CAMLprim value u32_xor(value a, value b) {
    CAMLparam2(a,b);
    uint32_t x = Int32_val(a);
    uint32_t y = Int32_val(b);
    CAMLreturn(caml_copy_int32(x ^ y));
}

CAMLprim value u32_shl(value a, value b) {
    CAMLparam2(a,b);
    uint32_t x = Int32_val(a);
    uint32_t n = Int32_val(b);
    CAMLreturn(caml_copy_int32(x << (n & 31)));
}

CAMLprim value u32_shr(value a, value b) {
    CAMLparam2(a,b);
    uint32_t x = Int32_val(a);
    uint32_t n = Int32_val(b);
    CAMLreturn(caml_copy_int32(x >> (n & 31)));
}

CAMLprim value u32_not(value a) {
    CAMLparam1(a);
    uint32_t x = Int32_val(a);
    CAMLreturn(caml_copy_int32(~x));
}

CAMLprim value u32_eq(value a, value b) {
    CAMLparam2(a,b);
    uint32_t x = Int32_val(a);
    uint32_t y = Int32_val(b);
    CAMLreturn(Val_bool(x == y));
}

CAMLprim value u32_compare(value a, value b) {
    CAMLparam2(a, b);
    uint32_t x = Int32_val(a);
    uint32_t y = Int32_val(b);
    CAMLreturn(Val_int((x > y) - (x < y)));
}