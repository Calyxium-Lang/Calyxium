#include <stdint.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>

CAMLprim value u64_add(value a, value b) {
    CAMLparam2(a, b);
    uint64_t x = Int64_val(a);
    uint64_t y = Int64_val(b);
    CAMLreturn(caml_copy_int64(x + y));
}

CAMLprim value u64_sub(value a, value b) {
    CAMLparam2(a, b);
    uint64_t x = Int64_val(a);
    uint64_t y = Int64_val(b);
    CAMLreturn(caml_copy_int64(x + y));
}

CAMLprim value u64_mul(value a, value b) {
    CAMLparam2(a, b);
    uint64_t x = Int64_val(a);
    uint64_t y = Int64_val(b);
    CAMLreturn(caml_copy_int64(x + y));
}

CAMLprim value u64_div(value a, value b) {
    CAMLparam2(a, b);
    uint64_t x = Int64_val(a);
    uint64_t y = Int64_val(b);
    if (y == 0) caml_failwith("Uint64.div: division by zero");
    CAMLreturn(caml_copy_int64(x / y));
}

CAMLprim value u64_rem(value a, value b) {
    CAMLparam2(a, b);
    uint64_t x = Int64_val(a);
    uint64_t y = Int64_val(b);
    if (y == 0) caml_failwith("Uint64.rem: division by zero");
    CAMLreturn(caml_copy_int64(x % y));
}

CAMLprim value u64_and(value a, value b) {
    CAMLparam2(a,b);
    uint64_t x = Int64_val(a);
    uint64_t y = Int64_val(b);
    CAMLreturn(caml_copy_int64(x & y));
}

CAMLprim value u64_or(value a, value b) {
    CAMLparam2(a,b);
    uint64_t x = Int64_val(a);
    uint64_t y = Int64_val(b);
    CAMLreturn(caml_copy_int64(x | y));
}

CAMLprim value u64_xor(value a, value b) {
    CAMLparam2(a,b);
    uint64_t x = Int64_val(a);
    uint64_t y = Int64_val(b);
    CAMLreturn(caml_copy_int64(x ^ y));
}

CAMLprim value u64_shl(value a, value b) {
    CAMLparam2(a,b);
    uint64_t x = Int64_val(a);
    uint64_t n = Int64_val(b);
    CAMLreturn(caml_copy_int64(x << (n & 63)));
}

CAMLprim value u64_shr(value a, value b) {
    CAMLparam2(a,b);
    uint64_t x = Int64_val(a);
    uint64_t n = Int64_val(b);
    CAMLreturn(caml_copy_int64(x >> (n & 63)));
}

CAMLprim value u64_not(value a) {
    CAMLparam1(a);
    uint64_t x = Int64_val(a);
    CAMLreturn(caml_copy_int64(~x));
}

CAMLprim value u64_eq(value a, value b) {
    CAMLparam2(a,b);
    uint64_t x = Int64_val(a);
    uint64_t y = Int64_val(b);
    CAMLreturn(Val_bool(x == y));
}

CAMLprim value u64_compare(value a, value b) {
    CAMLparam2(a, b);
    uint64_t x = Int64_val(a);
    uint64_t y = Int64_val(b);
    CAMLreturn(Val_int((x > y) - (x < y)));
}