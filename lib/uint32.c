#include <stdint.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>

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