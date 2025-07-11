open Format

type t = int64

external add : t -> t -> t = "u64_add"
external sub : t -> t -> t = "u64_sub"
external mul : t -> t -> t = "u64_mul"
external div : t -> t -> t = "u64_div"
external rem : t -> t -> t = "u64_rem"
external and_ : t -> t -> t = "u64_and"
external or_ : t -> t -> t = "u64_or"
external xor : t -> t -> t = "u64_xor"
external not : t -> t = "u64_not"
external shl : t -> t -> t = "u64_shl"
external shr : t -> t -> t = "u64_shr"
external eq : t -> t -> bool = "u64_eq"
external compare : t -> t -> int = "u64_compare"

let of_string = Int64.of_string
let to_string = Int64.to_string
let to_int64 x = x
let zero = 0L
let one = 1L
let succ x = add x one
let pred x = sub x one
let pp fmt x = fprintf fmt "%Lu" x
