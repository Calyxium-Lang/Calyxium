open Format

type t = int32

external add : t -> t -> t = "u32_add"
external sub : t -> t -> t = "u32_sub"
external mul : t -> t -> t = "u32_mul"
external div : t -> t -> t = "u32_div"
external rem : t -> t -> t = "u32_rem"
external and_ : t -> t -> t = "u32_and"
external or_ : t -> t -> t = "u32_or"
external xor : t -> t -> t = "u32_xor"
external not : t -> t = "u32_not"
external shl : t -> t -> t = "u32_shl"
external shr : t -> t -> t = "u32_shr"
external eq : t -> t -> bool = "u32_eq"
external compare : t -> t -> int = "u32_compare"

let of_string (s : string) : t =
  let i = Int64.of_string s in
  Int64.to_int32 (Int64.logand i 0xFFFFFFFFL)

let to_string (x : t) : string =
  Int64.to_string (Int64.logand (Int64.of_int32 x) 0xFFFFFFFFL)

let to_int64 (x : t) : int64 = Int64.logand (Int64.of_int32 x) 0xFFFFFFFFL
let zero = of_string "0"
let one = of_string "1"
let succ x = add x one
let pred x = sub x one

let pp fmt (x : t) =
  let u64 = Int64.logand (Int64.of_int32 x) 0xFFFFFFFFL in
  fprintf fmt "%Ld" u64
