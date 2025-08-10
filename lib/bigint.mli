type t

val zero : t
val one : t
val of_int : int -> t
val to_int : t -> int
val of_int64 : int64 -> t
val to_int64 : t -> int64
val to_string : t -> string
val of_string : string -> t
val to_float : t -> float
val of_float : float -> t
val sign : t -> int
val compare : t -> t -> int
val equal : t -> t -> bool
val gt : t -> t -> bool
val neg : t -> t
val abs : t -> t
val add : t -> t -> t
val sub : t -> t -> t
val mul : t -> t -> t
val div : t -> t -> t
val rem : t -> t -> t
val pow : t -> int -> t
val logand : t -> t -> t
val logor : t -> t -> t
val logxor : t -> t -> t
val lognot : t -> t
val shift_left : t -> int -> t
val shift_right : t -> int -> t
