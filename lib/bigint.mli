val limb_bits : int
val base : int
val base_mask : int

type t = { sign : int; limbs : int array }

val zero : t
val one : t
val minus_one : t
val is_zero : t -> bool
val make : int -> int array
val normalize : t -> t
val of_int : int -> t
val to_int : t -> int
val of_int64 : int64 -> t
val cmp_mag : 'a array -> 'a array -> int
val sign : t -> int
val compare : t -> t -> int
val equal : t -> t -> bool
val gt : t -> t -> bool
val lt : t -> t -> bool
val ge : t -> t -> bool
val le : t -> t -> bool
val neg : t -> t
val abs : t -> t
val make_from_sign_and_limbs : int -> int array -> t
val add_mag : int array -> int array -> int array
val sub_mag : int array -> int array -> int array
val add : t -> t -> t
val sub : t -> t -> t
val mul : t -> t -> t
val divrem_small : t -> int -> t * int
val div_rem : t -> t -> t * t
val div : t -> t -> t
val rem : t -> t -> t
val pow : t -> int -> t
val to_twos_comp : t -> int -> int array
val of_twos_comp : int array -> t
val bitwise_op : t -> t -> (int -> int -> int) -> t
val logand : t -> t -> t
val logor : t -> t -> t
val logxor : t -> t -> t
val lognot : t -> t
val shift_left : t -> int -> t
val shift_right : t -> int -> t
val to_string : t -> string
val of_string : string -> t
val to_float : t -> float
val of_float : float -> t
val to_int64 : t -> int64
val pp : Format.formatter -> t -> unit
