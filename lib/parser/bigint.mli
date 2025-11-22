val limb_bits : int
(** Number of bits per limb in the internal representation. *)

val base : int
(** The numeric base for each limb, typically [1 lsl limb_bits]. *)

val base_mask : int
(** A bitmask used to isolate the lower [limb_bits] bits of a limb. *)

type t = { sign : int; limbs : int array }
(** A big integer represented by a sign and an array of limbs. Limbs are stored
    in little-endian order (least significant first).

    @param sign Either [-1], [0], or [1].
    @param limbs The magnitude stored as an array of unsigned limb values. *)

val zero : t
(** The integer zero. *)

val one : t
(** The integer one. *)

val minus_one : t
(** The integer minus one. *)

val is_zero : t -> bool
(** [is_zero x] returns [true] if [x] is equal to zero. *)

val make : int -> int array
(** [make n limbs] creates a limb array of length [n] initialized with zeroes.
*)

val normalize : t -> t
(** [normalize x] removes leading zero limbs and fixes the sign of [x]. *)

val of_int : int -> t
(** Convert an OCaml [int] to a big integer. *)

val to_int : t -> int
(** Convert a big integer to an OCaml [int].

    @raise Overflow if the value does not fit into an OCaml [int]. *)

val of_int64 : int64 -> t
(** Convert an OCaml [int64] to a big integer. *)

val cmp_mag : 'a array -> 'a array -> int
(** Compare two limb arrays by magnitude only.
    @return [-1], [0], or [1]. *)

val sign : t -> int
(** Return the sign of the integer: [-1], [0], or [1]. *)

val compare : t -> t -> int
(** Compare two big integers.
    @return [-1], [0], or [1]. *)

val equal : t -> t -> bool
(** [equal a b] is [true] if [a = b]. *)

val gt : t -> t -> bool
(** Greater-than comparison. *)

val lt : t -> t -> bool
(** Less-than comparison. *)

val ge : t -> t -> bool
(** Greater-or-equal comparison. *)

val le : t -> t -> bool
(** Less-or-equal comparison. *)

val neg : t -> t
(** Unary negation. *)

val abs : t -> t
(** Absolute value. *)

val make_from_sign_and_limbs : int -> int array -> t
(** Construct a big integer directly from a sign and limb array.
    @param sign Must be [-1], [0], or [1].
    @param limbs The magnitude (not automatically normalized). *)

val add_mag : int array -> int array -> int array
(** Add magnitudes (positive only), without handling signs. *)

val sub_mag : int array -> int array -> int array
(** Subtract magnitudes (positive only), assuming the first operand is ≥ the
    second. *)

val add : t -> t -> t
(** Big integer addition. *)

val sub : t -> t -> t
(** Big integer subtraction. *)

val mul : t -> t -> t
(** Big integer multiplication. *)

val divrem_small : t -> int -> t * int
(** Divide by a small integer.
    @return (quotient, remainder). *)

val div_rem : t -> t -> t * t
(** Full division with remainder.
    @return (quotient, remainder). *)

val div : t -> t -> t
(** Integer division. *)

val rem : t -> t -> t
(** Remainder. *)

val pow : t -> int -> t
(** Exponentiation: [pow x n] computes [x^n] for non-negative [n]. *)

val to_twos_comp : t -> int -> int array
(** Convert to two’s complement form using the given bit width. *)

val of_twos_comp : int array -> t
(** Construct from a two’s complement limb array. *)

val bitwise_op : t -> t -> (int -> int -> int) -> t
(** Perform a bitwise operator on two integers (e.g. [land], [lor], [lxor]). *)

val logand : t -> t -> t
(** Bitwise AND. *)

val logor : t -> t -> t
(** Bitwise OR. *)

val logxor : t -> t -> t
(** Bitwise XOR. *)

val lognot : t -> t
(** Bitwise NOT. *)

val shift_left : t -> int -> t
(** Shift left by a number of bits. *)

val shift_right : t -> int -> t
(** Arithmetic right shift. *)

val to_string : t -> string
(** Convert to a decimal string. *)

val of_string : string -> t
(** Parse a decimal string into a big integer.

    @raise Stdlib.Invalid_argument if the string is not a valid integer. *)

val to_float : t -> float
(** Convert to a [float] (with potential loss of precision). *)

val of_float : float -> t
(** Convert from a [float], truncating the fractional part. *)

val to_int64 : t -> int64
(** Convert to [int64], raising Overflow on overflow. *)

val to_char : t -> char
(** Convert to a char, interpreting the integer as an ASCII code. *)

val pp : Format.formatter -> t -> unit
(** Pretty-printer for use with [Format]. *)
