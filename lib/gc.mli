open Opcode

type heap_obj =
  | HString of string
  | HArray of float array
  | HClosure of string * opcode list * (string * (value * bool)) list

and value =
  | VFloat of float
  | VInt of int
  | VInt64 of int64
  | VBool of bool
  | VByte of char
  | VHeapRef of int
  | VArray of value list
  | VTuple of value list
  | VUnit
  | VModule of (string, value) Hashtbl.t

val allocation_count : int ref
val allocation_threshold : int ref
val alloc_id : unit -> int
val alloc_in_young : heap_obj -> value
val find_heap_obj : int -> heap_obj
val get_string : int -> string option
val get_array : int -> float array option
val mark_and_promote : value list -> (string * (value * bool)) list -> unit
val reset_heap : unit -> unit
val maybe_collect_gc : value list -> (string * (value * bool)) list -> unit
val get_stack_roots : value Stack.t -> value list

val alloc_string_with_gc :
  value Stack.t -> (string * (value * bool)) list -> string -> value

val alloc_array_with_gc :
  value Stack.t -> (string * (value * bool)) list -> float array -> value
