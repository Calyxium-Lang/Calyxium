open Calyxium_parser

val interned_strings : (string, int) Hashtbl.t
(** Table of interned strings to their heap IDs. *)

type heap_obj = ..
(** Heap objects stored in the garbage-collected heap. *)

type value = ..
(** Runtime values of the language. *)

(** Runtime values variants. *)
type value +=
  | VFloat of float  (** Floating-point number. *)
  | VInt of Bigint.t  (** Arbitrary-precision integer. *)
  | VBool of bool  (** Boolean value. *)
  | VByte of char  (** Single byte/char. *)
  | VHeapRef of int  (** Reference to a heap object ID. *)
  | VArray of value list  (** Array of values. *)
  | VTuple of value list  (** Tuple of values. *)
  | VUnit  (** Unit value. *)
  | VModule of (string, value) Hashtbl.t  (** Module mapping names to values. *)
  | VNative of (value list -> value)  (** Native OCaml function. *)
  | VClosure of string  (** Named closure identifier. *)
  | VThunk of (unit -> value)  (** Lazy computation. *)
  | VRange of int  (** Range value. *)

(** Heap object variants. *)
type heap_obj +=
  | HString of string
  | HArray of float array
  | HBytes of char array
  | HClosure of string * Opcode.opcode list * (string * (value * bool)) list
  | HRange of { current : Bigint.t; step : Bigint.t; end_ : Bigint.t option }

type generation = {
  objs : (int, heap_obj) Hashtbl.t;  (** Objects in this generation. *)
  marked : (int, bool) Hashtbl.t;  (** Marked objects during GC. *)
}
(** A generation in the generational garbage collector. *)

val allocation_count : int ref
(** Total number of heap allocations so far. *)

val allocation_threshold : int ref
(** Threshold for triggering a GC collection. *)

val young_gen : generation
(** Young generation for the generational GC. *)

val old_gen : generation
(** Old generation for the generational GC. *)

val next_id : int ref
(** Next available heap ID. *)

val obj_size : 'a -> int
(** Compute the approximate size of a heap object. *)

val heap_memory : unit -> int * int
(** Return memory usage (bytes) for young and old generations. *)

val print_heap_memory : unit -> unit
(** Print memory usage for the heap. *)

val heap_size : unit -> int * int
(** Return the number of objects in young and old generations. *)

val print_heap_stats : unit -> unit
(** Print detailed heap statistics. *)

val alloc_id : unit -> int
(** Generate a new unique heap ID. *)

val alloc_in_young : heap_obj -> value
(** Allocate a heap object in the young generation and return a value
    referencing it. *)

val find_heap_obj : int -> heap_obj
(** Retrieve a heap object by its ID. *)

val get_string : int -> string option
(** Retrieve a string from the heap by ID, if it exists. *)

val get_bytes : int -> char array option
(** Retrieve a byte array from the heap by ID, if it exists. *)

val get_value : int -> value option
(** Retrieve a value from the heap by ID, if it exists. *)

val get_array : int -> float array option
(** Retrieve a float array from the heap by ID, if it exists. *)

val mark : int -> generation -> unit
(** Mark an object during GC. *)

val mark_value : value -> unit
(** Mark a value and all reachable heap objects during GC. *)

val mark_env : ('a * (value * 'b)) list -> unit
(** Mark all values in an environment during GC. *)

val sweep : generation -> unit
(** Sweep unmarked objects from a generation. *)

val mark_and_promote : value list -> (string * (value * bool)) list -> unit
(** Perform mark-and-promote for young generation objects reachable from roots.
*)

val maybe_collect_gc : value list -> (string * (value * bool)) list -> unit
(** Possibly trigger garbage collection based on allocation thresholds. *)

val get_stack_roots : value Stack.t -> value list
(** Collect roots from a value stack for GC. *)

val alloc_string_with_gc :
  value Stack.t -> (string * (value * bool)) list -> string -> value
(** Allocate a string with GC handling and return a value referencing it. *)

val alloc_bytes_with_gc :
  value Stack.t -> ('a * (value * 'b)) list -> char array -> value
(** Allocate a byte array with GC handling and return a value referencing it. *)

val alloc_array_with_gc :
  value Stack.t -> (string * (value * bool)) list -> float array -> value
(** Allocate a float array with GC handling and return a value referencing it.
*)

val alloc_range : Bigint.t -> Bigint.t -> Bigint.t option -> value
(** Allocate a range value [start, step, end] in the heap. *)

val reset_heap : unit -> unit
(** Reset the entire heap, clearing all generations. *)
