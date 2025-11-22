open Calyxium_parser

exception RuntimeError of string
(** Exception raised on runtime errors. *)

type trace_entry = { instr : string }
(** Trace entry for tracking executed instructions. *)

type frame = {
  mutable code : Opcode.opcode array;
  mutable pc : int;
  mutable env : (string * (Gc.value * bool)) list;
}
(** Call frame containing the current code, program counter, and environment. *)

val output_buffer : Buffer.t
(** Output buffer for runtime print statements. *)

val stdlib_modules : (string, Gc.value) Hashtbl.t
(** Hash table of loaded standard library modules. *)

val trace : trace_entry Queue.t
(** Queue of trace entries for debugging purposes. *)

val push_trace : string -> unit
(** Push a new entry into the trace queue. *)

val clear_trace : unit -> unit
(** Clear all trace entries. *)

val flush_buffer : unit -> unit
(** Flush the output buffer to the standard output. *)

val safe_push_output : string -> unit
(** Safely push a string to the output buffer. *)

val init_stdlib : unit -> unit
(** Initialize standard library modules. *)

val print_trace : string -> unit
(** Print the current trace to the given output. *)

val runtime_error : string -> 'a
(** Raise a runtime error with a message. *)

val stack : Gc.value Stack.t
(** Stack used by the VM to hold runtime values. *)

val global_env : (string * (Gc.value * bool)) list ref
(** Global environment mapping variable names to values. *)

val escape_sequences : (string * char) list
(** List of escape sequences and their character mappings. *)

val replace_escape_sequences : string -> string
(** Replace escape sequences in a string with their corresponding characters. *)

val pop_n : Gc.value list -> int -> Gc.value list
(** Pop [n] values from a list, returning the remaining list. *)

val pop_n_rev : Gc.value list -> int -> Gc.value list
(** Pop [n] values from a list in reverse order. *)

val pop2_safe : 'a Stack.t -> 'a * 'a
(** Safely pop two values from a stack. *)

val pop1 : 'a Stack.t -> 'a
(** Pop one value from a stack. *)

val get_var : (string * (Gc.value * 'a)) list -> string -> Gc.value
(** Retrieve a variable value from an environment by name. *)

val get_string_from_stack_value : Gc.value -> string
(** Extract a string from a stack value, converting if necessary. *)

val resolve_function_body : string -> Bytecode.function_info
(** Resolve a function body by its name, returning bytecode info. *)

val extract_param_names : Opcode.opcode list -> string list
(** Extract parameter names from a bytecode instruction list. *)

val update_variable :
  string ->
  Gc.value ->
  (string * (Gc.value * 'a)) list ->
  (string * (Gc.value * 'a)) list
(** Update a variable in an environment. *)

val equal_value : Gc.value -> Gc.value -> bool
(** Compare two runtime values for equality. *)

val not_equal_value : Gc.value -> Gc.value -> bool
(** Compare two runtime values for inequality. *)

val string_to_bytes : string -> char array
(** Convert a string to a byte array. *)

val safe_shift_left : Bigint.t -> Bigint.t -> Bigint.t
(** Safely shift a big integer left by another big integer. *)

val force : Gc.value -> Gc.value
(** Force evaluation of a lazy/thunk value. *)

val reset_vm_state : unit -> unit
(** Reset the entire VM state, including stack, globals, and heap. *)

val run : Opcode.opcode list -> unit
(** Execute a list of bytecode instructions. *)
