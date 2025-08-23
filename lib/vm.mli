exception RuntimeError of string

type trace_entry = { instr : string }

type frame = {
  mutable code : Opcode.opcode array;
  mutable pc : int;
  mutable env : (string * (Gc.value * bool)) list;
}

val output_buffer : Buffer.t
val stdlib_modules : (string, Gc.value) Hashtbl.t
val trace : trace_entry Queue.t
val push_trace : string -> unit
val clear_trace : unit -> unit
val flush_buffer : unit -> unit
val safe_push_output : string -> unit
val init_stdlib : unit -> unit
val print_trace : string -> unit
val runtime_error : string -> 'a
val stack : Gc.value Stack.t
val global_env : (string * (Gc.value * bool)) list ref
val escape_sequences : (string * char) list
val replace_escape_sequences : string -> string
val pop_n : Gc.value list -> int -> Gc.value list
val pop_n_rev : Gc.value list -> int -> Gc.value list
val pop2_safe : 'a Stack.t -> 'a * 'a
val pop1 : 'a Stack.t -> 'a
val get_var : (string * (Gc.value * 'a)) list -> string -> Gc.value
val get_string_from_stack_value : Gc.value -> string
val resolve_function_body : string -> Bytecode.function_info
val extract_param_names : Opcode.opcode list -> string list

val update_variable :
  string ->
  Gc.value ->
  (string * (Gc.value * 'a)) list ->
  (string * (Gc.value * 'a)) list

val equal_value : Gc.value -> Gc.value -> bool
val not_equal_value : Gc.value -> Gc.value -> bool
val string_to_bytes : string -> char array
val safe_shift_left : Bigint.t -> Bigint.t -> Bigint.t
val force : Gc.value -> Gc.value
val reset_vm_state : unit -> unit
val run : Opcode.opcode list -> unit
