val stack : Gc.value Stack.t
val init_stdlib : unit -> unit
val escape_sequences : (string * char) list
val replace_escape_sequences : string -> string
val get_var : (string * (Gc.value * 'a)) list -> string -> Gc.value
val get_string_from_stack_value : Gc.value -> string
val resolve_function_body : string -> Opcode.opcode list
val extract_param_names : Opcode.opcode list -> string list
val reset_vm_state : unit -> unit
val run : Opcode.opcode list -> unit
