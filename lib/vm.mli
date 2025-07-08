val stack : Gc.value Stack.t
val bool_to_float : bool -> float
val bool_to_int64 : bool -> int64
val escape_sequences : (string * char) list
val replace_escape_sequences : string -> string
val get_var : (string * (Gc.value * 'a)) list -> string -> Gc.value
val get_string_from_stack_value : Gc.value -> string
val resolve_function_body : string -> Opcode.opcode list
val extract_param_names : Opcode.opcode list -> string list

val execute :
  Opcode.opcode array -> (string * (Gc.value * bool)) list -> int -> Gc.value

val run : Opcode.opcode list -> Gc.value
