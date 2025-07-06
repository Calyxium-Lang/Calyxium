val stack : Gc.value Stack.t
val escape_sequences : (string * char) list
val replace_escape_sequences : string -> string

val binary_op :
  string -> (float -> float -> float) -> (int64 -> int64 -> int64) -> unit

val compare_op :
  string -> (float -> float -> bool) -> (int64 -> int64 -> bool) -> unit

val logic_op :
  string -> (float -> float -> bool) -> (int64 -> int64 -> bool) -> unit

val unary_logic_op : string -> (float -> bool) -> (int64 -> bool) -> unit
val unary_op : string -> (float -> float) -> (int64 -> int64) -> unit
val get_var : (string * (Gc.value * 'a)) list -> string -> Gc.value
val get_string_from_stack_value : Gc.value -> string
val resolve_function_body : string -> Opcode.opcode list
val extract_param_names : Opcode.opcode list -> string list

val execute :
  Opcode.opcode array -> (string * (Gc.value * bool)) list -> int -> Gc.value

val run : Opcode.opcode list -> Gc.value
