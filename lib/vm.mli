val stack : float Stack.t
val string_table : (int, string) Hashtbl.t
val add_string : string -> int
val escape_sequences : (string * char) list
val replace_escape_sequences : string -> string
val binary_op : string -> (float -> float -> float) -> unit
val compare_op : string -> (float -> float -> bool) -> unit
val logic_op : string -> (float -> float -> bool) -> unit
val unary_logic_op : string -> (float -> bool) -> unit
val unary_op : string -> (float -> float) -> unit
val get_var : (string * ('a * 'b)) list -> string -> 'a
val get_string_from_stack_value : float -> string
val resolve_function_body : string -> Opcode.opcode list
val extract_param_names : Opcode.opcode list -> string list

val execute :
  Opcode.opcode array -> (string * (float * bool)) list -> int -> float

val run : Opcode.opcode list -> float
