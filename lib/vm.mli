val stack : float Stack.t
val string_table : (int, string) Hashtbl.t
val add_string : string -> int
val escape_sequences : (string * char) list
val replace_escape_sequences : string -> string
val binary_op : string -> (float -> float -> float) -> unit

val execute :
  Opcode.opcode array -> (string * (float * bool)) list -> int -> float

val run : Opcode.opcode list -> float
