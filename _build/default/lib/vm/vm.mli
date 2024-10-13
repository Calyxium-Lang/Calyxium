val stack : float Stack.t
val string_table : (int, string) Hashtbl.t
val gc_threshold : int ref
val big_int_pow : Z.t -> Z.t -> Z.t
val safe_pow : float -> float -> float
val inf_check : float -> float
val log_memory_usage : string -> unit
val add_string : string -> int
val escape_sequences : (string * char) list
val replace_escape_sequences : string -> string
val update_gc_threshold : Gc.stat -> unit
val trigger_gc : int -> unit

val execute_bytecode :
  Bytecode.opcode array -> (string * (float * bool)) list -> int -> float

val run : Bytecode.opcode list -> float
