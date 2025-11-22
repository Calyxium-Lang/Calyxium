(** Bytecode program representation and file I/O utilities. *)

type program_image = {
  entry : Opcode.opcode list;
      (** The main program entry point as a list of opcodes. *)
  functions : (string * Bytecode.function_info) list;
      (** List of named functions with their compiled info. *)
}
(** A compiled program image containing the entry point and all functions. *)

val save_bytecode_to_file : string -> Opcode.opcode list -> unit
(** [save_bytecode_to_file filename bytecode] writes the given list of opcodes
    [bytecode] to the file at [filename]. *)

val load_bytecode_from_file : string -> Opcode.opcode list
(** [load_bytecode_from_file filename] reads a list of opcodes from the file at
    [filename] and returns it. *)
