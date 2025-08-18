type program_image = {
  entry : Opcode.opcode list;
  functions : (string * Bytecode.function_info) list;
}

val save_bytecode_to_file : string -> Opcode.opcode list -> unit
val load_bytecode_from_file : string -> Opcode.opcode list
