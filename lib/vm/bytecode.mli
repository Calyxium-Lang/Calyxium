open Calyxium_parser

type function_info = {
  return_type : Ast.Type.t;  (** The function's return type. *)
  params : Ast.Stmt.parameter list;  (** List of parameters with types. *)
  bytecode : Opcode.opcode list;  (** Sequence of opcodes for the function. *)
  body_code : Opcode.opcode array;
      (** Array representation of the function body. *)
}
(** Information about a compiled function. *)

val function_table : (string, function_info) Hashtbl.t
(** Table mapping function names to their compiled info. *)

val enum_tbl : (string, (string * int) list) Hashtbl.t
(** Table mapping enum names to a list of (variant name, integer value). *)

val struct_tbl : (string, (string * Calyxium_ir.Ir.expr) list) Hashtbl.t
(** Table mapping struct names to a list of (field name, default value) pairs.
*)

val builtins : (string * (Opcode.opcode list -> Opcode.opcode list)) list
(** Built-in functions and their compiler implementations. *)

val gensym : string -> string
(** [gensym prefix] generates a unique symbol string using the given [prefix].
*)

val opcode_of_binop : Token.t -> Opcode.opcode
(** [opcode_of_binop tok] returns the opcode corresponding to a binary operator
    token. *)

val ir_compile_stmt : Calyxium_ir.Ir.instr -> Opcode.opcode list
(** [ir_compile_stmt instr] compiles an IR instruction to a list of opcodes. *)

val ir_compile_expr : Calyxium_ir.Ir.expr -> Opcode.opcode list
(** [ir_compile_expr expr] compiles an IR expression to a list of opcodes. *)
