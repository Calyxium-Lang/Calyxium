type function_info = {
  return_type : Ast.Type.t;
  params : Ast.Stmt.parameter list;
  bytecode : Opcode.opcode list;
  body_code : Opcode.opcode array;
}

val function_table : (string, function_info) Hashtbl.t
val enum_tbl : (string, (string * int) list) Hashtbl.t
val struct_tbl : (string, (string * Ir.expr) list) Hashtbl.t
val builtins : (string * (Opcode.opcode list -> Opcode.opcode list)) list
val gensym : string -> string
val opcode_of_binop : Token.t -> Opcode.opcode
val ir_compile_stmt : Ir.instr -> Opcode.opcode list
val ir_compile_expr : Ir.expr -> Opcode.opcode list
(* val compile_stmt : Ast.Stmt.t -> Opcode.opcode list
val compile_expr : Ast.Expr.t -> Opcode.opcode list *)
