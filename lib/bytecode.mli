val function_table : (string, Opcode.opcode list) Hashtbl.t
val compile_expr : Ast.Expr.t -> Opcode.opcode list
val compile_stmt : Ast.Stmt.t -> Opcode.opcode list
