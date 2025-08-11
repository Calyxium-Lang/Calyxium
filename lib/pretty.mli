open Ast

val string_of_expr : ?top_level:bool -> Expr.t -> string
val string_of_stmt : Stmt.t -> string
val string_of_program : Stmt.t list -> string
