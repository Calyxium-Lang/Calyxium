val indent_level : int ref
val indent : unit -> string
val token_to_string : Token.t -> string
val string_of_type : Ast.Type.t -> string
val string_of_parameter : Ast.Stmt.parameter -> string
val string_of_expr : ?top_level:bool -> Ast.Expr.t -> string
val string_of_stmt : Ast.Stmt.t -> string
val string_of_program : Ast.Stmt.t list -> string
