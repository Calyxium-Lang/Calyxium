exception TypeError of string

val type_eq : Ast.Type.t -> Ast.Type.t -> bool

val check_expr :
  (string * Ast.Type.t) list ->
  (string * (Ast.Type.t list * Ast.Type.t)) list ->
  Ast.Expr.t ->
  Ast.Type.t

val find_return_exprs :
  (string * Ast.Type.t) list ->
  (string * (Ast.Type.t list * Ast.Type.t)) list ->
  Ast.Expr.t ->
  Ast.Type.t list

val check_stmt :
  (string * Ast.Type.t) list ->
  (string * (Ast.Type.t list * Ast.Type.t)) list ->
  Ast.Stmt.t ->
  (string * Ast.Type.t) list

val typecheck_program : Ast.Stmt.t list -> unit
