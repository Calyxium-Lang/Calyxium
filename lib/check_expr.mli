val check_stmt :
  (string * Ast.Type.t) list ->
  (string * (int list * Ast.Type.t) list) list ->
  Ast.Stmt.t ->
  (string * Ast.Type.t) list * (string * (int list * Ast.Type.t) list) list

val check_expr :
  (string * Ast.Type.t) list ->
  (string * (int list * Ast.Type.t) list) list ->
  Ast.Expr.t ->
  Ast.Type.t * (string * Ast.Type.t) list

val find_return_exprs :
  (string * Ast.Type.t) list ->
  (string * (int list * Ast.Type.t) list) list ->
  Ast.Expr.t ->
  (Ast.Type.t * 'a) list

val collect_functions :
  Ast.Stmt.t list -> (string * (int list * Ast.Type.t) list) list
