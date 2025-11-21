type type_error =
  | Mismatch of Ast.Type.t * Ast.Type.t * string
  | UnknownVariable of string
  | UnknownFunction of string
  | ArityMismatch of string * int * int
  | UnsupportedStmt of string
  | GenericError of string

val check_stmt :
  (string * Ast.Type.t) list ->
  (string * (int list * Ast.Type.t) list) list ->
  Ir.instr ->
  ( (string * Ast.Type.t) list * (string * (int list * Ast.Type.t) list) list,
    type_error )
  result

val check_expr :
  (string * Ast.Type.t) list ->
  (string * (int list * Ast.Type.t) list) list ->
  Ir.expr ->
  (Ast.Type.t * (string * Ast.Type.t) list, type_error) result

val find_return_exprs :
  (string * Ast.Type.t) list ->
  (string * (int list * Ast.Type.t) list) list ->
  Ir.expr ->
  Ir.expr list

val collect_functions :
  Ir.instr list -> (string * (int list * Ast.Type.t) list) list

val show_type : Ast.Type.t -> string
val string_of_type_error : type_error -> string
