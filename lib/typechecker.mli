exception TypeError of string
exception UnifyError of string

val fresh_var_counter : int ref
val fresh_tyvar : unit -> Ast.Type.t
val occurs_check : Subst.t -> int -> Ast.Type.t -> bool
val enum_variants : (string, string list) Hashtbl.t
val stdlib_used : bool ref
val string_of_type : Ast.Type.t -> string
val bind_var : (int, Ast.Type.t) Hashtbl.t -> int -> Ast.Type.t -> unit

val unify_with_subst :
  (int, Ast.Type.t) Hashtbl.t -> Ast.Type.t -> Ast.Type.t -> unit

val unify : Ast.Type.t -> Ast.Type.t -> unit
val ftv_type : Ast.Type.t -> int list
val ftv_env : ('a * Ast.Type.t) list -> int list
val generalize : ('a * Ast.Type.t) list -> Ast.Type.t -> int list * Ast.Type.t
val instantiate_scheme : int list * Ast.Type.t -> Ast.Type.t
val can_compare : Ast.Type.t -> Ast.Type.t -> bool

val fold_left_map2 :
  ('a -> 'b -> 'c -> 'a * 'd) -> 'a -> 'b list -> 'c list -> 'a * 'd list

val built_in_modules : (string * (string * Ast.Type.t) list) list
val builtins : (string * (int list * Ast.Type.t) list) list
val type_eq : Ast.Type.t -> Ast.Type.t -> bool

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

val typecheck_program : Ast.Stmt.t list -> bool
