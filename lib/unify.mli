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
