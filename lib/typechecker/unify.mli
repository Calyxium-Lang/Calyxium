open Calyxium_parser

val bind_var : (int, Ast.Type.t) Hashtbl.t -> int -> Ast.Type.t -> unit
(** [bind_var subst tv ty] binds type variable [tv] to type [ty] in the
    substitution [subst]. *)

val unify_with_subst :
  (int, Ast.Type.t) Hashtbl.t -> Ast.Type.t -> Ast.Type.t -> unit
(** [unify_with_subst subst t1 t2] attempts to unify types [t1] and [t2] under
    the given substitution [subst]. Raises [UnifyError] on failure. *)

val unify : Ast.Type.t -> Ast.Type.t -> unit
(** [unify t1 t2] unifies two types without providing an explicit substitution.
    Raises [UnifyError] if the types cannot be unified. *)

val ftv_type : Ast.Type.t -> int list
(** [ftv_type ty] returns the list of free type variable IDs appearing in [ty].
*)

val ftv_env : ('a * Ast.Type.t) list -> int list
(** [ftv_env env] returns the list of free type variable IDs appearing in the
    type environment [env]. *)

val generalize : ('a * Ast.Type.t) list -> Ast.Type.t -> int list * Ast.Type.t
(** [generalize env ty] produces a polymorphic type scheme by generalizing over
    all type variables in [ty] that do not appear in [env].

    @return
      A pair [(vars, ty')] where [vars] is the list of generalized type-variable
      IDs, and [ty'] is the type body. *)

val instantiate_scheme : int list * Ast.Type.t -> Ast.Type.t
(** [instantiate_scheme (vars, ty)] replaces all type variables [vars] in the
    type [ty] with fresh type variables, producing a concrete type instance. *)

val can_compare : Ast.Type.t -> Ast.Type.t -> bool
(** [can_compare t1 t2] returns [true] if values of type [t1] can be compared
    with values of type [t2]. *)

val fold_left_map2 :
  ('a -> 'b -> 'c -> 'a * 'd) -> 'a -> 'b list -> 'c list -> 'a * 'd list
(** [fold_left_map2 f acc l1 l2] is like a combination of [List.fold_left] and
    [List.map2]: it threads an accumulator [acc] through [l1] and [l2] while
    producing a new list of results.

    @param f
      A function that takes the accumulator, an element of [l1], and an element
      of [l2], and returns a pair of updated accumulator and output value.
    @param acc Initial accumulator.
    @param l1 First input list.
    @param l2 Second input list.
    @return A pair of the final accumulator and the list of mapped results. *)
