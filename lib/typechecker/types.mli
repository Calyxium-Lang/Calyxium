open Calyxium_parser

exception TypeError of string
(** Raised when a type error is encountered during inference or checking. *)

exception UnifyError of string
(** Raised when two types cannot be unified. *)

val fresh_var_counter : int ref
(** A counter used for generating fresh type variables. *)

val fresh_tyvar : unit -> Ast.Type.t
(** Create a fresh type variable, typically used during type inference. *)

val occurs_check : Subst.t -> int -> Ast.Type.t -> bool
(** [occurs_check subst tv ty] returns [true] if type variable [tv] occurs
    somewhere within type [ty] after applying the substitution [subst].

    This prevents constructing infinite types during unification. *)

val string_of_type : Ast.Type.t -> string
(** Convert a type to a human-readable string representation. *)

val type_eq : Ast.Type.t -> Ast.Type.t -> bool
(** Structural equality on types.

    Returns [true] if the two types are equal after comparing their constructors
    and subcomponents. *)
