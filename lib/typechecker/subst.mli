open Calyxium_parser

type t = (int, Ast.Type.t) Hashtbl.t
(** A substitution mapping type-variable IDs to concrete types. *)

val empty : unit -> ('a, 'b) Hashtbl.t
(** [empty ()] creates a fresh, empty hash table. This is a convenience wrapper
    for [Hashtbl.create]. *)

val find_opt : ('a, 'b) Hashtbl.t -> 'a -> 'b option
(** [find_opt tbl key] returns [Some value] if [key] exists in [tbl], otherwise
    returns [None]. This is a compatibility wrapper that provides optional
    lookup even for environments that do not use the standard library’s
    [Hashtbl.find_opt] directly. *)

val add : ('a, 'b) Hashtbl.t -> 'a -> 'b -> unit
(** [add tbl key value] inserts or replaces an entry in the hash table. *)

val apply : (int, Ast.Type.t) Hashtbl.t -> Ast.Type.t -> Ast.Type.t
(** [apply subst typ] applies a substitution to a type expression.

    This recursively walks the structure of [typ], replacing any type-variables
    that appear in [subst] with their mapped types.

    @param subst The substitution table from type-variable IDs to types.
    @param typ The type on which the substitution is applied.

    @return The resulting type after performing all substitutions. *)
