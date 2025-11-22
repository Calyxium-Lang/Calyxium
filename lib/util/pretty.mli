open Calyxium_parser

val indent_level : int ref
(** Current indentation level used for pretty-printing. *)

val indent : unit -> string
(** [indent] returns a string containing spaces corresponding to the current
    indentation level. *)

val token_to_string : Token.t -> string
(** [token_to_string token] converts a [Token.t] to its string representation.
*)

val string_of_type : Ast.Type.t -> string
(** [string_of_type ty] converts an [Ast.Type.t] to a human-readable string. *)

val string_of_parameter : Ast.Stmt.parameter -> string
(** [string_of_parameter param] converts a function parameter to a string,
    including its name and type. *)

val string_of_expr : ?top_level:bool -> Ast.Expr.t -> string
(** [string_of_expr ?top_level expr] converts an expression [expr] to a string.
    If [top_level] is [true], the expression may be formatted slightly
    differently. *)

val string_of_stmt : Ast.Stmt.t -> string
(** [string_of_stmt stmt] converts a statement [stmt] to a string. *)

val string_of_program : Ast.Stmt.t list -> string
(** [string_of_program stmts] converts a list of statements [stmts] into a
    nicely formatted string representing the entire program. *)
