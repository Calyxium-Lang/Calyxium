open Calyxium_parser

val enum_variants : (string, string list) Hashtbl.t
(** A global table mapping enum names to their list of variant constructors. *)

val stdlib_used : bool ref
(** A flag indicating whether the standard library has been imported. *)

val built_in_modules : (string * (string * Ast.Type.t) list) list
(** A list of built-in modules, each represented as:
    - the module name
    - a list of (identifier, type) pairs representing exported values. *)

val builtins : (string * (int list * Ast.Type.t) list) list
(** A list of built-in functions.

    Each entry maps a function name to a list of overloads. Each overload is
    represented as:
    - a list of argument type arities
    - the return type. *)
