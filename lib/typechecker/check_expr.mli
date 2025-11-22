open Calyxium_parser
open Calyxium_ir

(** The set of type errors that can occur during type checking. *)
type type_error =
  | Mismatch of Ast.Type.t * Ast.Type.t * string
      (** A type mismatch: expected type, actual type, and a descriptive
          message. *)
  | UnknownVariable of string
      (** The referenced variable is not defined in the current scope. *)
  | UnknownFunction of string
      (** The referenced function is not defined or not in scope. *)
  | ArityMismatch of string * int * int
      (** A function was called with an incorrect number of arguments. Contains
          the function name, expected arity, and actual arity. *)
  | UnsupportedStmt of string
      (** A statement was encountered that this type checker cannot process. *)
  | GenericError of string  (** A generic catch-all error with a message. *)

val check_stmt :
  (string * Ast.Type.t) list ->
  (string * (int list * Ast.Type.t) list) list ->
  Ir.instr ->
  ( (string * Ast.Type.t) list * (string * (int list * Ast.Type.t) list) list,
    type_error )
  result
(** [check_stmt vars funcs instr] type-checks a single IR instruction.

    @param vars The current variable environment mapping names to types.
    @param funcs
      The current function environment mapping function names to overloads
      (argument arities and return types).
    @param instr The IR instruction to check.

    @return
      Either:
      - [Ok (vars', funcs')] with updated environments, or
      - [Error e] if a type error occurred. *)

val check_expr :
  (string * Ast.Type.t) list ->
  (string * (int list * Ast.Type.t) list) list ->
  Ir.expr ->
  (Ast.Type.t * (string * Ast.Type.t) list, type_error) result
(** [check_expr vars funcs expr] type-checks an expression.

    @param vars The variable environment.
    @param funcs The function environment.
    @param expr The expression to check.

    @return
      Either:
      - [Ok (ty, vars')] giving the inferred type and an updated variable
        environment, or
      - [Error e] if a type error occurred. *)

val find_return_exprs :
  (string * Ast.Type.t) list ->
  (string * (int list * Ast.Type.t) list) list ->
  Ir.expr ->
  Ir.expr list
(** [find_return_exprs vars funcs expr] collects all return expressions
    reachable within a function body. This is used to ensure all branches of a
    function produce a consistent return value.

    @param vars Variable environment.
    @param funcs Function environment.
    @param expr The expression to analyze.

    @return A list of returned expressions found in the body. *)

val collect_functions :
  Ir.instr list -> (string * (int list * Ast.Type.t) list) list
(** [collect_functions instrs] scans IR instructions for function declarations
    and builds an environment of function signatures.

    @param instrs The IR instruction list.
    @return A function environment mapping names to possible overloads. *)

val show_type : Ast.Type.t -> string
(** Pretty-print an [Ast.Type.t] to a string. *)

val string_of_type_error : type_error -> string
(** Convert a [type_error] to a human-readable string message. *)
