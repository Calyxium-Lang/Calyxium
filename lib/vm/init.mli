(** Standard library definitions and helper functions for the runtime. *)

type stdlib_module = string * (string * Gc.value) list
(** A standard library module consisting of a name and a list of bindings. *)

val define_module : string -> (string * Gc.value) list -> stdlib_module
(** [define_module name bindings] creates a standard library module with the
    given [name] and list of [bindings]. *)

val native1 : (Gc.value -> Gc.value) -> Gc.value
(** [native1 f] wraps a unary OCaml function [f] as a runtime value of type
    [Gc.value]. *)

val native2 : (Gc.value -> Gc.value -> Gc.value) -> Gc.value
(** [native2 f] wraps a binary OCaml function [f] as a runtime value of type
    [Gc.value]. *)

val stdlib_definitions : stdlib_module list
(** List of all standard library module definitions. *)

val stdlib_modules : (string, (string, Gc.value) Hashtbl.t) Hashtbl.t
(** Hash table mapping module names to their runtime representations. *)
