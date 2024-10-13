val eval_input : string -> unit
(** Evaluates a single input string in the REPL, parsing it into an AST, type checking, 
    compiling it into bytecode, and executing the bytecode.
    @param input The input string to evaluate.
    @raise Failure if parsing, type checking, or execution fails.
*)

val get_version : unit -> string
(** Returns the current version of the REPL.
        @return A string representing the version number.
    *)

val print_repl_info : unit -> unit
(** Prints information about the REPL, including the version and platform. *)

val repl : unit -> unit
(** The main REPL loop. It repeatedly reads user input, evaluates it, and prints the result.
        To exit the loop, the user must input the command ["exit"].
    *)
