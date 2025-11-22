(** Utility functions for error reporting and source extraction. *)

val get_line : string -> int -> string option
(** [get_line filename n] returns line number [n] from file [filename]. If the
    file contains fewer than [n] lines, returns [None].

    @param filename The path to the file to read.
    @param n The 1-indexed line number to retrieve.
    @return [Some line] if the line exists, otherwise [None]. *)

val print_error :
  file:string ->
  line:int ->
  col:int ->
  msg:string ->
  source:string option ->
  unit
(** [print_error ~file ~line ~col ~msg ~source] prints a formatted
    compiler-style error message with optional source code context.

    @param file The filename where the error occurred.
    @param line The 1-indexed line number of the error.
    @param col The 1-indexed column number of the error.
    @param msg The human-readable error message to display.
    @param source
      An optional line of source code associated with the error. If provided, a
      caret ([^]) is shown at column [col]. *)
