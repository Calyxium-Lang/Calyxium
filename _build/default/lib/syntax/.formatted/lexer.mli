val get_line : unit -> int
(** Returns the current line number. *)

val get_column : unit -> int
(** Returns the current column number. *)

val update_column : unit -> unit
(** Updates the column counter by incrementing it. *)

val update_line : unit -> unit
(** Updates the line counter by incrementing it and resets the column counter. *)

val token_and_update_column : 'a -> Lexing.lexbuf -> 'a
(** Updates the column counter based on the length of the token and returns the token.
    @param t The token to return.
    @param lexbuf The lexer buffer to extract the token length from.
    @return The token after updating the column counter. *)

val token : Lexing.lexbuf -> Parser.token
(** The main lexer rule for tokenizing input.
    @param lexbuf The lexer buffer used to tokenize the input.
    @return The token extracted from the input, compatible with the parser. *)

val read_comment : Lexing.lexbuf -> Parser.token
(** Parses and ignores comments, advancing line and column counters as necessary.
    @param lexbuf The lexer buffer used to read comments.
    @return The end of file token. *)
