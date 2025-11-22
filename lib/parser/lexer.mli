exception LexerError of string * Lexing.position
(** Raised when the lexer encounters an invalid or malformed token.

    @param message A human-readable description of the error.
    @param position The lexing position where the error occurred. *)

val token : Lexing.lexbuf -> Parser.token
(** [token lexbuf] reads and returns the next token from the given lexing
    buffer.

    This is the main entry point for the lexer and is responsible for advancing
    the lexing cursor and producing a value of type [Parser.token].

    @param lexbuf The input lexing buffer.
    @return The next token.
    @raise LexerError If an invalid character or malformed token is encountered.
*)

val read_string : Buffer.t -> Lexing.lexbuf -> Parser.token
(** [read_string buf lexbuf] processes a string literal starting at the current
    lexing position. It reads characters until the closing quote and returns the
    corresponding string literal token.

    @param buf A buffer used to accumulate string contents.
    @param lexbuf The source lexing buffer.
    @return A [Parser.token] representing the parsed string literal.
    @raise LexerError If the string is unterminated or contains invalid escapes.
*)

val read_comment : Lexing.lexbuf -> Parser.token
(** [read_comment lexbuf] skips or processes a comment starting at the current
    lexing position. It handles nested or multiline comments depending on the
    lexer specification.

    @param lexbuf The source lexing buffer.
    @return The next token following the comment block.
    @raise LexerError If the comment is unterminated. *)
