exception LexerError of string * Lexing.position

val token : Lexing.lexbuf -> Parser.token
val read_string : Buffer.t -> Lexing.lexbuf -> Parser.token
val read_comment : Lexing.lexbuf -> Parser.token
