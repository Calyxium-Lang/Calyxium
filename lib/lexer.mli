val get_line : unit -> int
val get_column : unit -> int
val update_column : unit -> unit
val update_line : unit -> unit
val token_and_update_column : 'a -> Lexing.lexbuf -> 'a
val token : Lexing.lexbuf -> Parser.token
val read_comment : Lexing.lexbuf -> Parser.token
