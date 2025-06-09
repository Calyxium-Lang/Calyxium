val line : int ref
val column : int ref
val update_column : unit -> unit
val update_line : unit -> unit
val advance_by_lexeme : Lexing.lexbuf -> string
val advance_and_return : 'a -> Lexing.lexbuf -> 'a
val advance_fixed_width : 'a -> int -> 'a
val token : Lexing.lexbuf -> Parser.token
