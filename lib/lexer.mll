{
  open Parser
  exception LexerError of string
}

let whitespace = [' ' '\t']
let newline = '\n'
let identifier = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let digits = ['0'-'9']+
let floats = digits '.' digits+

rule token = parse
  | whitespace               { token lexbuf }
  | newline                  { Lexing.new_line lexbuf; token lexbuf }
  | "#"                      { read_comment lexbuf }

  | "=="                     { Eq }
  | "!="                     { Neq }
  | ">="                     { Geq }
  | "<="                     { Leq }
  | "||"                     { LogicalOr }
  | "&&"                     { LogicalAnd }
  | "**"                     { Pow }
  | "--"                     { Dec }
  | "++"                     { Inc }
  | "->"                     { MapsTo }
  | "+="                     { PlusAssign }
  | "-="                     { MinusAssign }
  | "*="                     { StarAssign }
  | "/="                     { SlashAssign }

  | "+"                      { Plus }
  | "-"                      { Minus }
  | "*"                      { Star }
  | "/"                      { Slash }
  | "%"                      { Mod }
  | "^"                      { Carot }
  | "="                      { Assign }
  | ">"                      { Greater }
  | "<"                      { Less }
  | "("                      { LParen }
  | ")"                      { RParen }
  | "["                      { LBracket }
  | "]"                      { RBracket }
  | "{"                      { LBrace }
  | "}"                      { RBrace }
  | "."                      { Dot }
  | ":"                      { Colon }
  | ";"                      { Semi }
  | ","                      { Comma }
  | "!"                      { Not }
  | "|"                      { Pipe }
  | "_"                      { UnderScore }
  | "?"                      { Question }

  | "rec"                    { Recursive }
  | "if"                     { If }
  | "then"                   { Then }
  | "else"                   { Else }
  | "let"                    { Let }
  | "match"                  { Match }
  | "with"                   { With }
  | "return"                 { Return }
  | "for"                    { For }
  | "use"                    { Use }
  | "mod"                    { Module }
  | "true"                   { True }
  | "false"                  { False }

  | "int"                    { IntType }
  | "float"                  { FloatType }
  | "string"                 { StringType }
  | "byte"                   { ByteType }
  | "bool"                   { BoolType }
  | "unit"                   { UnitType }

  | floats as f              { Float (float_of_string f) }
  | digits as d              { Int (Int64.of_string d) }
  | identifier as id         { Ident id }

  | '\'' '\\' (['n' 't' '\\' '\'' '\"'] as esc) '\'' { let c = match esc with | 'n'  -> '\n' | 't'  -> '\t' | '\\' -> '\\' | '\'' -> '\'' | '\"' -> '\"' | _    -> esc in Byte c }
  | '\'' ([^'\n' '\\'] as c) '\'' { Byte c }
  | '\''                        { raise (LexerError "Unterminated character literal") }
  | '"'                         { read_string (Buffer.create 16) lexbuf }
  | eof                         { EOF }
  | _                           { let c = Lexing.lexeme_char lexbuf 0 in let msg = Printf.sprintf "Unrecognized character: '%c'" c in raise (LexerError msg) }

and read_string buf = parse
  | '"'                         { String (Buffer.contents buf) }
  | '\n'                        { raise (LexerError "Unterminated string literal") }
  | eof                         { raise (LexerError "Unterminated string literal") }
  | '\\' (['\\' '"' 'n' 't'] as esc) { Buffer.add_char buf (match esc with | '\\' -> '\\' | '"'  -> '"' | 'n'  -> '\n' | 't'  -> '\t' | _    -> esc); read_string buf lexbuf }
  | _ as c                      { Buffer.add_char buf c; read_string buf lexbuf }

and read_comment = parse
  | newline                     { Lexing.new_line lexbuf; token lexbuf }
  | _                           { read_comment lexbuf }
  | eof                         { EOF }
