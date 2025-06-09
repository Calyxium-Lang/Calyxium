{
  open Parser

  let line = ref 1
  let column = ref 0

  let update_column () = incr column

  let update_line () =
    incr line;
    column := 0

  let advance_by_lexeme lexbuf =
    let lexeme = Lexing.lexeme lexbuf in
    column := !column + String.length lexeme;
    lexeme

  let advance_and_return token lexbuf =
    column := !column + (Lexing.lexeme_end lexbuf - Lexing.lexeme_start lexbuf);
    token

  let advance_fixed_width token width =
    column := !column + width;
    token
}

let whitespace = [' ' '\t']
let newline = '\n'
let identifier = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let digits = ['0'-'9']+
let floats = digits '.' digits+
let char_literal = '\'' [^'\''] '\''
let string_literal = '"' [^'"']* '"'

rule token = parse
  | whitespace               { update_column (); token lexbuf }
  | newline                  { update_line (); token lexbuf }
  | "#"                      { read_comment lexbuf }

  | "=="                     { advance_fixed_width Eq 2 }
  | "!="                     { advance_fixed_width Neq 2 }
  | ">="                     { advance_fixed_width Geq 2 }
  | "<="                     { advance_fixed_width Leq 2 }
  | "||"                     { advance_fixed_width LogicalOr 2 }
  | "&&"                     { advance_fixed_width LogicalAnd 2 }
  | "**"                     { advance_fixed_width Pow 2 }
  | "--"                     { advance_fixed_width Dec 2 }
  | "++"                     { advance_fixed_width Inc 2 }
  | "->"                     { advance_fixed_width MapsTo 2 }
  | "+="                     { advance_fixed_width PlusAssign 2 }
  | "-="                     { advance_fixed_width MinusAssign 2 }
  | "*="                     { advance_fixed_width StarAssign 2 }
  | "/="                     { advance_fixed_width SlashAssign 2 }

  | "+"                      { advance_and_return Plus lexbuf }
  | "-"                      { advance_and_return Minus lexbuf }
  | "*"                      { advance_and_return Star lexbuf }
  | "/"                      { advance_and_return Slash lexbuf }
  | "%"                      { advance_and_return Mod lexbuf }
  | "^"                      { advance_and_return Carot lexbuf }
  | "="                      { advance_and_return Assign lexbuf }
  | ">"                      { advance_and_return Greater lexbuf }
  | "<"                      { advance_and_return Less lexbuf }
  | "("                      { advance_and_return LParen lexbuf }
  | ")"                      { advance_and_return RParen lexbuf }
  | "["                      { advance_and_return LBracket lexbuf }
  | "]"                      { advance_and_return RBracket lexbuf }
  | "{"                      { advance_and_return LBrace lexbuf }
  | "}"                      { advance_and_return RBrace lexbuf }
  | "."                      { advance_and_return Dot lexbuf }
  | ":"                      { advance_and_return Colon lexbuf }
  | ";"                      { advance_and_return Semi lexbuf }
  | ","                      { advance_and_return Comma lexbuf }
  | "!"                      { advance_and_return Not lexbuf }
  | "|"                      { advance_and_return Pipe lexbuf }
  | "_"                      { advance_and_return UnderScore lexbuf }

  | "fun"                    { ignore (advance_by_lexeme lexbuf); Function }
  | "rec"                    { ignore (advance_by_lexeme lexbuf); Recursive }
  | "if"                     { ignore (advance_by_lexeme lexbuf); If }
  | "then"                   { ignore (advance_by_lexeme lexbuf); Then }
  | "else"                   { ignore (advance_by_lexeme lexbuf); Else }
  | "let"                    { ignore (advance_by_lexeme lexbuf); Let }
  | "match"                  { ignore (advance_by_lexeme lexbuf); Match }
  | "with"                   { ignore (advance_by_lexeme lexbuf); With }
  | "return"                 { ignore (advance_by_lexeme lexbuf); Return }
  | "for"                    { ignore (advance_by_lexeme lexbuf); For }
  | "use"                    { ignore (advance_by_lexeme lexbuf); Use }
  | "mod"                    { ignore (advance_by_lexeme lexbuf); Module }
  | "true"                   { ignore (advance_by_lexeme lexbuf); True }
  | "false"                  { ignore (advance_by_lexeme lexbuf); False }

  | "int"                    { ignore (advance_by_lexeme lexbuf); IntType }
  | "float"                  { ignore (advance_by_lexeme lexbuf); FloatType }
  | "string"                 { ignore (advance_by_lexeme lexbuf); StringType }
  | "byte"                   { ignore (advance_by_lexeme lexbuf); ByteType }
  | "bool"                   { ignore (advance_by_lexeme lexbuf); BoolType }
  | "unit"                   { ignore (advance_by_lexeme lexbuf); UnitType }

  | floats as f              { column := !column + String.length f; Float (float_of_string f) }
  | digits as d              { column := !column + String.length d; Int (Int64.of_string d) }
  | identifier as id         { column := !column + String.length id; Ident id }
  | char_literal as c        { column := !column + String.length c; Byte (c.[1]) }
  | string_literal as s      { column := !column + String.length s; String (String.sub s 1 (String.length s - 2)) }
  | eof                      { EOF }

and read_comment = parse
  | newline                  { update_line (); token lexbuf }
  | _                        { read_comment lexbuf }
  | eof                      { EOF }
