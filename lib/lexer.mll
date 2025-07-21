{
  open Parser
  exception LexerError of string
  let at_line_start = ref true
}

let whitespace = [' ' '\t']
let newline = '\n'
let identifier = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let digits = ['0'-'9']+
let floats = digits '.' digits+

rule token = parse
  | whitespace               { token lexbuf }
  | newline                  { at_line_start := true; Lexing.new_line lexbuf; token lexbuf }
  | "--"                     { if !at_line_start then read_comment lexbuf else (at_line_start := false; Dec) }
  | "--"                     { read_comment lexbuf }

  | "=="                     { at_line_start := false; Eq }
  | "!="                     { at_line_start := false; Neq }
  | ">="                     { at_line_start := false; Geq }
  | "<="                     { at_line_start := false; Leq }
  | "||"                     { at_line_start := false; LogicalOr }
  | "&&"                     { at_line_start := false; LogicalAnd }
  | "**"                     { at_line_start := false; Pow }
  | "++"                     { at_line_start := false; Inc }
  | "->"                     { at_line_start := false; MapsTo }
  | "+="                     { at_line_start := false; PlusAssign }
  | "-="                     { at_line_start := false; MinusAssign }
  | "*="                     { at_line_start := false; StarAssign }
  | "/="                     { at_line_start := false; SlashAssign }
  | "&="                     { at_line_start := false; BitWiseANDAssign }
  | "`="                     { at_line_start := false; BitWiseORAssign }
  | "$="                     { at_line_start := false; BitWiseXORAssign }
  | "|>"                     { at_line_start := false; Pipeline }
  | "<<="                    { at_line_start := false; LeftShiftAssign }
  | ">>="                    { at_line_start := false; RightShiftAssign }

  | "+"                      { at_line_start := false; Plus }
  | "-"                      { at_line_start := false; Minus }
  | "*"                      { at_line_start := false; Star }
  | "/"                      { at_line_start := false; Slash }
  | "%"                      { at_line_start := false; Mod }
  | "^"                      { at_line_start := false; Carot }
  | "="                      { at_line_start := false; Assign }
  | ">"                      { at_line_start := false; Greater }
  | "<"                      { at_line_start := false; Less }
  | "("                      { at_line_start := false; LParen }
  | ")"                      { at_line_start := false; RParen }
  | "["                      { at_line_start := false; LBracket }
  | "]"                      { at_line_start := false; RBracket }
  | "{"                      { at_line_start := false; LBrace }
  | "}"                      { at_line_start := false; RBrace }
  | "."                      { at_line_start := false; Dot }
  | ":"                      { at_line_start := false; Colon }
  | ";"                      { at_line_start := false; Semi }
  | ","                      { at_line_start := false; Comma }
  | "!"                      { at_line_start := false; Not }
  | "|"                      { at_line_start := false; Pipe }
  | "_"                      { at_line_start := false; UnderScore }
  | "?"                      { at_line_start := false; Question }
  | "`"                      { at_line_start := false; BitWiseOR }
  | "&"                      { at_line_start := false; BitWiseAND }
  | "~"                      { at_line_start := false; BitWiseNOT }
  | "$"                      { at_line_start := false; BitWiseXOR }
  | "<<"                     { at_line_start := false; LeftShift }
  | ">>"                     { at_line_start := false; RightShift }
  | ">>>"                    { at_line_start := false; RightShiftLogical }

  | "rec"                    { at_line_start := false; Recursive }
  | "if"                     { at_line_start := false; If }
  | "then"                   { at_line_start := false; Then }
  | "else"                   { at_line_start := false; Else }
  | "let"                    { at_line_start := false; Let }
  | "match"                  { at_line_start := false; Match }
  | "with"                   { at_line_start := false; With }
  | "return"                 { at_line_start := false; Return }
  | "for"                    { at_line_start := false; For }
  | "use"                    { at_line_start := false; Use }
  | "mod"                    { at_line_start := false; Module }
  | "true"                   { at_line_start := false; True }
  | "false"                  { at_line_start := false; False }
  | "enum"                   { at_line_start := false; Enum }

  | "int"                    { at_line_start := false; Int64Type }
  | "float"                  { at_line_start := false; FloatType }
  | "string"                 { at_line_start := false; StringType }
  | "byte"                   { at_line_start := false; ByteType }
  | "bool"                   { at_line_start := false; BoolType }
  | "unit"                   { at_line_start := false; UnitType }
  | "tuple"                  { at_line_start := false; TupleType }

  | floats as f              { at_line_start := false; Float (float_of_string f) }
  | digits as d              { at_line_start := false; Int64 (Int64.of_string d) }
  | identifier as id         { at_line_start := false; Ident id }

  | '\'' '\\' (['n' 't' '\\' '\'' '\"'] as esc) '\''  { at_line_start := false; let c = match esc with | 'n' -> '\n' | 't' -> '\t' | '\\' -> '\\' | '\'' -> '\'' | '\"' -> '\"' | '0' -> '\000' | _ -> esc in Byte c }
  | '\'' ([^'\n' '\\'] as c) '\''  { at_line_start := false; Byte c }
  | '\''                    { raise (LexerError "Unterminated character literal") }
  | '"'                     { at_line_start := false; read_string (Buffer.create 16) lexbuf }
  | eof                     { EOF }
  | "0b" ['0'-'1']+ as bin  { at_line_start := false; let len = String.length bin in let rec parse i acc = if i = len then acc else let bit = if bin.[i] = '1' then 1 else 0 in parse (i + 1) ((acc lsl 1) lor bit) in Int64 (Int64.of_int (parse 2 0)) }
  | "0x" ['0'-'9' 'a'-'f' 'A'-'F']+ as hex { at_line_start := false; let len = String.length hex in let rec parse i acc = if i = len then acc else let v = match hex.[i] with | '0'..'9' -> int_of_char hex.[i] - int_of_char '0' | 'a'..'f' -> 10 + int_of_char hex.[i] - int_of_char 'a' | 'A'..'F' -> 10 + int_of_char hex.[i] - int_of_char 'A' | _ -> failwith "Invalid hex digit" in parse (i + 1) ((acc lsl 4) lor v) in Int64 (Int64.of_int (parse 2 0)) }
  | _                       { let c = Lexing.lexeme_char lexbuf 0 in let msg = Printf.sprintf "Unrecognized character: '%c'" c in raise (LexerError msg) }

and read_string buf = parse
  | '"'                     { String (Buffer.contents buf) }
  | '\n'                    { raise (LexerError "Unterminated string literal") }
  | eof                     { raise (LexerError "Unterminated string literal") }
  | '\\' (['\\' '"' 'n' 't'] as esc) { Buffer.add_char buf ( match esc with | '\\' -> '\\' | '"' -> '"' | 'n' -> '\n' | 't' -> '\t' | _ -> esc ); read_string buf lexbuf }
  | _ as c                  { Buffer.add_char buf c; read_string buf lexbuf }

and read_comment = parse
  | newline                 { Lexing.new_line lexbuf; at_line_start := true; token lexbuf }
  | _                       { read_comment lexbuf }
  | eof                     { EOF }
