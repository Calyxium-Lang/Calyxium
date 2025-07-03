type opcode =
  | LOAD_INT of int64
  | LOAD_FLOAT of float
  | LOAD_STRING of string
  | LOAD_BYTE of char
  | LOAD_BOOL of bool
  | LOAD_UNIT of unit
  | LOAD_ARRAY of int
  | LOAD_VAR of string
  | STORE_VAR of string
  | FUNCTION of string
  | JUMP of int
  | JUMP_IF_FALSE of int
  | CALL of string
  | LOAD_INDEX
  | PLUS
  | MINUS
  | STAR
  | SLASH
  | MOD
  | POW
  | CONCAT
  | AND
  | OR
  | GREATER
  | LESS
  | EQUAL
  | GREATER_EQUAL
  | LESS_EQUAL
  | NOT_EQUAL
  | RETURN
  | PRINTLN
  | NOT
  | INC
  | DEC
  | DUP
  | POP
  | INPUT
  | NEG
  | FLOAT
[@@deriving show]
