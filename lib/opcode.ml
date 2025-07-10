type opcode =
  | LOAD_INT64 of int64
  | LOAD_UINT32 of Uint32.t
  | LOAD_BINARY of int
  | LOAD_FLOAT of float
  | LOAD_STRING of string
  | LOAD_BYTE of char
  | LOAD_BOOL of bool
  | LOAD_UNIT of unit
  | LOAD_TUPLE of int
  | LOAD_ARRAY of int
  | LOAD_VAR of string
  | STORE_VAR of string
  | FUNCTION of string
  | LOAD_VAR_REF of string
  | JUMP of int
  | JUMP_IF_FALSE of int
  | CALL of string
  | TAIL_CALL of string
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
  | INT
  | STRING
  | PLUSASSIGN
  | MINUSASSIGN
  | STARASSIGN
  | SLASHASSIGN
  | LENGTH
  | BITWISEAND
  | BITWISEOR
  | BITWISEXOR
  | BITWISENOT
  | LEFTSHIFT
  | RIGHTSHIFT
  | RIGHTSHIFTLOGICAL
  | BITWISEANDASSIGN
  | BITWISEORASSIGN
  | BITWISEXORASSIGN
  | LEFTSHIFTASSIGN
  | RIGHTSHIFTASSIGN
  | ASSERT
[@@deriving show]
