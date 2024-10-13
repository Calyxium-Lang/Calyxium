type opcode =
  | LOAD_INT of int64
  | LOAD_FLOAT of float
  | LOAD_VAR of string
  | STORE_VAR of string
  | LOAD_STRING of string
  | LOAD_BYTE of char
  | LOAD_BOOL of bool
  | LOAD_ARRAY of int
  | LOAD_INDEX
  | FUNC of string
  | POW
  | MOD
  | CONCAT
  | FADD
  | FSUB
  | FMUL
  | FDIV
  | POP
  | HALT
  | RETURN
  | AND
  | OR
  | NOT
  | EQUAL
  | NOT_EQUAL
  | GREATER_EQUAL
  | LESS_EQUAL
  | GREATER
  | LESS
  | INC
  | DEC
  | JUMP of int
  | JUMP_IF_FALSE of int
  | PRINT
  | PRINTLN
  | LEN
  | TOSTRING
  | TOINT
  | TOFLOAT
  | CALL of string
  | PUSH_ARGS
  | SWITCH
  | CASE of float
  | DEFAULT
  | BREAK
  | DUP
  | INPUT

val function_table : (string, opcode list) Hashtbl.t
val pp_opcode : Format.formatter -> opcode -> unit
val compile_expr : Syntax.Ast.Expr.t -> opcode list
val compile_stmt : Syntax.Ast.Stmt.t -> opcode list
