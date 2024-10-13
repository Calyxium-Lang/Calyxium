
(* The type of tokens. *)

type token = 
  | Var
  | True
  | Switch
  | StringType
  | String of (string)
  | StarAssign
  | Star
  | SlashAssign
  | Slash
  | Semi
  | Return
  | RParen
  | RBracket
  | RBrace
  | Pow
  | PlusAssign
  | Plus
  | Null
  | Not
  | New
  | Neq
  | Mod
  | MinusAssign
  | Minus
  | LogicalOr
  | LogicalAnd
  | Less
  | Leq
  | LParen
  | LBracket
  | LBrace
  | IntType
  | Int of (int64)
  | Inc
  | Import
  | If
  | Ident of (string)
  | Greater
  | Geq
  | Function
  | For
  | FloatType
  | Float of (float)
  | False
  | Export
  | Eq
  | Else
  | EOF
  | Dot
  | Default
  | Dec
  | Const
  | Comma
  | Colon
  | Class
  | Case
  | Carot
  | ByteType
  | Byte of (char)
  | BoolType
  | Bool of (bool)
  | Assign

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.Stmt.t)
