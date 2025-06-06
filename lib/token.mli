type t =
  | Plus
  | Minus
  | Star
  | Slash
  | Mod
  | Pow
  | Carot
  | LParen
  | RParen
  | LBracket
  | RBracket
  | LBrace
  | RBrace
  | Dot
  | Question
  | Colon
  | Semi
  | Comma
  | Not
  | Pipe
  | Amspersand
  | Greater
  | Less
  | LogicalOr
  | LogicalAnd
  | Eq
  | Neq
  | Geq
  | Leq
  | Dec
  | Inc
  | Assign
  | PlusAssign
  | MinusAssign
  | StarAssign
  | SlashAssign
  | Function
  | If
  | Else
  | Return
  | Var
  | Const
  | Switch
  | Case
  | Break
  | Default
  | For
  | Import
  | Export
  | Class
  | True
  | False
  | New
  | Null
  | IntType
  | FloatType
  | StringType
  | ByteType
  | BoolType
  | Ident of string
  | Int of int
  | Float of float
  | String of string
  | Byte of char
  | Bool of bool
  | EOF
[@@deriving show]
