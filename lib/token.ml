type t =
  | Eq
  | Neq
  | Geq
  | Leq
  | LogicalOr
  | LogicalAnd
  | Pow
  | Dec
  | Inc
  | Impiles
  | MapsTo
  | PlusAssign
  | MinusAssign
  | StarAssign
  | SlashAssign
  | Plus
  | Minus
  | Star
  | Slash
  | Mod
  | Carot
  | Assign
  | Greater
  | Less
  | LParen
  | RParen
  | LBracket
  | RBracket
  | LBrace
  | RBrace
  | Dot
  | Colon
  | Semi
  | Comma
  | Not
  | Pipe
  | UnderScore
  | Function
  | Recursive
  | If
  | Else
  | Let
  | Match
  | With
  | Return
  | For
  | Use
  | Module
  | True
  | False
  | IntType
  | FloatType
  | StringType
  | ByteType
  | BoolType
  | UnitType
  | Identifier of string
  | Int of int
  | Float of float
  | String of string
  | Byte of char
  | Bool of bool
  | Unit of unit
  | EOF
[@@deriving show]
