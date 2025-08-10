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
  | ArrConcat
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
  | DeRef
  | UnderScore
  | Function
  | Question
  | Recursive
  | If
  | Then
  | Else
  | Let
  | Match
  | With
  | Return
  | Use
  | Module
  | True
  | False
  | Enum
  | Ref
  | IntType
  | FloatType
  | StringType
  | ByteType
  | BoolType
  | UnitType
  | Ident of string
  | Int of Bigint.t
  | Float of float
  | String of string
  | Byte of char
  | Bool of bool
  | Unit of unit
  | Tuple of t list
  | Binary of int
  | BitWiseAND
  | BitWiseOR
  | BitWiseXOR
  | BitWiseNOT
  | LeftShift
  | RightShift
  | BitWiseANDAssign
  | BitWiseORAssign
  | BitWiseXORAssign
  | LeftShiftAssign
  | RightShiftAssign
  | Pipeline
  | Range
  | EOF
