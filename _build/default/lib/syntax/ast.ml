type token =
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

let pp_token fmt = function
  | Plus -> Format.fprintf fmt "Plus"
  | Minus -> Format.fprintf fmt "Minus"
  | Star -> Format.fprintf fmt "Star"
  | Slash -> Format.fprintf fmt "Slash"
  | Mod -> Format.fprintf fmt "Mod"
  | Pow -> Format.fprintf fmt "Pow"
  | Carot -> Format.fprintf fmt "Carot"
  | LParen -> Format.fprintf fmt "LParen"
  | RParen -> Format.fprintf fmt "RParen"
  | LBracket -> Format.fprintf fmt "LBracket"
  | RBracket -> Format.fprintf fmt "RBracket"
  | LBrace -> Format.fprintf fmt "LBrace"
  | RBrace -> Format.fprintf fmt "RBrace"
  | Dot -> Format.fprintf fmt "Dot"
  | Question -> Format.fprintf fmt "Question"
  | Colon -> Format.fprintf fmt "Colon"
  | Semi -> Format.fprintf fmt "Semi"
  | Comma -> Format.fprintf fmt "Comma"
  | Not -> Format.fprintf fmt "Not"
  | Pipe -> Format.fprintf fmt "Pipe"
  | Amspersand -> Format.fprintf fmt "Amspersand"
  | Greater -> Format.fprintf fmt "Greater"
  | Less -> Format.fprintf fmt "Less"
  | LogicalOr -> Format.fprintf fmt "LogicalOr"
  | LogicalAnd -> Format.fprintf fmt "LogicalAnd"
  | Eq -> Format.fprintf fmt "Eq"
  | Neq -> Format.fprintf fmt "Neq"
  | Geq -> Format.fprintf fmt "Geq"
  | Leq -> Format.fprintf fmt "Leq"
  | Dec -> Format.fprintf fmt "Dec"
  | Inc -> Format.fprintf fmt "Inc"
  | Assign -> Format.fprintf fmt "Assign"
  | PlusAssign -> Format.fprintf fmt "PlusAssign"
  | MinusAssign -> Format.fprintf fmt "MinusAssign"
  | StarAssign -> Format.fprintf fmt "StarAssign"
  | SlashAssign -> Format.fprintf fmt "SlashAssign"
  | Function -> Format.fprintf fmt "Function"
  | If -> Format.fprintf fmt "If"
  | Else -> Format.fprintf fmt "Else"
  | Return -> Format.fprintf fmt "Return"
  | Var -> Format.fprintf fmt "Var"
  | Const -> Format.fprintf fmt "Const"
  | Switch -> Format.fprintf fmt "Switch"
  | Case -> Format.fprintf fmt "Case"
  | Break -> Format.fprintf fmt "Break"
  | Default -> Format.fprintf fmt "Default"
  | For -> Format.fprintf fmt "For"
  | Import -> Format.fprintf fmt "Import"
  | Export -> Format.fprintf fmt "Export"
  | Class -> Format.fprintf fmt "Class"
  | True -> Format.fprintf fmt "True"
  | False -> Format.fprintf fmt "False"
  | New -> Format.fprintf fmt "New"
  | Null -> Format.fprintf fmt "Null"
  | IntType -> Format.fprintf fmt "IntType"
  | FloatType -> Format.fprintf fmt "FloatType"
  | StringType -> Format.fprintf fmt "StringType"
  | ByteType -> Format.fprintf fmt "ByteType"
  | BoolType -> Format.fprintf fmt "BoolType"
  | Ident s -> Format.fprintf fmt "Ident(%s)" s
  | Int i -> Format.fprintf fmt "Int(%d)" i
  | Float f -> Format.fprintf fmt "Float(%f)" f
  | String s -> Format.fprintf fmt "String(%s)" s
  | Byte c -> Format.fprintf fmt "Byte(%c)" c
  | Bool b -> Format.fprintf fmt "Bool(%b)" b
  | EOF -> Format.fprintf fmt "EOF"

module Type = struct
  type t =
    | SymbolType of { value : string }
    | ArrayType of { element_type : t }
    | ClassType of { name : string; properties : (string * t) list }
    | Any
  [@@deriving show]
end

module Expr = struct
  type t =
    | IntExpr of { value : int64 }
    | FloatExpr of { value : float }
    | StringExpr of { value : string }
    | ByteExpr of { value : char }
    | BoolExpr of { value : bool }
    | VarExpr of string
    | BinaryExpr of { left : t; operator : token; right : t }
    | CallExpr of { callee : t; arguments : t list }
    | UnaryExpr of { operator : token; operand : t }
    | NullExpr
    | NewExpr of { class_name : string; arguments : t list }
    | PropertyAccessExpr of { object_name : t; property_name : string }
    | ArrayExpr of { elements : t list }
    | IndexExpr of { array : t; index : t }
  [@@deriving show]
end

module Stmt = struct
  type parameter = { name : string; param_type : Type.t } [@@deriving show]

  type t =
    | BlockStmt of { body : t list }
    | VarDeclarationStmt of {
        identifier : string;
        constant : bool;
        assigned_value : Expr.t option;
        explicit_type : Type.t;
      }
    | NewVarDeclarationStmt of {
        identifier : string;
        constant : bool;
        assigned_value : Expr.t option;
        arguments : Expr.t list;
      }
    | FunctionDeclStmt of {
        name : string;
        parameters : parameter list;
        return_type : Type.t option;
        body : t list;
      }
    | ReturnStmt of Expr.t
    | ExprStmt of Expr.t
    | IfStmt of { condition : Expr.t; then_branch : t; else_branch : t option }
    | ForStmt of {
        init : t option;
        condition : Expr.t;
        increment : t option;
        body : t;
      }
    | ClassDeclStmt of {
        name : string;
        properties : parameter list;
        methods : t list;
      }
    | ImportStmt of { module_name : string }
    | ExportStmt of { identifier : string }
    | SwitchStmt of {
        expr : Expr.t;
        cases : (Expr.t * t list) list;
        default_case : t list option;
      }
  [@@deriving show]
end
