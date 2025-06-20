module Type : sig
  type t =
    | SymbolType of { value : string }
    | ArrayType of { element_type : t }
    | ClassType of { name : string; properties : (string * t) list }
    | Any
  [@@deriving show]
end

module Expr : sig
  type t =
    | IntExpr of { value : int64 }
    | FloatExpr of { value : float }
    | StringExpr of { value : string }
    | ByteExpr of { value : char }
    | BoolExpr of { value : bool }
    | UnitExpr of { value : unit }
    | VarExpr of string
    | BinaryExpr of { left : t; operator : Token.t; right : t }
    | CallExpr of { callee : t; arguments : t list }
    | UnaryExpr of { operator : Token.t; operand : t }
    | NewExpr of { class_name : string; arguments : t list }
    | PropertyAccessExpr of { object_name : t; property_name : string }
    | ArrayExpr of { elements : t list }
    | IndexExpr of { array : t; index : t }
  [@@deriving show]
end

module Stmt : sig
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
    | SwitchStmt of {
        expr : Expr.t;
        cases : (Expr.t * t list) list;
        default_case : t list option;
      }
  [@@deriving show]
end
