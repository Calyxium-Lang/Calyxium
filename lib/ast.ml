module Type = struct
  type t =
    | SymbolType of { value : string }
    | ArrayType of { element_type : t }
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
    | UnitExpr of { value : unit }
    | VarExpr of string
    | BinaryExpr of { left : t; operator : Token.t; right : t }
    | CallExpr of { callee : t; arguments : t list }
    | UnaryExpr of { operator : Token.t; operand : t }
    | ArrayExpr of { elements : t list }
    | IndexExpr of { array : t; index : t }
    | IfExpr of { condition : t; then_branch : t; else_branch : t }
    | ReturnExpr of t
  [@@deriving show]
end

module Stmt = struct
  type parameter = { name : string; param_type : Type.t } [@@deriving show]

  type t =
    | BlockStmt of { body : t list }
    | VarDeclarationStmt of {
        identifier : string;
        assigned_value : Expr.t option;
        explicit_type : Type.t;
      }
    | FunctionDeclStmt of {
        name : string;
        is_rec : bool;
        parameters : parameter list;
        return_type : Type.t;
        body : t list;
      }
    | IfStmt of { condition : Expr.t; then_branch : t; else_branch : t option }
    | ForStmt of {
        init : t option;
        condition : Expr.t;
        increment : t option;
        body : t;
      }
    | ImportStmt of { module_name : string }
    | ModuleStmt of { module_name : string; block : t list }
    | MatchStmt of {
        expr : Expr.t;
        cases : (Expr.t * t list) list;
        default_case : t list option;
      }
    | ExprStmt of Expr.t
  [@@deriving show]
end
