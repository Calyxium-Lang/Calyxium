module rec Type : sig
  type t =
    | VarType of int
    | SymbolType of { value : string }
    | ArrayType of { element_type : t }
    | TupleType of t list
    | FunctionType of t list * t
    | RecordType of (string * t) list
    | Any
    | Infer
end = struct
  type t =
    | VarType of int
    | SymbolType of { value : string }
    | ArrayType of { element_type : t }
    | TupleType of t list
    | FunctionType of t list * t
    | RecordType of (string * t) list
    | Any
    | Infer
end

and Stmt : sig
  type parameter = { name : string; param_type : Type.t }

  type t =
    | BlockStmt of { body : t list }
    | FunctionDeclStmt of {
        name : string;
        is_rec : bool;
        parameters : parameter list;
        return_type : Type.t;
        body : t list;
      }
    | ExprStmt of Expr.t
    | RecordStmt of { name : string; fields : Expr.t list }
end = struct
  type parameter = { name : string; param_type : Type.t }

  type t =
    | BlockStmt of { body : t list }
    | FunctionDeclStmt of {
        name : string;
        is_rec : bool;
        parameters : parameter list;
        return_type : Type.t;
        body : t list;
      }
    | ExprStmt of Expr.t
    | RecordStmt of { name : string; fields : Expr.t list }
end

and Expr : sig
  type t =
    | IntExpr of { value : Bigint.t }
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
    | IfExpr of { condition : t; then_branch : t; else_branch : t option }
    | DotExpr of { left : t; right : string }
    | TernaryExpr of { cond : t; onTrue : t; onFalse : t }
    | TupleExpr of t list
    | PipelineExpr of { left : t; right : t }
    | MatchExpr of { expr : t; cases : (t option * Stmt.t list) list }
    | RangeExpr of { start : t option; end_ : t option }
    | BlockExpr of { body : Stmt.t list }
    | SliceExpr of { array : t; start : t option; end_ : t option }
    | VarDeclExpr of {
        identifier : string;
        assigned_value : t option;
        explicit_type : Type.t;
      }
    | MultiVarDeclExpr of {
        identifier : string list;
        assigned_value : t list;
        explicit_type : Type.t;
      }
    | ImportExpr of { module_name : string list }
    | ModuleExpr of { module_name : string; block : t list }
    | EnumExpr of { name : string; members : string list }
end = struct
  type t =
    | IntExpr of { value : Bigint.t }
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
    | IfExpr of { condition : t; then_branch : t; else_branch : t option }
    | DotExpr of { left : t; right : string }
    | TernaryExpr of { cond : t; onTrue : t; onFalse : t }
    | TupleExpr of t list
    | PipelineExpr of { left : t; right : t }
    | MatchExpr of { expr : t; cases : (t option * Stmt.t list) list }
    | RangeExpr of { start : t option; end_ : t option }
    | BlockExpr of { body : Stmt.t list }
    | SliceExpr of { array : t; start : t option; end_ : t option }
    | VarDeclExpr of {
        identifier : string;
        assigned_value : t option;
        explicit_type : Type.t;
      }
    | MultiVarDeclExpr of {
        identifier : string list;
        assigned_value : t list;
        explicit_type : Type.t;
      }
    | ImportExpr of { module_name : string list }
    | ModuleExpr of { module_name : string; block : t list }
    | EnumExpr of { name : string; members : string list }
end
