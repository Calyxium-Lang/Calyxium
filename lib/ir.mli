type instr = ..
type expr = ..
type var = { var_name : string; typ : Ast.Type.t; value : expr option }

type func_decl = {
  name : string;
  is_rec : bool;
  parameters : Ast.Stmt.parameter list;
  return_type : Ast.Type.t;
  body : instr list;
}

type instr +=
  | IR_Expr of expr
  | IR_Block of instr list
  | IR_FuncDecl of func_decl

type expr +=
  | IR_Int of Bigint.t
  | IR_Float of float
  | IR_String of string
  | IR_Byte of char
  | IR_Bool of bool
  | IR_Unit
  | IR_Var of string
  | IR_Binary of expr * Token.t * expr
  | IR_Unary of Token.t * expr
  | IR_Call of expr * expr list
  | IR_Array of expr list
  | IR_Index of expr * expr
  | IR_Tuple of expr list
  | IR_If of expr * expr * expr option
  | IR_BlockExpr of instr list
  | IR_Ternary of expr * expr * expr
  | IR_Pipeline of expr * expr
  | IR_Lambda of { parameters : Ast.Stmt.parameter list; body : instr list }
  | IR_Range of expr option * expr option
  | IR_Slice of expr * expr option * expr option
  | IR_Enum of { name : string; members : (string * int) list }
  | IR_Dot of expr * string
  | IR_Import of { module_name : string list }
  | IR_Match of expr * (expr option * instr list) list
  | IR_VarDecl of var
  | IR_MultiVarDecl of { names : string list; value : expr; typ : Ast.Type.t }

type t = instr list

val ast_to_ir_expr : Ast.Expr.t -> expr
val ast_to_ir_stmt : Ast.Stmt.t -> instr
val ast_to_ir : Ast.Stmt.t -> instr list
