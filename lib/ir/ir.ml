open Calyxium_parser

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

let rec ast_to_ir_expr = function
  | Ast.Expr.IntExpr { value } -> IR_Int value
  | Ast.Expr.FloatExpr { value } -> IR_Float value
  | Ast.Expr.StringExpr { value } -> IR_String value
  | Ast.Expr.ByteExpr { value } -> IR_Byte value
  | Ast.Expr.BoolExpr { value } -> IR_Bool value
  | Ast.Expr.UnitExpr _ -> IR_Unit
  | Ast.Expr.VarExpr v -> IR_Var v
  | Ast.Expr.BinaryExpr { left; operator; right } ->
      IR_Binary (ast_to_ir_expr left, operator, ast_to_ir_expr right)
  | Ast.Expr.UnaryExpr { operator; operand } ->
      IR_Unary (operator, ast_to_ir_expr operand)
  | Ast.Expr.CallExpr { callee; arguments } -> (
      let ir_args = List.map ast_to_ir_expr arguments in
      match (callee, ir_args) with
      | Ast.Expr.VarExpr "to_byte", [ IR_Int n ] -> IR_Byte (Bigint.to_char n)
      | Ast.Expr.VarExpr "to_byte", [ IR_Array lst ] ->
          IR_Array
            (List.map
               (function IR_Int n -> IR_Byte (Bigint.to_char n) | x -> x)
               lst)
      | _ -> IR_Call (ast_to_ir_expr callee, ir_args))
  | Ast.Expr.ArrayExpr { elements } ->
      IR_Array (List.map ast_to_ir_expr elements)
  | Ast.Expr.IndexExpr { array; index } ->
      IR_Index (ast_to_ir_expr array, ast_to_ir_expr index)
  | Ast.Expr.TupleExpr elems -> IR_Tuple (List.map ast_to_ir_expr elems)
  | Ast.Expr.IfExpr { condition; then_branch; else_branch } ->
      IR_If
        ( ast_to_ir_expr condition,
          ast_to_ir_expr then_branch,
          Option.map ast_to_ir_expr else_branch )
  | Ast.Expr.BlockExpr { body } -> IR_BlockExpr (List.map ast_to_ir_stmt body)
  | Ast.Expr.VarDeclExpr { identifier; assigned_value; explicit_type } ->
      IR_VarDecl
        {
          var_name = identifier;
          typ = explicit_type;
          value = Option.map ast_to_ir_expr assigned_value;
        }
  | Ast.Expr.MultiVarDeclExpr { identifier; assigned_value; explicit_type } ->
      let value_ir =
        match assigned_value with
        | [] ->
            failwith
              "Multi-var assignment must have at least one RHS expression"
        | [ value ] -> ast_to_ir_expr value
        | _ -> IR_Tuple (List.map ast_to_ir_expr assigned_value)
      in
      IR_MultiVarDecl
        { names = identifier; value = value_ir; typ = explicit_type }
  | Ast.Expr.TernaryExpr { cond; onTrue; onFalse } ->
      IR_If
        ( ast_to_ir_expr cond,
          ast_to_ir_expr onTrue,
          Some (ast_to_ir_expr onFalse) )
  | Ast.Expr.PipelineExpr { left; right } ->
      IR_Pipeline (ast_to_ir_expr left, ast_to_ir_expr right)
  | Ast.Expr.LambdaExpr { parameters; body } ->
      IR_Lambda { parameters; body = [ IR_Expr (ast_to_ir_expr body) ] }
  | Ast.Expr.RangeExpr { start; end_ } ->
      IR_Range (Option.map ast_to_ir_expr start, Option.map ast_to_ir_expr end_)
  | Ast.Expr.SliceExpr { array; start; end_ } ->
      IR_Slice
        ( ast_to_ir_expr array,
          Option.map ast_to_ir_expr start,
          Option.map ast_to_ir_expr end_ )
  | Ast.Expr.EnumExpr { name; members } ->
      let numbered_members = List.mapi (fun i m -> (m, i)) members in
      IR_Enum { name; members = numbered_members }
  | Ast.Expr.DotExpr { left; right } -> IR_Dot (ast_to_ir_expr left, right)
  | Ast.Expr.MatchExpr { expr; cases } ->
      IR_Match
        ( ast_to_ir_expr expr,
          List.map
            (fun (case_expr_opt, body) ->
              ( Option.map ast_to_ir_expr case_expr_opt,
                List.map ast_to_ir_stmt body ))
            cases )
  | Ast.Expr.ImportExpr { module_name } -> IR_Import { module_name }
  | _ -> failwith "unhandled expression"

and ast_to_ir_stmt stmt =
  match stmt with
  | Ast.Stmt.BlockStmt { body } -> IR_Block (List.map ast_to_ir_stmt body)
  | Ast.Stmt.ExprStmt expr -> IR_Expr (ast_to_ir_expr expr)
  | Ast.Stmt.FunctionDeclStmt { name; is_rec; parameters; return_type; body } ->
      let body_ir = List.map ast_to_ir_stmt body in
      IR_FuncDecl { name; is_rec; parameters; return_type; body = body_ir }
  | _ -> failwith "unhandled statement"

let ast_to_ir ast = [ ast_to_ir_stmt ast ]
