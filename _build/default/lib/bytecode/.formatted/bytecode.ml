open Syntax
open Syntax.Ast.Stmt

type opcode =
  | LOAD_INT of int64
  | LOAD_FLOAT of float
  | LOAD_VAR of string
  | STORE_VAR of string
  | LOAD_STRING of string
  | LOAD_BYTE of char
  | LOAD_BOOL of bool
  | LOAD_ARRAY of int
  | LOAD_INDEX
  | FUNC of string
  | POW
  | MOD
  | CONCAT
  | FADD
  | FSUB
  | FMUL
  | FDIV
  | POP
  | HALT
  | RETURN
  | AND
  | OR
  | NOT
  | EQUAL
  | NOT_EQUAL
  | GREATER_EQUAL
  | LESS_EQUAL
  | GREATER
  | LESS
  | INC
  | DEC
  | JUMP of int
  | JUMP_IF_FALSE of int
  | PRINT
  | PRINTLN
  | LEN
  | TOSTRING
  | TOINT
  | TOFLOAT
  | CALL of string
  | PUSH_ARGS
  | SWITCH
  | CASE of float
  | DEFAULT
  | BREAK
  | DUP
  | INPUT

let function_table : (string, opcode list) Hashtbl.t = Hashtbl.create 10

let pp_opcode fmt = function
  | LOAD_INT value -> Format.fprintf fmt "LOAD_INT %d" (Int64.to_int value)
  | LOAD_FLOAT value -> Format.fprintf fmt "LOAD_FLOAT %f" value
  | LOAD_VAR name -> Format.fprintf fmt "LOAD_VAR %s" name
  | STORE_VAR name -> Format.fprintf fmt "STORE_VAR %s" name
  | LOAD_STRING value -> Format.fprintf fmt "LOAD_STRING %s" value
  | LOAD_BYTE value -> Format.fprintf fmt "LOAD_BYTE %c" value
  | LOAD_BOOL value -> Format.fprintf fmt "LOAD_BOOL %b" value
  | FUNC name -> Format.fprintf fmt "FUNC %s" name
  | POW -> Format.fprintf fmt "POW"
  | MOD -> Format.fprintf fmt "MOD"
  | CONCAT -> Format.fprintf fmt "CONCAT"
  | FADD -> Format.fprintf fmt "FADD"
  | FSUB -> Format.fprintf fmt "FSUB"
  | FMUL -> Format.fprintf fmt "FMUL"
  | FDIV -> Format.fprintf fmt "FDIV"
  | POP -> Format.fprintf fmt "POP"
  | RETURN -> Format.fprintf fmt "RETURN"
  | HALT -> Format.fprintf fmt "HALT"
  | AND -> Format.fprintf fmt "AND"
  | OR -> Format.fprintf fmt "OR"
  | NOT -> Format.fprintf fmt "NOT"
  | EQUAL -> Format.fprintf fmt "EQUAL"
  | NOT_EQUAL -> Format.fprintf fmt "NOT_EQUAL"
  | GREATER_EQUAL -> Format.fprintf fmt "GREATER_EQUAL"
  | LESS_EQUAL -> Format.fprintf fmt "LESS_EQUAL"
  | GREATER -> Format.fprintf fmt "GREATER"
  | INC -> Format.fprintf fmt "INC"
  | DEC -> Format.fprintf fmt "DEC"
  | LESS -> Format.fprintf fmt "LESS"
  | JUMP label -> Format.fprintf fmt "JUMP %d" label
  | JUMP_IF_FALSE label -> Format.fprintf fmt "JUMP_IF_FALSE %d" label
  | PRINT -> Format.fprintf fmt "PRINT"
  | PRINTLN -> Format.fprintf fmt "PRINTLN"
  | LEN -> Format.fprintf fmt "LEN"
  | TOSTRING -> Format.fprintf fmt "TOSTRING"
  | TOINT -> Format.fprintf fmt "TOINT"
  | TOFLOAT -> Format.fprintf fmt "TOFLOAT"
  | LOAD_ARRAY value -> Format.fprintf fmt "LOAD_ARRAY %d" value
  | LOAD_INDEX -> Format.fprintf fmt "LOAD_INDEX"
  | CALL name -> Format.fprintf fmt "CALL %s" name
  | PUSH_ARGS -> Format.fprintf fmt "PUSH ARGS"
  | SWITCH -> Format.fprintf fmt "SWITCH"
  | CASE value -> Format.fprintf fmt "CASE %f" value
  | DEFAULT -> Format.fprintf fmt "DEFAULT"
  | BREAK -> Format.fprintf fmt "BREAK"
  | DUP -> Format.fprintf fmt "DUP"
  | INPUT -> Format.fprintf fmt "INPUT"

let rec compile_expr = function
  | Ast.Expr.IntExpr { value } -> [ LOAD_INT value ]
  | Ast.Expr.FloatExpr { value } -> [ LOAD_FLOAT value ]
  | Ast.Expr.StringExpr { value } -> [ LOAD_STRING value ]
  | Ast.Expr.ByteExpr { value } -> [ LOAD_BYTE value ]
  | Ast.Expr.BoolExpr { value } ->
      if value then [ LOAD_BOOL true ] else [ LOAD_BOOL false ]
  | Ast.Expr.VarExpr name -> [ LOAD_VAR name ]
  | Ast.Expr.IndexExpr { array; index } ->
      compile_expr array @ compile_expr index @ [ LOAD_INDEX ]
  | Ast.Expr.BinaryExpr { left; operator; right } -> (
      let left_bytecode = compile_expr left in
      let right_bytecode = compile_expr right in
      match operator with
      | Ast.Plus -> left_bytecode @ right_bytecode @ [ FADD ]
      | Ast.Minus -> left_bytecode @ right_bytecode @ [ FSUB ]
      | Ast.Star -> left_bytecode @ right_bytecode @ [ FMUL ]
      | Ast.Slash -> left_bytecode @ right_bytecode @ [ FDIV ]
      | Ast.Mod -> left_bytecode @ right_bytecode @ [ MOD ]
      | Ast.Pow -> left_bytecode @ right_bytecode @ [ POW ]
      | Ast.Carot -> left_bytecode @ right_bytecode @ [ CONCAT ]
      | Ast.LogicalAnd -> left_bytecode @ right_bytecode @ [ AND ]
      | Ast.LogicalOr -> left_bytecode @ right_bytecode @ [ OR ]
      | Ast.Greater -> left_bytecode @ right_bytecode @ [ GREATER ]
      | Ast.Less -> left_bytecode @ right_bytecode @ [ LESS ]
      | Ast.Eq -> left_bytecode @ right_bytecode @ [ EQUAL ]
      | Ast.Geq -> left_bytecode @ right_bytecode @ [ GREATER_EQUAL ]
      | Ast.Leq -> left_bytecode @ right_bytecode @ [ LESS_EQUAL ]
      | Ast.Neq -> left_bytecode @ right_bytecode @ [ NOT_EQUAL ]
      | _ -> failwith "ByteCode: Unsupported operator")
  | Ast.Expr.CallExpr { callee; arguments } -> (
      match callee with
      | Ast.Expr.VarExpr "print" ->
          let args_bytecode =
            List.fold_left (fun acc arg -> acc @ compile_expr arg) [] arguments
          in
          args_bytecode @ [ PRINT ]
      | Ast.Expr.VarExpr "println" ->
          let args_bytecode =
            List.fold_left (fun acc arg -> acc @ compile_expr arg) [] arguments
          in
          args_bytecode @ [ PRINTLN ]
      | Ast.Expr.VarExpr "len" ->
          let args_bytecode =
            List.fold_left (fun acc arg -> acc @ compile_expr arg) [] arguments
          in
          args_bytecode @ [ LEN ]
      | Ast.Expr.VarExpr "ToString" ->
          let args_bytecode =
            List.fold_left (fun acc arg -> acc @ compile_expr arg) [] arguments
          in
          args_bytecode @ [ TOSTRING ]
      | Ast.Expr.VarExpr "ToInt" ->
          let args_bytecode =
            List.fold_left (fun acc arg -> acc @ compile_expr arg) [] arguments
          in
          args_bytecode @ [ TOINT ]
      | Ast.Expr.VarExpr "ToFloat" ->
          let args_bytecode =
            List.fold_left (fun acc arg -> acc @ compile_expr arg) [] arguments
          in
          args_bytecode @ [ TOFLOAT ]
      | Ast.Expr.VarExpr "input" ->
          let args_bytecode =
            List.fold_left (fun acc arg -> acc @ compile_expr arg) [] arguments
          in
          args_bytecode @ [ INPUT ]
      | Ast.Expr.VarExpr function_name ->
          let args_bytecode =
            List.fold_left (fun acc arg -> acc @ compile_expr arg) [] arguments
          in
          args_bytecode @ [ CALL function_name ]
      | _ -> failwith "ByteCode: Unsupported function call")
  | Ast.Expr.UnaryExpr { operator; operand } -> (
      let operand_bytecode = compile_expr operand in
      match operator with
      | Ast.Not -> operand_bytecode @ [ NOT ]
      | Ast.Inc -> operand_bytecode @ [ INC ]
      | Ast.Dec -> operand_bytecode @ [ DEC ]
      | _ -> failwith "Unsupported unary operator")
  | Ast.Expr.NullExpr -> failwith "NullExpr not supported"
  | Ast.Expr.NewExpr _ -> failwith "NewExpr not supported"
  | Ast.Expr.PropertyAccessExpr _ -> failwith "PropertyAccessExpr"
  | Ast.Expr.ArrayExpr { elements } ->
      let elements_bytecode = List.concat (List.map compile_expr elements) in
      elements_bytecode @ [ LOAD_ARRAY (List.length elements) ]

let rec compile_stmt = function
  | Ast.Stmt.ExprStmt expr -> compile_expr expr
  | Ast.Stmt.BlockStmt { body } ->
      let rec compile_body = function
        | [] -> []
        | [ stmt ] -> compile_stmt stmt
        | stmt :: rest -> compile_stmt stmt @ compile_body rest
      in
      compile_body body
  | Ast.Stmt.ReturnStmt expr -> compile_expr expr @ [ RETURN ]
  | Ast.Stmt.IfStmt { condition; then_branch; else_branch } ->
      let condition_bytecode = compile_expr condition in
      let then_bytecode = compile_stmt then_branch in
      let else_bytecode =
        match else_branch with Some branch -> compile_stmt branch | None -> []
      in
      let then_jump_label = List.length then_bytecode + 1 in
      let else_jump_label = List.length else_bytecode + 1 in
      condition_bytecode
      @ [ JUMP_IF_FALSE (then_jump_label + 1) ]
      @ then_bytecode @ [ JUMP else_jump_label ] @ else_bytecode
  | Ast.Stmt.VarDeclarationStmt
      { identifier; constant = _; assigned_value; explicit_type = _ } ->
      let expr_bytecode =
        match assigned_value with
        | Some expr -> compile_expr expr
        | None -> [ LOAD_INT 0L ]
      in
      expr_bytecode @ [ STORE_VAR identifier ]
  | Ast.Stmt.NewVarDeclarationStmt _ ->
      failwith "NewVarDeclarationStmt not supported"
  | Ast.Stmt.FunctionDeclStmt { name; parameters; body; _ } ->
      let start_bytecode = [ FUNC name ] in
      let function_body = compile_stmt (Ast.Stmt.BlockStmt { body }) in
      let param_bytecodes =
        List.map
          (fun (param : Syntax.Ast.Stmt.parameter) -> [ STORE_VAR param.name ])
          parameters
      in
      let full_function_bytecode =
        start_bytecode @ List.concat param_bytecodes @ function_body
      in
      Hashtbl.add function_table name full_function_bytecode;
      full_function_bytecode
  | Ast.Stmt.ForStmt _ -> failwith "ForStmt not implemented"
  | Ast.Stmt.ClassDeclStmt _ -> failwith "ClassStmt not implemented"
  | Ast.Stmt.SwitchStmt { expr; cases; default_case } ->
      let expr_bytecode = compile_expr expr in
      let switch_bytecode = ref expr_bytecode in
      let compiled_cases =
        List.mapi
          (fun _i (case_expr, case_body) ->
            let case_bytecode = compile_expr case_expr in
            let case_compare_bytecode = [ DUP ] @ case_bytecode @ [ EQUAL ] in
            let case_body_bytecode =
              List.flatten (List.map compile_stmt case_body)
            in
            let jump_to_next_case = List.length case_body_bytecode + 2 in
            let jump_if_false = [ JUMP_IF_FALSE jump_to_next_case ] in
            let jump_to_end = [ JUMP (-1) ] in
            case_compare_bytecode @ jump_if_false @ case_body_bytecode
            @ jump_to_end)
          cases
      in
      let default_bytecode =
        match default_case with
        | Some body -> List.flatten (List.map compile_stmt body)
        | None -> []
      in
      switch_bytecode :=
        !switch_bytecode @ List.flatten compiled_cases @ default_bytecode;
      let end_of_switch = List.length !switch_bytecode in
      let patched_bytecode =
        List.mapi
          (fun i instr ->
            if instr = JUMP (-1) then
              let jump_distance = end_of_switch - i in
              JUMP jump_distance
            else instr)
          !switch_bytecode
      in
      patched_bytecode
  | Ast.Stmt.ImportStmt _ -> failwith "ImportStmt not implemented"
  | Ast.Stmt.ExportStmt _ -> failwith "ExportStmt not implemented"
