type opcode =
  | LOAD_INT of int64
  | LOAD_FLOAT of float
  | LOAD_VAR of string
  | STORE_VAR of string
  | LOAD_STRING of string
  | LOAD_BYTE of char
  | LOAD_BOOL of bool
  | LOAD_UNIT of unit
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
[@@deriving show]

let function_table : (string, opcode list) Hashtbl.t = Hashtbl.create 10

let rec compile_expr = function
  | Ast.Expr.IntExpr { value } -> [ LOAD_INT value ]
  | Ast.Expr.FloatExpr { value } -> [ LOAD_FLOAT value ]
  | Ast.Expr.StringExpr { value } -> [ LOAD_STRING value ]
  | Ast.Expr.ByteExpr { value } -> [ LOAD_BYTE value ]
  | Ast.Expr.UnitExpr { value } -> [ LOAD_UNIT value ]
  | Ast.Expr.BoolExpr { value } ->
      if value then [ LOAD_BOOL true ] else [ LOAD_BOOL false ]
  | Ast.Expr.VarExpr name -> [ LOAD_VAR name ]
  | Ast.Expr.IndexExpr { array; index } ->
      compile_expr array @ compile_expr index @ [ LOAD_INDEX ]
  | Ast.Expr.BinaryExpr { left; operator; right } -> (
      let left_bytecode = compile_expr left in
      let right_bytecode = compile_expr right in
      match operator with
      | Token.Plus -> left_bytecode @ right_bytecode @ [ FADD ]
      | Token.Minus -> left_bytecode @ right_bytecode @ [ FSUB ]
      | Token.Star -> left_bytecode @ right_bytecode @ [ FMUL ]
      | Token.Slash -> left_bytecode @ right_bytecode @ [ FDIV ]
      | Token.Mod -> left_bytecode @ right_bytecode @ [ MOD ]
      | Token.Pow -> left_bytecode @ right_bytecode @ [ POW ]
      | Token.Carot -> left_bytecode @ right_bytecode @ [ CONCAT ]
      | Token.LogicalAnd -> left_bytecode @ right_bytecode @ [ AND ]
      | Token.LogicalOr -> left_bytecode @ right_bytecode @ [ OR ]
      | Token.Greater -> left_bytecode @ right_bytecode @ [ GREATER ]
      | Token.Less -> left_bytecode @ right_bytecode @ [ LESS ]
      | Token.Eq -> left_bytecode @ right_bytecode @ [ EQUAL ]
      | Token.Geq -> left_bytecode @ right_bytecode @ [ GREATER_EQUAL ]
      | Token.Leq -> left_bytecode @ right_bytecode @ [ LESS_EQUAL ]
      | Token.Neq -> left_bytecode @ right_bytecode @ [ NOT_EQUAL ]
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
      | Token.Not -> operand_bytecode @ [ NOT ]
      | Token.Inc -> operand_bytecode @ [ INC ]
      | Token.Dec -> operand_bytecode @ [ DEC ]
      | _ -> failwith "Unsupported unary operator")
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
      let function_body = compile_stmt (Ast.Stmt.BlockStmt { body }) in
      let param_bytecodes =
        List.map
          (fun (param : Ast.Stmt.parameter) -> [ STORE_VAR param.name ])
          parameters
      in
      let func_code = List.concat param_bytecodes @ function_body in
      Hashtbl.replace function_table name func_code;
      []
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
