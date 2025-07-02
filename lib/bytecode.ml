open Opcode
open Token
open Ast.Expr
open Ast.Stmt

let function_table : (string, opcode list) Hashtbl.t = Hashtbl.create 10

let opcode_of_binop = function
  | Plus -> PLUS
  | Minus -> MINUS
  | Star -> STAR
  | Slash -> SLASH
  | Mod -> MOD
  | Pow -> POW
  | Carot -> CONCAT
  | LogicalAnd -> AND
  | LogicalOr -> OR
  | Greater -> GREATER
  | Less -> LESS
  | Eq -> EQUAL
  | Geq -> GREATER_EQUAL
  | Leq -> LESS_EQUAL
  | Neq -> NOT_EQUAL
  | _ -> failwith "Unsupported operator"

let rec compile_expr = function
  | IntExpr { value } -> [ LOAD_INT value ]
  | FloatExpr { value } -> [ LOAD_FLOAT value ]
  | StringExpr { value } -> [ LOAD_STRING value ]
  | ByteExpr { value } -> [ LOAD_BYTE value ]
  | UnitExpr { value } -> [ LOAD_UNIT value ]
  | BoolExpr { value } ->
      if value then [ LOAD_BOOL true ] else [ LOAD_BOOL false ]
  | VarExpr name -> [ LOAD_VAR name ]
  | IndexExpr { array; index } ->
      compile_expr array @ compile_expr index @ [ LOAD_INDEX ]
  | BinaryExpr { left; operator; right } ->
      let left = compile_expr left in
      let right = compile_expr right in
      left @ right @ [ opcode_of_binop operator ]
  | ReturnExpr expr -> compile_expr expr @ [ RETURN ]
  | CallExpr { callee; arguments } -> (
      match callee with
      | VarExpr "println" ->
          let args_bytecode =
            List.fold_left (fun acc arg -> acc @ compile_expr arg) [] arguments
          in
          args_bytecode @ [ PRINTLN ]
      | VarExpr "input" ->
          let args_bytecode =
            List.fold_left (fun acc arg -> acc @ compile_expr arg) [] arguments
          in
          args_bytecode @ [ INPUT ]
      | VarExpr function_name ->
          let args_bytecode =
            List.fold_left (fun acc arg -> acc @ compile_expr arg) [] arguments
          in
          args_bytecode @ [ CALL function_name ]
      | _ -> failwith "Not implemented")
  | ArrayExpr { elements } ->
      let elements_bytecode = List.concat (List.map compile_expr elements) in
      elements_bytecode @ [ LOAD_ARRAY (List.length elements) ]
  | UnaryExpr { operator; operand } -> (
      let operand = compile_expr operand in
      match operator with
      | Not -> operand @ [ NOT ]
      | Inc -> operand @ [ INC ]
      | Dec -> operand @ [ DEC ]
      | _ -> failwith "Unsupported unary operator")
  | IfExpr { condition; then_branch; else_branch } ->
      let condition_code = compile_expr condition in
      let then_code = compile_expr then_branch in
      let else_code = compile_expr else_branch in
      let then_jump = List.length then_code + 2 in
      let else_jump = List.length else_code + 1 in
      condition_code
      @ [ JUMP_IF_FALSE then_jump ]
      @ then_code @ [ JUMP else_jump ] @ else_code
  | _ -> failwith "Not implemented"

let rec compile_stmt = function
  | ExprStmt expr -> compile_expr expr
  | BlockStmt { body } -> List.flatten (List.map compile_stmt body)
  | FunctionDeclStmt { name; parameters; body; _ } ->
      let start_bytecode = [ FUNCTION name ] in
      let function_body = compile_stmt (Ast.Stmt.BlockStmt { body }) in
      let param_bytecodes =
        List.map
          (fun (param : parameter) -> [ STORE_VAR param.name ])
          parameters
      in
      let full_function_bytecode =
        start_bytecode @ List.concat param_bytecodes @ function_body
      in
      Hashtbl.add function_table name full_function_bytecode;
      []
  | VarDeclarationStmt { identifier; assigned_value; explicit_type = _ } ->
      let expr_bytecode =
        match assigned_value with
        | Some expr -> compile_expr expr
        | None -> [ LOAD_INT 0L ]
      in
      expr_bytecode @ [ STORE_VAR identifier ]
  | IfStmt { condition; then_branch; else_branch } ->
      let condition = compile_expr condition in
      let then_branch = compile_stmt then_branch in
      let else_branch =
        match else_branch with Some branch -> compile_stmt branch | None -> []
      in
      let then_jump_label = List.length then_branch + 1 in
      let else_jump_label = List.length else_branch + 1 in
      condition
      @ [ JUMP_IF_FALSE (then_jump_label + 1) ]
      @ then_branch @ [ JUMP else_jump_label ] @ else_branch
  | MatchStmt { expr; cases } ->
      let expr_bytecode = compile_expr expr in
      let compiled_cases = ref [] in
      let jump_placeholders = ref [] in
      let match_code = expr_bytecode @ [ DUP ] in
      List.iter
        (fun (case_expr_opt, case_body) ->
          let body_code = List.flatten (List.map compile_stmt case_body) in
          let body_len = List.length body_code in
          let jump_to_next_case = body_len + 2 in
          match case_expr_opt with
          | Some case_expr ->
              let cmp_code =
                [ DUP ] @ compile_expr case_expr
                @ [ EQUAL; JUMP_IF_FALSE jump_to_next_case ]
              in
              compiled_cases :=
                !compiled_cases @ cmp_code @ body_code @ [ JUMP (-1) ];
              jump_placeholders :=
                !jump_placeholders @ [ ref (List.length !compiled_cases - 1) ]
          | None ->
              compiled_cases := !compiled_cases @ body_code @ [ JUMP (-1) ];
              jump_placeholders :=
                !jump_placeholders @ [ ref (List.length !compiled_cases - 1) ])
        cases;
      let full_code = match_code @ !compiled_cases @ [ POP ] in
      let final_len = List.length full_code in
      let rec patch_jumps idx code =
        match code with
        | [] -> []
        | JUMP -1 :: rest ->
            let jump_len = final_len - idx in
            JUMP jump_len :: patch_jumps (idx + 1) rest
        | instr :: rest -> instr :: patch_jumps (idx + 1) rest
      in
      patch_jumps 0 full_code
  | ForStmt { init; condition; increment; body } ->
      let init_code =
        match init with Some stmt -> compile_stmt stmt | None -> []
      in
      let condition_code = compile_expr condition in
      let body_code = compile_stmt body in
      let increment_code =
        match increment with Some stmt -> compile_stmt stmt | None -> []
      in
      let cond_len = List.length condition_code in
      let body_len = List.length body_code in
      let incr_len = List.length increment_code in
      let jump_past_body = body_len + incr_len + 1 in
      let jump_back_to_condition = -(cond_len + 1 + body_len + incr_len) in
      init_code @ condition_code
      @ [ JUMP_IF_FALSE jump_past_body ]
      @ body_code @ increment_code
      @ [ JUMP jump_back_to_condition ]
  | _ -> failwith ""
