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
      | VarExpr function_name ->
          let args_bytecode =
            List.fold_left (fun acc arg -> acc @ compile_expr arg) [] arguments
          in
          args_bytecode @ [ CALL function_name ]
      | _ -> failwith "Not implemented")
  | ArrayExpr { elements } ->
      let elements_bytecode = List.concat (List.map compile_expr elements) in
      elements_bytecode @ [ LOAD_ARRAY (List.length elements) ]
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
      full_function_bytecode
  | VarDeclarationStmt { identifier; assigned_value; explicit_type = _ } ->
      let expr_bytecode =
        match assigned_value with
        | Some expr -> compile_expr expr
        | None -> [ LOAD_INT 0L ]
      in
      expr_bytecode @ [ STORE_VAR identifier ]
  | _ -> failwith ""
