type function_info = {
  return_type : Ast.Type.t;
  params : Ast.Stmt.parameter list;
  bytecode : Opcode.opcode list;
  body_code : Opcode.opcode array;
}

let function_table = Hashtbl.create 10
let enum_tbl = Hashtbl.create 10
let struct_tbl = Hashtbl.create 10

let builtins =
  [
    ("print", fun args -> args @ [ Opcode.PRINT ]);
    ("to_float", fun args -> args @ [ Opcode.FLOAT ]);
    ("to_int", fun args -> args @ [ Opcode.INT ]);
    ("to_string", fun args -> args @ [ Opcode.STRING ]);
    ("to_bytes", fun args -> args @ [ Opcode.BYTES ]);
    ("to_byte", fun args -> args @ [ Opcode.BYTE ]);
    ("length", fun args -> args @ [ Opcode.LENGTH ]);
    ("input", fun args -> args @ [ Opcode.INPUT ]);
    ("assert", fun args -> args @ [ Opcode.ASSERT ]);
    ("panic", fun args -> args @ [ Opcode.PANIC ]);
    ("of_type", fun args -> args @ [ Opcode.TYPE ]);
    ("head", fun args -> args @ [ Opcode.HEAD ]);
    ("tail", fun args -> args @ [ Opcode.TAIL ]);
    ("reverse", fun args -> args @ [ Opcode.REVERSE ]);
    ("fst", fun args -> args @ [ Opcode.FST ]);
    ("snd", fun args -> args @ [ Opcode.SND ]);
  ]

let gensym =
  let counter = ref 0 in
  fun prefix ->
    let name = prefix ^ string_of_int !counter in
    incr counter;
    name

let opcode_of_binop = function
  | Token.Plus -> Opcode.PLUS
  | Token.Minus -> Opcode.MINUS
  | Token.Star -> Opcode.STAR
  | Token.Slash -> Opcode.SLASH
  | Token.Mod -> Opcode.MOD
  | Token.Pow -> Opcode.POW
  | Token.Carot -> Opcode.CONCAT
  | Token.LogicalAnd -> Opcode.AND
  | Token.LogicalOr -> Opcode.OR
  | Token.Greater -> Opcode.GREATER
  | Token.Less -> Opcode.LESS
  | Token.Eq -> Opcode.EQUAL
  | Token.Geq -> Opcode.GREATER_EQUAL
  | Token.Leq -> Opcode.LESS_EQUAL
  | Token.Neq -> Opcode.NOT_EQUAL
  | Token.PlusAssign -> Opcode.PLUSASSIGN
  | Token.MinusAssign -> Opcode.MINUSASSIGN
  | Token.StarAssign -> Opcode.STARASSIGN
  | Token.SlashAssign -> Opcode.SLASHASSIGN
  | Token.BitWiseAND -> Opcode.BITWISEAND
  | Token.BitWiseOR -> Opcode.BITWISEOR
  | Token.BitWiseXOR -> Opcode.BITWISEXOR
  | Token.LeftShift -> Opcode.LEFTSHIFT
  | Token.RightShift -> Opcode.RIGHTSHIFT
  | Token.BitWiseANDAssign -> Opcode.BITWISEANDASSIGN
  | Token.BitWiseORAssign -> Opcode.BITWISEORASSIGN
  | Token.BitWiseXORAssign -> Opcode.BITWISEXORASSIGN
  | Token.LeftShiftAssign -> Opcode.LEFTSHIFTASSIGN
  | Token.RightShiftAssign -> Opcode.RIGHTSHIFTASSIGN
  | Token.ArrConcat -> Opcode.ARRAYCONCAT
  | _ -> failwith "Unsupported operator"

let rec compile_stmt = function
  | Ast.Stmt.ExprStmt expr -> compile_expr expr
  | Ast.Stmt.BlockStmt { body } -> List.flatten (List.map compile_stmt body)
  | Ast.Stmt.FunctionDeclStmt
      { name; is_rec = _; parameters; body; return_type } ->
      let param_stores =
        List.rev_map
          (fun (param : Ast.Stmt.parameter) ->
            Opcode.STORE_VAR param.Ast.Stmt.name)
          parameters
      in
      let start_bytecode = [ Opcode.FUNCTION name ] @ param_stores in
      let function_body = compile_stmt (Ast.Stmt.BlockStmt { body }) in
      let full_function_bytecode =
        start_bytecode @ function_body @ [ Opcode.RETURN ]
      in
      Hashtbl.replace function_table name
        {
          return_type;
          params = parameters;
          bytecode = full_function_bytecode;
          body_code = Array.of_list full_function_bytecode;
        };
      []
  | Ast.Stmt.RecordStmt { name; fields } ->
      let register_struct prefix fields =
        let full_name = String.concat "." prefix in
        let actual_fields =
          List.fold_left
            (fun acc expr ->
              match expr with
              | Ast.Expr.VarDeclExpr { identifier; assigned_value = Some v; _ }
                ->
                  (identifier, v) :: acc
              | _ -> failwith ("Invalid field in struct `" ^ full_name ^ "`"))
            [] fields
        in
        Hashtbl.replace struct_tbl full_name (List.rev actual_fields)
      in
      register_struct [ name ] fields;
      []
  | _ -> failwith "No Supported"

and compile_expr = function
  | Ast.Expr.IntExpr { value } -> [ Opcode.LOAD_INT value ]
  | Ast.Expr.FloatExpr { value } -> [ Opcode.LOAD_FLOAT value ]
  | Ast.Expr.StringExpr { value } -> [ Opcode.LOAD_STRING value ]
  | Ast.Expr.ByteExpr { value } -> [ Opcode.LOAD_BYTE value ]
  | Ast.Expr.UnitExpr { value } -> [ Opcode.LOAD_UNIT value ]
  | Ast.Expr.BoolExpr { value } ->
      if value then [ Opcode.LOAD_BOOL true ] else [ Opcode.LOAD_BOOL false ]
  | Ast.Expr.TupleExpr elements ->
      let compiled_elements = List.concat_map compile_expr elements in
      compiled_elements @ [ Opcode.LOAD_TUPLE (List.length elements) ]
  | Ast.Expr.VarExpr name -> [ Opcode.LOAD_VAR name ]
  | Ast.Expr.IndexExpr { array; index } ->
      compile_expr array @ compile_expr index @ [ Opcode.LOAD_INDEX ]
  | Ast.Expr.BinaryExpr { left; operator; right } -> (
      match operator with
      | Token.PlusAssign | Token.MinusAssign | Token.StarAssign
      | Token.SlashAssign | Token.BitWiseANDAssign | Token.BitWiseORAssign
      | Token.BitWiseXORAssign | Token.LeftShiftAssign | Token.RightShiftAssign
        -> (
          match left with
          | Ast.Expr.VarExpr name ->
              let load_var_ref_code = [ Opcode.LOAD_VAR_REF name ] in
              let rhs_code = compile_expr right in
              load_var_ref_code @ rhs_code @ [ opcode_of_binop operator ]
          | _ -> failwith "Assignment target must be a variable")
      | _ ->
          let left_code = compile_expr left in
          let right_code = compile_expr right in
          left_code @ right_code @ [ opcode_of_binop operator ])
  | Ast.Expr.CallExpr { callee; arguments } -> (
      let args_bytecode = List.concat (List.map compile_expr arguments) in
      match callee with
      | Ast.Expr.VarExpr name -> (
          match List.assoc_opt name builtins with
          | Some handler -> handler args_bytecode
          | None -> args_bytecode @ [ Opcode.CALL name ])
      | _ -> failwith "Unsupported call expression")
  | Ast.Expr.ArrayExpr { elements } ->
      let elements_bytecode = List.concat (List.map compile_expr elements) in
      elements_bytecode @ [ Opcode.LOAD_ARRAY (List.length elements) ]
  | Ast.Expr.UnaryExpr { operator; operand } -> (
      let operand = compile_expr operand in
      match operator with
      | Token.Not -> operand @ [ Opcode.NOT ]
      | Token.Inc -> (
          match operand with
          | [ Opcode.LOAD_VAR name ] ->
              [
                Opcode.LOAD_VAR name;
                Opcode.INC;
                Opcode.DUP;
                Opcode.STORE_VAR name;
              ]
          | _ -> failwith "INC expects a variable")
      | Token.Dec -> (
          match operand with
          | [ Opcode.LOAD_VAR name ] ->
              [
                Opcode.LOAD_VAR name;
                Opcode.DEC;
                Opcode.DUP;
                Opcode.STORE_VAR name;
              ]
          | _ -> failwith "DEC expects a variable")
      | Token.Minus -> operand @ [ Opcode.NEG ]
      | Token.BitWiseNOT -> operand @ [ Opcode.BITWISENOT ]
      | _ -> failwith "Unsupported unary operator")
  | Ast.Expr.IfExpr { condition; then_branch; else_branch } ->
      let condition_code = compile_expr condition in
      let then_code = compile_expr then_branch in
      let else_code =
        match else_branch with Some expr -> compile_expr expr | None -> []
      in
      let then_jump = List.length then_code + 2 in
      let else_jump = List.length else_code + 1 in
      condition_code
      @ [ Opcode.JUMP_IF_FALSE then_jump ]
      @ then_code @ [ Opcode.JUMP else_jump ] @ else_code
  | Ast.Expr.TernaryExpr { cond; onTrue; onFalse } ->
      let cond_code = compile_expr cond in
      let true_code = compile_expr onTrue in
      let false_code = compile_expr onFalse in
      let true_jump = List.length true_code + 2 in
      let false_jump = List.length false_code + 1 in
      cond_code
      @ [ Opcode.JUMP_IF_FALSE true_jump ]
      @ true_code @ [ Opcode.JUMP false_jump ] @ false_code
  | Ast.Expr.PipelineExpr { left; right } -> (
      let left_code = compile_expr left in
      match right with
      | Ast.Expr.VarExpr name -> (
          match List.assoc_opt name builtins with
          | Some handler -> handler left_code
          | None ->
              if Hashtbl.mem function_table name then
                left_code @ [ Opcode.CALL name ]
              else
                left_code @ [ Opcode.LOAD_VAR name ] @ [ Opcode.CALL_CLOSURE 1 ]
          )
      | Ast.Expr.LambdaExpr _ ->
          let closure_code = compile_expr right in
          left_code @ closure_code @ [ Opcode.CALL_CLOSURE 1 ]
      | _ ->
          failwith
            "Right-hand side of pipeline must be a function name or lambda")
  | Ast.Expr.DotExpr { left; right } -> (
      match left with
      | Ast.Expr.VarExpr struct_name -> (
          match Hashtbl.find_opt struct_tbl struct_name with
          | Some fields -> (
              match List.assoc_opt right fields with
              | Some value_expr -> compile_expr value_expr
              | None ->
                  failwith
                    ("Unknown field `" ^ right ^ "` in struct `" ^ struct_name
                   ^ "`"))
          | None -> (
              match Hashtbl.find_opt enum_tbl struct_name with
              | Some members -> (
                  match List.assoc_opt right members with
                  | Some value -> [ Opcode.LOAD_INT (Bigint.of_int value) ]
                  | None ->
                      failwith
                        ("Unknown enum member `" ^ right ^ "` in enum `"
                       ^ struct_name ^ "`"))
              | None -> failwith ("Unknown type `" ^ struct_name ^ "`")))
      | _ -> failwith "DotExpr left must be a struct or enum name")
  | Ast.Expr.MatchExpr { expr; cases } ->
      let expr_bytecode = compile_expr expr in
      let compiled_cases = ref [] in
      let jump_placeholders = ref [] in
      let match_code = expr_bytecode @ [ Opcode.DUP ] in
      List.iter
        (fun (case_expr_opt, case_body) ->
          let body_code = List.flatten (List.map compile_stmt case_body) in
          let body_len = List.length body_code in
          let jump_to_next_case = body_len + 2 in
          match case_expr_opt with
          | Some case_expr ->
              let cmp_code =
                [ Opcode.DUP ] @ compile_expr case_expr
                @ [ Opcode.EQUAL; Opcode.JUMP_IF_FALSE jump_to_next_case ]
              in
              compiled_cases :=
                !compiled_cases @ cmp_code @ body_code @ [ Opcode.JUMP (-1) ];
              jump_placeholders :=
                !jump_placeholders @ [ ref (List.length !compiled_cases - 1) ]
          | None ->
              compiled_cases :=
                !compiled_cases @ body_code @ [ Opcode.JUMP (-1) ];
              jump_placeholders :=
                !jump_placeholders @ [ ref (List.length !compiled_cases - 1) ])
        cases;
      let full_code = match_code @ !compiled_cases @ [ Opcode.POP ] in
      let final_len = List.length full_code in
      let rec patch_jumps idx code =
        match code with
        | [] -> []
        | Opcode.JUMP -1 :: rest ->
            let jump_len = final_len - idx in
            Opcode.JUMP jump_len :: patch_jumps (idx + 1) rest
        | instr :: rest -> instr :: patch_jumps (idx + 1) rest
      in
      patch_jumps 0 full_code
  | Ast.Expr.RangeExpr { start; end_ } -> (
      match (start, end_) with
      | ( Some (Ast.Expr.IntExpr { value = s }),
          Some (Ast.Expr.IntExpr { value = e }) ) ->
          let count = Bigint.to_int (Bigint.add (Bigint.sub e s) Bigint.one) in
          let range_elements =
            List.init count (fun i ->
                Opcode.LOAD_INT (Bigint.add s (Bigint.of_int i)))
          in
          range_elements @ [ Opcode.LOAD_ARRAY count ]
      | Some (Ast.Expr.IntExpr { value = s }), None ->
          [ Opcode.LOAD_INT s; Opcode.MAKE_RANGE ]
      | None, Some (Ast.Expr.IntExpr { value = e }) ->
          let count = Bigint.to_int (Bigint.add e Bigint.one) in
          let range_elements =
            List.init count (fun i -> Opcode.LOAD_INT (Bigint.of_int i))
          in
          range_elements @ [ Opcode.LOAD_ARRAY count ]
      | _ -> failwith "Range bounds must be integer literals for now")
  | Ast.Expr.BlockExpr { body } -> List.flatten (List.map compile_stmt body)
  | Ast.Expr.SliceExpr { array; start; end_ } ->
      let array_code = compile_expr array in
      let start_code =
        match start with
        | Some s -> compile_expr s
        | None -> [ Opcode.LOAD_UNIT () ]
      in
      let end_code =
        match end_ with
        | Some e -> compile_expr e
        | None -> [ Opcode.LOAD_UNIT () ]
      in
      array_code @ start_code @ end_code @ [ Opcode.SLICE ]
  | Ast.Expr.VarDeclExpr { identifier; assigned_value; explicit_type = _ } ->
      let expr_bytecode =
        match assigned_value with
        | Some expr -> compile_expr expr
        | None -> [ Opcode.LOAD_INT Bigint.zero ]
      in
      expr_bytecode @ [ Opcode.STORE_VAR identifier ]
  | Ast.Expr.MultiVarDeclExpr { identifier; assigned_value; _ } ->
      let rec flatten_expr expr =
        match expr with
        | Ast.Expr.TupleExpr elements -> List.concat_map flatten_expr elements
        | _ -> [ expr ]
      in
      let index_exprs expr =
        List.init (List.length identifier) (fun i ->
            Ast.Expr.IndexExpr
              {
                array = expr;
                index = Ast.Expr.IntExpr { value = Bigint.of_int i };
              })
      in
      let values =
        match assigned_value with
        | [ (Ast.Expr.TupleExpr _ as t) ] -> flatten_expr t
        | [ expr ] when List.length identifier > 1 -> index_exprs expr
        | _ -> assigned_value
      in
      if List.length identifier <> List.length values then
        failwith
          ("MultiVarDeclExpr: number of identifiers ("
          ^ string_of_int (List.length identifier)
          ^ ") does not match number of assigned values ("
          ^ string_of_int (List.length values)
          ^ ")");
      let expr_codes = List.map compile_expr values in
      let store_codes =
        List.map2
          (fun ident _ -> [ Opcode.STORE_VAR ident ])
          (List.rev identifier) expr_codes
        |> List.concat
      in
      List.concat expr_codes @ store_codes
  | Ast.Expr.ImportExpr { module_name } ->
      let mod_name, field_name =
        match List.rev module_name with
        | field :: rest -> (String.concat "." (List.rev rest), field)
        | [] -> failwith "Invalid module path"
      in
      [
        Opcode.LOAD_MODULE mod_name;
        Opcode.LOAD_FIELD field_name;
        Opcode.STORE_VAR field_name;
      ]
  | Ast.Expr.ModuleExpr _ -> failwith "Modules not implemented."
  | Ast.Expr.EnumExpr { name; members } ->
      let numbered_members = List.mapi (fun i m -> (m, i)) members in
      Hashtbl.replace enum_tbl name numbered_members;
      []
  | Ast.Expr.LambdaExpr { parameters; body } ->
      let func_name = gensym "lambda" in
      let param_stores =
        List.rev_map
          (fun (param : Ast.Stmt.parameter) ->
            Opcode.STORE_VAR param.Ast.Stmt.name)
          parameters
      in
      let start_bytecode = [ Opcode.FUNCTION func_name ] @ param_stores in
      let function_body_code = compile_expr body in
      let full_function_bytecode =
        start_bytecode @ function_body_code @ [ Opcode.RETURN ]
      in
      Hashtbl.replace function_table func_name
        {
          return_type = Ast.Type.Infer;
          params = parameters;
          bytecode = full_function_bytecode;
          body_code = Array.of_list full_function_bytecode;
        };
      [ Opcode.CLOSURE func_name ]
  | _ -> failwith "Not Supported"
