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

let rec ir_compile_stmt = function
  | Ir.IR_Expr expr -> ir_compile_expr expr
  | Ir.IR_Block instrs -> List.flatten (List.map ir_compile_stmt instrs)
  | Ir.IR_FuncDecl { name; is_rec = _; parameters; return_type; body } ->
      let param_stores =
        List.rev_map
          (fun (param : Ast.Stmt.parameter) ->
            Opcode.STORE_VAR param.Ast.Stmt.name)
          parameters
      in
      let start_bytecode = [ Opcode.FUNCTION name ] @ param_stores in
      let body_bytecode = ir_compile_stmt (Ir.IR_Block body) in
      let full_bytecode = start_bytecode @ body_bytecode @ [ Opcode.RETURN ] in
      Hashtbl.replace function_table name
        {
          return_type;
          params = parameters;
          bytecode = full_bytecode;
          body_code = Array.of_list full_bytecode;
        };
      []
  | _ -> failwith "Not Supported"

and ir_compile_expr = function
  | Ir.IR_Int i -> [ Opcode.LOAD_INT i ]
  | Ir.IR_Float f -> [ Opcode.LOAD_FLOAT f ]
  | Ir.IR_String s -> [ Opcode.LOAD_STRING s ]
  | Ir.IR_Byte c -> [ Opcode.LOAD_BYTE c ]
  | Ir.IR_Unit -> [ Opcode.LOAD_UNIT () ]
  | Ir.IR_Bool b -> [ Opcode.LOAD_BOOL b ]
  | Ir.IR_Tuple elems ->
      let elems_code = List.concat_map ir_compile_expr elems in
      elems_code @ [ Opcode.LOAD_TUPLE (List.length elems) ]
  | Ir.IR_Var name -> [ Opcode.LOAD_VAR name ]
  | Ir.IR_Index (arr, idx) ->
      ir_compile_expr arr @ ir_compile_expr idx @ [ Opcode.LOAD_INDEX ]
  | Ir.IR_Binary (lhs, op, rhs) ->
      let is_assign_op = function
        | Token.PlusAssign | Token.MinusAssign | Token.StarAssign
        | Token.SlashAssign | Token.BitWiseANDAssign | Token.BitWiseORAssign
        | Token.BitWiseXORAssign | Token.LeftShiftAssign
        | Token.RightShiftAssign ->
            true
        | _ -> false
      in
      if is_assign_op op then
        match lhs with
        | Ir.IR_Var name ->
            let load_var_ref_code = [ Opcode.LOAD_VAR_REF name ] in
            let rhs_code = ir_compile_expr rhs in
            load_var_ref_code @ rhs_code @ [ opcode_of_binop op ]
        | _ -> failwith "Assignment target must be a variable"
      else
        let left_code = ir_compile_expr lhs in
        let right_code = ir_compile_expr rhs in
        left_code @ right_code @ [ opcode_of_binop op ]
  | Ir.IR_Call (callee, args) -> (
      let args_code = List.concat_map ir_compile_expr args in
      match callee with
      | Ir.IR_Var name -> (
          match List.assoc_opt name builtins with
          | Some handler -> handler args_code
          | None -> args_code @ [ Opcode.CALL name ])
      | _ ->
          let closure_code = ir_compile_expr callee in
          args_code @ closure_code @ [ Opcode.CALL_CLOSURE (List.length args) ])
  | Ir.IR_Array elems ->
      let elems_code = List.concat_map ir_compile_expr elems in
      elems_code @ [ Opcode.LOAD_ARRAY (List.length elems) ]
  | Ir.IR_Unary (op, e) -> (
      let code = ir_compile_expr e in
      match op with
      | Token.Not -> code @ [ Opcode.NOT ]
      | Token.Minus -> code @ [ Opcode.NEG ]
      | Token.Inc -> (
          match e with
          | Ir.IR_Var name ->
              [
                Opcode.LOAD_VAR name;
                Opcode.INC;
                Opcode.DUP;
                Opcode.STORE_VAR name;
              ]
          | _ -> failwith "INC expects variable")
      | Token.Dec -> (
          match e with
          | Ir.IR_Var name ->
              [
                Opcode.LOAD_VAR name;
                Opcode.DEC;
                Opcode.DUP;
                Opcode.STORE_VAR name;
              ]
          | _ -> failwith "DEC expects variable")
      | Token.BitWiseNOT -> code @ [ Opcode.BITWISENOT ]
      | _ -> failwith "Unsupported unary op")
  | Ir.IR_If (cond, thn, Some els) ->
      let cond_code = ir_compile_expr cond in
      let thn_code = ir_compile_expr thn in
      let els_code = ir_compile_expr els in
      let then_jump = List.length thn_code + 2 in
      let else_jump = List.length els_code + 1 in
      cond_code
      @ [ Opcode.JUMP_IF_FALSE then_jump ]
      @ thn_code @ [ Opcode.JUMP else_jump ] @ els_code
  | Ir.IR_If (cond, thn, None) ->
      let cond_code = ir_compile_expr cond in
      let thn_code = ir_compile_expr thn in
      let then_jump = List.length thn_code + 1 in
      cond_code @ [ Opcode.JUMP_IF_FALSE then_jump ] @ thn_code
  | Ir.IR_Ternary (cond, thn, els) ->
      let cond_code = ir_compile_expr cond in
      let thn_code = ir_compile_expr thn in
      let els_code = ir_compile_expr els in
      let then_jump = List.length thn_code + 2 in
      let else_jump = List.length els_code + 1 in
      cond_code
      @ [ Opcode.JUMP_IF_FALSE then_jump ]
      @ thn_code @ [ Opcode.JUMP else_jump ] @ els_code
  | Ir.IR_Pipeline (lhs, rhs) -> (
      let lhs_code = ir_compile_expr lhs in
      match rhs with
      | Ir.IR_Var name -> (
          match List.assoc_opt name builtins with
          | Some handler -> handler lhs_code
          | None ->
              if Hashtbl.mem function_table name then
                lhs_code @ [ Opcode.CALL name ]
              else
                lhs_code @ [ Opcode.LOAD_VAR name ] @ [ Opcode.CALL_CLOSURE 1 ])
      | Ir.IR_Unary (_, _) | Ir.IR_Lambda _ | Ir.IR_Call _ ->
          let rhs_code = ir_compile_expr rhs in
          lhs_code @ rhs_code @ [ Opcode.CALL_CLOSURE 1 ]
      | _ ->
          failwith
            "Right-hand side of pipeline must be a function name, lambda, or \
             callable expression")
  | Ir.IR_Range (start_opt, end_opt) -> (
      match (start_opt, end_opt) with
      | Some (Ir.IR_Int s), Some (Ir.IR_Int e) ->
          let count = Bigint.to_int (Bigint.add (Bigint.sub e s) Bigint.one) in
          let elems =
            List.init count (fun i ->
                Opcode.LOAD_INT (Bigint.add s (Bigint.of_int i)))
          in
          elems @ [ Opcode.LOAD_ARRAY count ]
      | Some (Ir.IR_Int s), None -> [ Opcode.LOAD_INT s; Opcode.MAKE_RANGE ]
      | None, Some (Ir.IR_Int e) ->
          let count = Bigint.to_int (Bigint.add e Bigint.one) in
          let elems =
            List.init count (fun i -> Opcode.LOAD_INT (Bigint.of_int i))
          in
          elems @ [ Opcode.LOAD_ARRAY count ]
      | _ -> failwith "Range bounds must be integer literals for now")
  | Ir.IR_Slice (arr, start_opt, end_opt) ->
      let arr_code = ir_compile_expr arr in
      let start_code =
        match start_opt with
        | Some s -> ir_compile_expr s
        | None -> [ Opcode.LOAD_UNIT () ]
      in
      let end_code =
        match end_opt with
        | Some e -> ir_compile_expr e
        | None -> [ Opcode.LOAD_UNIT () ]
      in
      arr_code @ start_code @ end_code @ [ Opcode.SLICE ]
  | Ir.IR_Enum { name; members } ->
      Hashtbl.replace enum_tbl name members;
      []
  | Ir.IR_BlockExpr instrs -> ir_compile_stmt (Ir.IR_Block instrs)
  | Ir.IR_VarDecl var ->
      let code =
        match var.Ir.value with
        | Some e -> ir_compile_expr e
        | None -> [ Opcode.LOAD_INT Bigint.zero ]
      in
      code @ [ Opcode.STORE_VAR var.Ir.name ]
  | Ir.IR_MultiVarDecl { names; value; typ = _ } ->
      let flatten_value v =
        match v with
        | Ir.IR_Tuple elems -> elems
        | _ when List.length names > 1 ->
            List.init (List.length names) (fun i ->
                Ir.IR_Index (v, Ir.IR_Int (Bigint.of_int i)))
        | _ -> [ v ]
      in
      let values = flatten_value value in

      if List.length names <> List.length values then
        failwith
          ("IR_MultiVarDecl: number of identifiers ("
          ^ string_of_int (List.length names)
          ^ ") does not match number of assigned values ("
          ^ string_of_int (List.length values)
          ^ ")");

      let expr_codes = List.map ir_compile_expr values in
      let store_codes =
        List.map2
          (fun name _ -> [ Opcode.STORE_VAR name ])
          (List.rev names) expr_codes
        |> List.concat
      in
      List.concat expr_codes @ store_codes
  | Ir.IR_Lambda { parameters; body } ->
      let func_name = gensym "lambda" in
      let param_stores =
        List.map
          (fun (param : Ast.Stmt.parameter) ->
            Opcode.STORE_VAR param.Ast.Stmt.name)
          parameters
      in
      let body_code = List.concat_map ir_compile_stmt body in
      let full_function_bytecode =
        [ Opcode.FUNCTION func_name ]
        @ param_stores @ body_code @ [ Opcode.RETURN ]
      in
      Hashtbl.replace function_table func_name
        {
          return_type = Ast.Type.Infer;
          params = parameters;
          bytecode = full_function_bytecode;
          body_code = Array.of_list full_function_bytecode;
        };
      [ Opcode.CLOSURE func_name ]
  | Ir.IR_Dot (lhs, field) -> (
      match lhs with
      | Ir.IR_Var struct_name -> (
          match Hashtbl.find_opt struct_tbl struct_name with
          | Some fields -> (
              match List.assoc_opt field fields with
              | Some value_expr -> ir_compile_expr value_expr
              | None ->
                  failwith
                    ("Unknown field `" ^ field ^ "` in struct `" ^ struct_name
                   ^ "`"))
          | None -> (
              match Hashtbl.find_opt enum_tbl struct_name with
              | Some members -> (
                  match List.assoc_opt field members with
                  | Some value -> [ Opcode.LOAD_INT (Bigint.of_int value) ]
                  | None ->
                      failwith
                        ("Unknown enum member `" ^ field ^ "` in enum `"
                       ^ struct_name ^ "`"))
              | None -> failwith ("Unknown type `" ^ struct_name ^ "`")))
      | _ -> failwith "DotExpr left must be a struct or enum name")
  | Ir.IR_Match (match_expr, cases) ->
      let match_code = ir_compile_expr match_expr @ [ Opcode.DUP ] in
      let compiled_cases = ref [] in
      let jump_placeholders = ref [] in

      List.iter
        (fun (case_expr_opt, body) ->
          let body_code = List.concat_map ir_compile_stmt body in
          let body_len = List.length body_code in
          let jump_to_next_case = body_len + 2 in
          match case_expr_opt with
          | Some case_expr ->
              let cmp_code =
                [ Opcode.DUP ] @ ir_compile_expr case_expr
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
  | Ir.IR_Import { module_name } ->
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
  | _ -> failwith "Not Supported"
