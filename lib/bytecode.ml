open Opcode
open Token
open Ast.Expr
open Ast.Stmt

type function_info = {
  return_type : Ast.Type.t;
  params : Ast.Stmt.parameter list;
  bytecode : opcode list;
}

let function_table : (string, function_info) Hashtbl.t = Hashtbl.create 10
let enum_tbl : (string, (string * int) list) Hashtbl.t = Hashtbl.create 10

let struct_tbl : (string, (string * Ast.Expr.t) list) Hashtbl.t =
  Hashtbl.create 10

let builtins =
  [
    ("print", fun args -> args @ [ PRINT ]);
    ("to_float", fun args -> args @ [ FLOAT ]);
    ("to_int", fun args -> args @ [ INT ]);
    ("to_string", fun args -> args @ [ STRING ]);
    ("to_bytes", fun args -> args @ [ BYTES ]);
    ("to_byte", fun args -> args @ [ BYTE ]);
    ("length", fun args -> args @ [ LENGTH ]);
    ("input", fun args -> args @ [ INPUT ]);
    ("assert", fun args -> args @ [ ASSERT ]);
    ("panic", fun args -> args @ [ PANIC ]);
  ]

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
  | PlusAssign -> PLUSASSIGN
  | MinusAssign -> MINUSASSIGN
  | StarAssign -> STARASSIGN
  | SlashAssign -> SLASHASSIGN
  | BitWiseAND -> BITWISEAND
  | BitWiseOR -> BITWISEOR
  | BitWiseXOR -> BITWISEXOR
  | LeftShift -> LEFTSHIFT
  | RightShift -> RIGHTSHIFT
  | BitWiseANDAssign -> BITWISEANDASSIGN
  | BitWiseORAssign -> BITWISEORASSIGN
  | BitWiseXORAssign -> BITWISEXORASSIGN
  | LeftShiftAssign -> LEFTSHIFTASSIGN
  | RightShiftAssign -> RIGHTSHIFTASSIGN
  | ArrConcat -> ARRAYCONCAT
  | _ -> failwith "Unsupported operator"

let rec compile_stmt = function
  | ExprStmt expr -> compile_expr expr
  | BlockStmt { body } -> List.flatten (List.map compile_stmt body)
  | FunctionDeclStmt { name; is_rec = _; parameters; body; return_type } ->
      let param_stores =
        List.rev_map
          (fun (param : parameter) -> STORE_VAR param.name)
          parameters
      in
      let start_bytecode = [ FUNCTION name ] @ param_stores in
      let function_body = compile_stmt (BlockStmt { body }) in
      let full_function_bytecode =
        start_bytecode @ function_body @ [ RETURN ]
      in
      Hashtbl.replace function_table name
        { return_type; params = parameters; bytecode = full_function_bytecode };
      []
  | VarDeclarationStmt { identifier; assigned_value; explicit_type = _ } ->
      let expr_bytecode =
        match assigned_value with
        | Some expr -> compile_expr expr
        | None -> [ LOAD_INT Z.zero ]
      in
      expr_bytecode @ [ STORE_VAR identifier ]
  | MultiVarDeclarationStmt { identifier; assigned_value; _ } ->
      let rec flatten_expr expr =
        match expr with
        | TupleExpr elements -> List.concat_map flatten_expr elements
        | _ -> [ expr ]
      in
      let index_exprs expr =
        List.init (List.length identifier) (fun i ->
            IndexExpr { array = expr; index = IntExpr { value = Z.of_int i } })
      in
      let values =
        match assigned_value with
        | [ (TupleExpr _ as t) ] -> flatten_expr t
        | [ expr ] when List.length identifier > 1 -> index_exprs expr
        | _ -> assigned_value
      in
      if List.length identifier <> List.length values then
        failwith
          ("MultiVarDeclarationStmt: number of identifiers ("
          ^ string_of_int (List.length identifier)
          ^ ") does not match number of assigned values ("
          ^ string_of_int (List.length values)
          ^ ")");
      let expr_codes = List.map compile_expr values in
      let store_codes =
        List.map2
          (fun ident _ -> [ STORE_VAR ident ])
          (List.rev identifier) expr_codes
        |> List.concat
      in
      List.concat expr_codes @ store_codes
  | ImportStmt { module_name } ->
      let mod_name, field_name =
        match List.rev module_name with
        | field :: rest -> (String.concat "." (List.rev rest), field)
        | [] -> failwith "Invalid module path"
      in
      [ LOAD_MODULE mod_name; LOAD_FIELD field_name; STORE_VAR field_name ]
  | ModuleStmt _ -> failwith "Modules not implemented."
  | EnumStmt { name; members } ->
      let numbered_members = List.mapi (fun i m -> (m, i)) members in
      Hashtbl.replace enum_tbl name numbered_members;
      []
  | StructStmt { name; fields } ->
      let rec register_struct prefix fields =
        let full_name = String.concat "." prefix in
        let actual_fields =
          List.fold_left
            (fun acc stmt ->
              match stmt with
              | VarDeclarationStmt { identifier; assigned_value = Some v; _ } ->
                  (identifier, v) :: acc
              | StructStmt { name = nested_name; fields = nested_fields } ->
                  let nested_full = prefix @ [ nested_name ] in
                  register_struct nested_full nested_fields;
                  acc
              | _ -> failwith ("Invalid field in struct `" ^ full_name ^ "`"))
            [] fields
        in
        Hashtbl.replace struct_tbl full_name (List.rev actual_fields)
      in
      register_struct [ name ] fields;
      []

and compile_expr = function
  | IntExpr { value } -> [ LOAD_INT value ]
  | FloatExpr { value } -> [ LOAD_FLOAT value ]
  | StringExpr { value } -> [ LOAD_STRING value ]
  | ByteExpr { value } -> [ LOAD_BYTE value ]
  | UnitExpr { value } -> [ LOAD_UNIT value ]
  | BoolExpr { value } ->
      if value then [ LOAD_BOOL true ] else [ LOAD_BOOL false ]
  | TupleExpr elements ->
      let compiled_elements = List.concat_map compile_expr elements in
      compiled_elements @ [ LOAD_TUPLE (List.length elements) ]
  | VarExpr name -> [ LOAD_VAR name ]
  | IndexExpr { array; index } ->
      compile_expr array @ compile_expr index @ [ LOAD_INDEX ]
  | BinaryExpr { left; operator; right } -> (
      match operator with
      | PlusAssign | MinusAssign | StarAssign | SlashAssign | BitWiseANDAssign
      | BitWiseORAssign | BitWiseXORAssign | LeftShiftAssign | RightShiftAssign
        -> (
          match left with
          | VarExpr name ->
              let load_var_ref_code = [ LOAD_VAR_REF name ] in
              let rhs_code = compile_expr right in
              load_var_ref_code @ rhs_code @ [ opcode_of_binop operator ]
          | _ -> failwith "Assignment target must be a variable")
      | _ ->
          let left_code = compile_expr left in
          let right_code = compile_expr right in
          left_code @ right_code @ [ opcode_of_binop operator ])
  | CallExpr { callee; arguments } -> (
      let args_bytecode = List.concat (List.map compile_expr arguments) in
      match callee with
      | VarExpr name -> (
          match List.assoc_opt name builtins with
          | Some handler -> handler args_bytecode
          | None -> args_bytecode @ [ CALL name ])
      | _ -> failwith "Unsupported call expression")
  | ArrayExpr { elements } ->
      let elements_bytecode = List.concat (List.map compile_expr elements) in
      elements_bytecode @ [ LOAD_ARRAY (List.length elements) ]
  | UnaryExpr { operator; operand } -> (
      let operand = compile_expr operand in
      match operator with
      | Not -> operand @ [ NOT ]
      | Inc -> (
          match operand with
          | [ LOAD_VAR name ] -> [ LOAD_VAR name; INC; DUP; STORE_VAR name ]
          | _ -> failwith "INC expects a variable")
      | Dec -> (
          match operand with
          | [ LOAD_VAR name ] -> [ LOAD_VAR name; DEC; DUP; STORE_VAR name ]
          | _ -> failwith "DEC expects a variable")
      | Minus -> operand @ [ NEG ]
      | BitWiseNOT -> operand @ [ BITWISENOT ]
      | _ -> failwith "Unsupported unary operator")
  | IfExpr { condition; then_branch; else_branch } ->
      let condition_code = compile_expr condition in
      let then_code = compile_expr then_branch in
      let else_code =
        match else_branch with Some expr -> compile_expr expr | None -> []
      in
      let then_jump = List.length then_code + 2 in
      let else_jump = List.length else_code + 1 in
      condition_code
      @ [ JUMP_IF_FALSE then_jump ]
      @ then_code @ [ JUMP else_jump ] @ else_code
  | TernaryExpr { cond; onTrue; onFalse } ->
      let cond_code = compile_expr cond in
      let true_code = compile_expr onTrue in
      let false_code = compile_expr onFalse in
      let true_jump = List.length true_code + 2 in
      let false_jump = List.length false_code + 1 in
      cond_code
      @ [ JUMP_IF_FALSE true_jump ]
      @ true_code @ [ JUMP false_jump ] @ false_code
  | PipelineExpr { left; right } -> (
      let left_code = compile_expr left in
      match right with
      | VarExpr name -> (
          match List.assoc_opt name builtins with
          | Some handler -> handler left_code
          | None -> left_code @ [ CALL name ])
      | _ -> failwith "Right-hand side of pipeline must be a function name")
  | DotExpr { left; right } -> (
      match left with
      | VarExpr struct_name -> (
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
                  | Some value -> [ LOAD_INT (Z.of_int value) ]
                  | None ->
                      failwith
                        ("Unknown enum member `" ^ right ^ "` in enum `"
                       ^ struct_name ^ "`"))
              | None -> failwith ("Unknown type `" ^ struct_name ^ "`")))
      | _ -> failwith "DotExpr left must be a struct or enum name")
  | MatchExpr { expr; cases } ->
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
  | RangeExpr { start; end_ } -> (
      match (start, end_) with
      | Some (IntExpr { value = s }), Some (IntExpr { value = e }) ->
          let count = Z.to_int (Z.add (Z.sub e s) Z.one) in
          let range_elements =
            List.init count (fun i -> LOAD_INT (Z.add s (Z.of_int i)))
          in
          range_elements @ [ LOAD_ARRAY count ]
      | Some (IntExpr { value = s }), None -> [ LOAD_INT s; MAKE_RANGE ]
      | None, Some (IntExpr { value = e }) ->
          let count = Z.to_int (Z.add e Z.one) in
          let range_elements =
            List.init count (fun i -> LOAD_INT (Z.of_int i))
          in
          range_elements @ [ LOAD_ARRAY count ]
      | _ -> failwith "Range bounds must be integer literals for now")
  | BlockExpr { body } -> List.flatten (List.map compile_stmt body)
  | SliceExpr { array; start; end_ } ->
      let array_code = compile_expr array in
      let start_code =
        match start with Some s -> compile_expr s | None -> [ LOAD_UNIT () ]
      in
      let end_code =
        match end_ with Some e -> compile_expr e | None -> [ LOAD_UNIT () ]
      in
      array_code @ start_code @ end_code @ [ SLICE ]
