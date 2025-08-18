exception TypeError of string
exception UnifyError of string

let fresh_var_counter = ref 0

let fresh_tyvar () =
  let id = !fresh_var_counter in
  incr fresh_var_counter;
  Ast.Type.VarType id

let rec occurs_check s id ty =
  let ty = Subst.apply s ty in
  match ty with
  | Ast.Type.VarType v -> v = id
  | Ast.Type.ArrayType { element_type } -> occurs_check s id element_type
  | Ast.Type.TupleType ts -> List.exists (occurs_check s id) ts
  | Ast.Type.FunctionType (params, ret) ->
      List.exists (occurs_check s id) params || occurs_check s id ret
  | Ast.Type.RecordType fields ->
      List.exists (fun (_, t) -> occurs_check s id t) fields
  | _ -> false

let enum_variants = Hashtbl.create 10
let stdlib_used = ref false

let rec string_of_type = function
  | Ast.Type.Any -> "any"
  | Ast.Type.Infer -> "infer"
  | Ast.Type.VarType id -> "'" ^ string_of_int id
  | Ast.Type.SymbolType { value } -> value
  | Ast.Type.ArrayType { element_type } ->
      "[" ^ string_of_type element_type ^ "]"
  | Ast.Type.TupleType types ->
      "(" ^ String.concat ", " (List.map string_of_type types) ^ ")"
  | Ast.Type.FunctionType (params, ret) ->
      let params_str = String.concat " * " (List.map string_of_type params) in
      Printf.sprintf "(%s -> %s)" params_str (string_of_type ret)
  | Ast.Type.RecordType fields ->
      let field_strs =
        List.map (fun (name, ty) -> name ^ ": " ^ string_of_type ty) fields
      in
      "record { " ^ String.concat "; " field_strs ^ " }"
  | _ -> failwith "Unknown"

let bind_var subst var_id ty =
  let ty = Subst.apply subst ty in
  if ty = Ast.Type.VarType var_id then ()
  else if occurs_check subst var_id ty then
    raise
      (UnifyError
         ("Occurs check failed: cannot construct infinite type for "
         ^ string_of_type (Ast.Type.VarType var_id)))
  else Subst.add subst var_id ty

let rec unify_with_subst subst t1 t2 =
  let t1 = Subst.apply subst t1 in
  let t2 = Subst.apply subst t2 in
  match (t1, t2) with
  | Ast.Type.Any, _ | _, Ast.Type.Any -> ()
  | Ast.Type.VarType id, t | t, Ast.Type.VarType id -> bind_var subst id t
  | Ast.Type.SymbolType { value = v1 }, Ast.Type.SymbolType { value = v2 }
    when v1 = v2 ->
      ()
  | ( Ast.Type.ArrayType { element_type = e1 },
      Ast.Type.ArrayType { element_type = e2 } ) ->
      unify_with_subst subst e1 e2
  | Ast.Type.TupleType xs, Ast.Type.TupleType ys
    when List.length xs = List.length ys ->
      List.iter2 (unify_with_subst subst) xs ys
  | Ast.Type.FunctionType (ps1, r1), Ast.Type.FunctionType (ps2, r2)
    when List.length ps1 = List.length ps2 ->
      List.iter2 (unify_with_subst subst) ps1 ps2;
      unify_with_subst subst r1 r2
  | Ast.Type.RecordType fs1, Ast.Type.RecordType fs2 ->
      let names1 = List.map fst fs1 in
      let names2 = List.map fst fs2 in
      if List.sort_uniq compare names1 <> List.sort_uniq compare names2 then
        raise (UnifyError "Cannot unify record types with different fields")
      else
        List.iter
          (fun name ->
            let t1 = List.assoc name fs1 in
            let t2 = List.assoc name fs2 in
            unify_with_subst subst t1 t2)
          names1
  | _, Ast.Type.Infer -> ()
  | Ast.Type.SymbolType { value = "*" }, _ -> ()
  | _, Ast.Type.SymbolType { value = "*" } -> ()
  | _ ->
      raise
        (UnifyError
           ("Cannot unify " ^ string_of_type t1 ^ " with " ^ string_of_type t2))

let unify t1 t2 =
  let s = Subst.empty () in
  try
    let s' = unify_with_subst s t1 t2 in
    s'
  with UnifyError msg -> raise (TypeError ("Unification error: " ^ msg))

let rec ftv_type = function
  | Ast.Type.VarType id -> [ id ]
  | Ast.Type.ArrayType { element_type } -> ftv_type element_type
  | Ast.Type.TupleType ts -> List.concat_map ftv_type ts
  | Ast.Type.FunctionType (ps, r) -> List.concat_map ftv_type (r :: ps)
  | Ast.Type.RecordType fs -> List.concat_map (fun (_, t) -> ftv_type t) fs
  | _ -> []

let ftv_env env =
  let vars = List.concat_map (fun (_, t) -> ftv_type t) env in
  List.sort_uniq compare vars

let generalize env t =
  let ftv_t = ftv_type t in
  let ftv_e = ftv_env env in
  let quant = List.filter (fun v -> not (List.mem v ftv_e)) ftv_t in
  (quant, t)

let instantiate_scheme (quantified_vars, tp) =
  let map_tbl = Hashtbl.create (List.length quantified_vars) in
  let fresh_for id =
    try Hashtbl.find map_tbl id
    with Not_found ->
      let v = fresh_tyvar () in
      Hashtbl.add map_tbl id v;
      v
  in
  let rec inst t =
    match t with
    | Ast.Type.VarType id when List.mem id quantified_vars -> fresh_for id
    | Ast.Type.VarType _ -> t
    | Ast.Type.ArrayType { element_type } ->
        Ast.Type.ArrayType { element_type = inst element_type }
    | Ast.Type.TupleType ts -> Ast.Type.TupleType (List.map inst ts)
    | Ast.Type.FunctionType (ps, r) ->
        Ast.Type.FunctionType (List.map inst ps, inst r)
    | Ast.Type.RecordType fs ->
        Ast.Type.RecordType (List.map (fun (n, t) -> (n, inst t)) fs)
    | other -> other
  in
  inst tp

let rec can_compare t1 t2 =
  match (t1, t2) with
  | Ast.Type.Any, _ -> true
  | _, Ast.Type.Any -> true
  | Ast.Type.SymbolType { value = v1 }, Ast.Type.SymbolType { value = v2 } ->
      v1 = v2
  | Ast.Type.TupleType ts1, Ast.Type.TupleType ts2 ->
      List.length ts1 = List.length ts2 && List.for_all2 can_compare ts1 ts2
  | ( Ast.Type.ArrayType { element_type = et1 },
      Ast.Type.ArrayType { element_type = et2 } ) ->
      can_compare et1 et2
  | Ast.Type.VarType _, _ | _, Ast.Type.VarType _ -> true
  | Ast.Type.SymbolType { value = "*" }, _ -> true
  | _, Ast.Type.SymbolType { value = "*" } -> true
  | _ -> false

let rec fold_left_map2 f env idents exprs =
  match (idents, exprs) with
  | [], [] -> (env, [])
  | ident :: id_rest, expr :: expr_rest ->
      let env', inferred = f env ident expr in
      let env'', rest = fold_left_map2 f env' id_rest expr_rest in
      (env'', inferred :: rest)
  | _ -> failwith "fold_left_map2: lists have different lengths"

let built_in_modules =
  [
    ( "Math",
      [
        ("pi", Ast.Type.SymbolType { value = "float" });
        ("e", Ast.Type.SymbolType { value = "float" });
        ("tau", Ast.Type.SymbolType { value = "float" });
        ("nan", Ast.Type.SymbolType { value = "float" });
        ("inf", Ast.Type.SymbolType { value = "float" });
        ("neg_inf", Ast.Type.SymbolType { value = "float" });
        ( "sin",
          Ast.Type.FunctionType
            ( [ Ast.Type.SymbolType { value = "float" } ],
              Ast.Type.SymbolType { value = "float" } ) );
      ] );
  ]

let builtins =
  let mk_builtin name overloads =
    let schemes =
      List.map
        (fun (params, ret) ->
          let func_type = Ast.Type.FunctionType (params, ret) in
          generalize [] func_type)
        overloads
    in
    (name, schemes)
  in
  [
    mk_builtin "print"
      [ ([ Ast.Type.Any ], Ast.Type.SymbolType { value = "unit" }) ];
    mk_builtin "input"
      [
        ( [ Ast.Type.SymbolType { value = "string" } ],
          Ast.Type.SymbolType { value = "unit" } );
      ];
    mk_builtin "to_bytes"
      [
        ( [ Ast.Type.SymbolType { value = "string" } ],
          Ast.Type.ArrayType
            { element_type = Ast.Type.SymbolType { value = "byte" } } );
      ];
    mk_builtin "to_float"
      [
        ( [ Ast.Type.SymbolType { value = "*" } ],
          Ast.Type.SymbolType { value = "float" } );
        ( [ Ast.Type.SymbolType { value = "string" } ],
          Ast.Type.SymbolType { value = "float" } );
        ( [ Ast.Type.SymbolType { value = "int" } ],
          Ast.Type.SymbolType { value = "float" } );
      ];
    mk_builtin "to_int"
      [
        ( [ Ast.Type.SymbolType { value = "*" } ],
          Ast.Type.SymbolType { value = "int" } );
        ( [ Ast.Type.SymbolType { value = "byte" } ],
          Ast.Type.SymbolType { value = "int" } );
        ( [ Ast.Type.SymbolType { value = "string" } ],
          Ast.Type.SymbolType { value = "int" } );
        ( [ Ast.Type.SymbolType { value = "float" } ],
          Ast.Type.SymbolType { value = "int" } );
      ];
    mk_builtin "length"
      [ ([ Ast.Type.Any ], Ast.Type.SymbolType { value = "int" }) ];
    mk_builtin "to_string"
      [ ([ Ast.Type.Any ], Ast.Type.SymbolType { value = "string" }) ];
    mk_builtin "assert"
      [
        ( [ Ast.Type.SymbolType { value = "bool" } ],
          Ast.Type.SymbolType { value = "unit" } );
      ];
    mk_builtin "panic"
      [
        ( [ Ast.Type.SymbolType { value = "string" } ],
          Ast.Type.SymbolType { value = "unit" } );
      ];
    mk_builtin "to_byte"
      [
        ( [
            Ast.Type.ArrayType
              { element_type = Ast.Type.SymbolType { value = "int" } };
          ],
          Ast.Type.SymbolType { value = "byte" } );
        ( [ Ast.Type.SymbolType { value = "int" } ],
          Ast.Type.SymbolType { value = "byte" } );
        ( [ Ast.Type.SymbolType { value = "string" } ],
          Ast.Type.SymbolType { value = "byte" } );
      ];
    mk_builtin "of_type"
      [ ([ Ast.Type.Any ], Ast.Type.SymbolType { value = "string" }) ];
    mk_builtin "head"
      [ ([ Ast.Type.ArrayType { element_type = Ast.Type.Any } ], Ast.Type.Any) ];
    mk_builtin "tail"
      [ ([ Ast.Type.ArrayType { element_type = Ast.Type.Any } ], Ast.Type.Any) ];
    mk_builtin "reverse"
      [ ([ Ast.Type.ArrayType { element_type = Ast.Type.Any } ], Ast.Type.Any) ];
  ]

let rec type_eq expected actual =
  match (expected, actual) with
  | Ast.Type.SymbolType { value = "*" }, Ast.Type.SymbolType _ -> true
  | Ast.Type.SymbolType { value = "unit" }, _ -> true
  | Ast.Type.Any, _ | _, Ast.Type.Any -> true
  | Ast.Type.SymbolType { value = v1 }, Ast.Type.SymbolType { value = v2 } ->
      v1 = v2
  | ( Ast.Type.ArrayType { element_type = e1 },
      Ast.Type.ArrayType { element_type = e2 } ) ->
      type_eq e1 e2
  | Ast.Type.TupleType xs, Ast.Type.TupleType ys ->
      List.length xs = List.length ys && List.for_all2 type_eq xs ys
  | Ast.Type.VarType _, _ | _, Ast.Type.VarType _ -> true
  | Ast.Type.SymbolType { value = "*" }, _ -> true
  | _, Ast.Type.SymbolType { value = "*" } -> true
  | _ -> false

let rec check_stmt env func_env stmt =
  match stmt with
  | Ast.Stmt.ExprStmt expr ->
      let _, env1 = check_expr env func_env expr in
      (env1, func_env)
  | Ast.Stmt.FunctionDeclStmt { name; is_rec; parameters; return_type; body } ->
      let local_funcs = collect_functions body in
      let param_types =
        List.map
          (fun (p : Ast.Stmt.parameter) ->
            match p.Ast.Stmt.param_type with
            | Ast.Type.Infer -> fresh_tyvar ()
            | other -> other)
          parameters
      in
      let declared_return =
        match return_type with
        | Ast.Type.Infer -> fresh_tyvar ()
        | other -> other
      in
      let func_type = Ast.Type.FunctionType (param_types, declared_return) in
      let env_for_generalize = env in
      let placeholder_scheme = ([], func_type) in

      let func_env_with_placeholder =
        let base_env = local_funcs @ func_env in
        if is_rec then
          (name, [ placeholder_scheme ]) :: List.remove_assoc name base_env
        else base_env
      in

      let param_env =
        List.map2
          (fun (p : Ast.Stmt.parameter) ty -> (p.Ast.Stmt.name, ty))
          parameters param_types
      in
      let env_with_params = param_env @ env in

      let final_env, final_func_env =
        List.fold_left
          (fun (e, fe) stmt -> check_stmt e fe stmt)
          (env_with_params, func_env_with_placeholder)
          body
      in

      let rec gather_return_types env func_env stmts =
        List.concat_map
          (function
            | Ast.Stmt.ExprStmt e -> find_return_exprs env func_env e
            | Ast.Stmt.BlockStmt { body } ->
                gather_return_types env func_env body
            | _ -> [])
          stmts
      in

      let return_expr_types =
        gather_return_types final_env final_func_env body
      in
      let return_types_only = List.map fst return_expr_types in

      let last_expr_type =
        match
          List.rev body
          |> List.find_opt (function Ast.Stmt.ExprStmt _ -> true | _ -> false)
        with
        | Some (Ast.Stmt.ExprStmt expr) ->
            Some (check_expr final_env final_func_env expr)
        | _ -> None
      in

      let last_type_only = Option.map fst last_expr_type in
      let all_return_types =
        match last_type_only with
        | Some t -> if return_types_only = [] then [ t ] else return_types_only
        | None -> return_types_only
      in

      List.iter
        (fun actual_type ->
          try ignore (unify declared_return actual_type)
          with TypeError msg ->
            raise
              (TypeError
                 ("Function " ^ name ^ " has mismatched return type:\n"
                ^ "  Expected: "
                 ^ string_of_type declared_return
                 ^ "\n" ^ "  Found:    " ^ string_of_type actual_type ^ "\n"
                 ^ "  All return expressions must match the declared return \
                    type.\n" ^ "  Unify error: " ^ msg)))
        all_return_types;

      let scheme = generalize env_for_generalize func_type in
      let func_env_updated =
        let base_env = local_funcs @ func_env in
        match List.assoc_opt name base_env with
        | Some existing_schemes ->
            (name, scheme :: existing_schemes)
            :: List.remove_assoc name base_env
        | None -> (name, [ scheme ]) :: base_env
      in

      (final_env, func_env_updated)
  | Ast.Stmt.BlockStmt { body } ->
      let final_env, final_func_env =
        List.fold_left
          (fun (e, fe) stmt -> check_stmt e fe stmt)
          (env, func_env) body
      in
      (final_env, final_func_env)
  | Ast.Stmt.RecordStmt { name; fields } ->
      let record_env, record_func_env =
        List.fold_left
          (fun (acc_env, acc_func_env) field_stmt ->
            match field_stmt with
            | Ast.Expr.VarDeclExpr _ ->
                check_stmt acc_env acc_func_env (Ast.Stmt.ExprStmt field_stmt)
            | _ ->
                raise
                  (TypeError
                     ("Invalid statement in record `" ^ name
                    ^ "`: only variable declarations are allowed")))
          ([], func_env) fields
      in
      let fields_as_types =
        List.rev_map (fun (id, ty) -> (id, ty)) record_env
      in
      let record_type = Ast.Type.RecordType fields_as_types in
      ((name, record_type) :: env, record_func_env)
  | _ -> failwith "Not Supported"

and check_expr env func_env expr =
  match expr with
  | Ast.Expr.IntExpr _ -> (Ast.Type.SymbolType { value = "int" }, env)
  | Ast.Expr.FloatExpr _ -> (Ast.Type.SymbolType { value = "float" }, env)
  | Ast.Expr.StringExpr _ -> (Ast.Type.SymbolType { value = "string" }, env)
  | Ast.Expr.BoolExpr _ -> (Ast.Type.SymbolType { value = "bool" }, env)
  | Ast.Expr.ByteExpr _ -> (Ast.Type.SymbolType { value = "byte" }, env)
  | Ast.Expr.UnitExpr _ -> (Ast.Type.SymbolType { value = "unit" }, env)
  | Ast.Expr.TupleExpr elements ->
      let element_types, env' =
        List.fold_right
          (fun elem (types_acc, env_acc) ->
            let ty, env_new = check_expr env_acc func_env elem in
            let ty =
              match ty with Ast.Type.Infer -> fresh_tyvar () | _ -> ty
            in
            (ty :: types_acc, env_new))
          elements ([], env)
      in
      (Ast.Type.TupleType element_types, env')
  | Ast.Expr.VarExpr name -> (
      try
        let ty = List.assoc name env in
        (ty, env)
      with Not_found ->
        raise
          (TypeError
             ("Unbound variable reference:\n" ^ "  Variable `" ^ name
            ^ "` is not in scope.")))
  | Ast.Expr.UnaryExpr { operator; operand } -> (
      let operand_type, env' = check_expr env func_env operand in
      match operator with
      | Token.Not ->
          if not (type_eq operand_type (Ast.Type.SymbolType { value = "bool" }))
          then
            raise
              (TypeError
                 ("Type error in unary `not` expression:\n"
                ^ "  Expected: bool\n" ^ "  Found:    "
                 ^ string_of_type operand_type));
          (Ast.Type.SymbolType { value = "bool" }, env')
      | Token.BitWiseNOT ->
          if not (type_eq operand_type (Ast.Type.SymbolType { value = "int" }))
          then
            raise
              (TypeError
                 ("Type error in unary bitwise NOT expression:\n"
                ^ "  Expected: int\n" ^ "  Found:    "
                 ^ string_of_type operand_type));
          (operand_type, env')
      | Token.Inc | Token.Dec ->
          if
            not
              (type_eq operand_type (Ast.Type.SymbolType { value = "int" })
              || type_eq operand_type (Ast.Type.SymbolType { value = "float" })
              )
          then
            raise
              (TypeError
                 ("Type error in increment/decrement expression:\n"
                ^ "  Expected: int or float\n" ^ "  Found:    "
                 ^ string_of_type operand_type));
          (operand_type, env')
      | Token.Minus ->
          if
            not
              (type_eq operand_type (Ast.Type.SymbolType { value = "int" })
              || type_eq operand_type (Ast.Type.SymbolType { value = "float" })
              )
          then
            raise
              (TypeError
                 ("Type error in unary minus expression:\n"
                ^ "  Expected: int or float\n" ^ "  Found:    "
                 ^ string_of_type operand_type));
          (operand_type, env')
      | _ -> raise (TypeError "Unsupported unary operator in expression"))
  | Ast.Expr.BinaryExpr { left; operator; right } -> (
      let lt, env1 = check_expr env func_env left in
      let rt, env2 = check_expr env1 func_env right in
      match operator with
      | Token.Eq | Token.Neq ->
          let _ = unify lt rt in
          if not (can_compare lt rt) then
            raise
              (TypeError
                 ("Type error in equality expression:\n"
                ^ "  Left operand type:  " ^ string_of_type lt ^ "\n"
                ^ "  Right operand type: " ^ string_of_type rt ^ "\n"
                ^ "  Operands cannot be compared for equality."));
          (Ast.Type.SymbolType { value = "bool" }, env2)
      | Token.Geq | Token.Leq | Token.Less | Token.Greater | Token.LogicalAnd
      | Token.LogicalOr ->
          let _ = unify lt rt in
          (Ast.Type.SymbolType { value = "bool" }, env2)
      | Token.BitWiseAND | Token.BitWiseOR | Token.BitWiseXOR | Token.LeftShift
      | Token.RightShift | Token.BitWiseANDAssign | Token.BitWiseORAssign
      | Token.BitWiseXORAssign | Token.LeftShiftAssign | Token.RightShiftAssign
        ->
          let _ = unify lt (Ast.Type.SymbolType { value = "int" }) in
          (lt, env2)
      | _ ->
          let _ = unify lt rt in
          (lt, env2))
  | Ast.Expr.CallExpr { callee = Ast.Expr.VarExpr name; arguments } -> (
      let env', arg_types =
        List.fold_left_map
          (fun acc_env arg ->
            let t, new_env = check_expr acc_env func_env arg in
            (new_env, t))
          env arguments
      in
      match List.assoc_opt name func_env with
      | Some schemes -> (
          let instantiated_overloads =
            List.map
              (fun scheme ->
                match instantiate_scheme scheme with
                | Ast.Type.FunctionType (params, ret) -> (params, ret)
                | _ ->
                    failwith ("Non-function type stored in func_env for " ^ name))
              schemes
          in
          let matching =
            List.find_opt
              (fun (params, _) ->
                List.length params = List.length arg_types
                &&
                try
                  List.iter2
                    (fun param_ty arg_ty -> ignore (unify param_ty arg_ty))
                    params arg_types;
                  true
                with TypeError _ -> false)
              instantiated_overloads
          in
          match matching with
          | Some (_, return_type) -> (return_type, env')
          | None ->
              raise
                (TypeError
                   ("Function call argument mismatch for `" ^ name ^ "`:\n"
                  ^ "  No matching overload found for arguments:\n" ^ "  "
                   ^ String.concat ", " (List.map string_of_type arg_types))))
      | None -> (
          match List.assoc_opt name env with
          | Some (Ast.Type.FunctionType (param_types, return_type)) ->
              if
                List.length param_types = List.length arg_types
                && List.for_all2 type_eq param_types arg_types
              then (return_type, env')
              else
                raise
                  (TypeError
                     ("Function call argument mismatch for `" ^ name ^ "`:\n"
                    ^ "  Expected: ("
                     ^ String.concat ", " (List.map string_of_type param_types)
                     ^ ")\n" ^ "  Found:    ("
                     ^ String.concat ", " (List.map string_of_type arg_types)
                     ^ ")"))
          | _ ->
              raise
                (TypeError
                   ("Unknown function:\n" ^ "  `" ^ name
                  ^ "` is not declared as a function"))))
  | Ast.Expr.CallExpr _ ->
      raise
        (TypeError
           "Only calls to named functions (e.g. `foo(...)`) are currently \
            supported")
  | Ast.Expr.ArrayExpr { elements } -> (
      let final_env, element_types =
        List.fold_left_map
          (fun acc_env elem ->
            let t, new_env = check_expr acc_env func_env elem in
            (new_env, t))
          env elements
      in
      match element_types with
      | [] -> (Ast.Type.ArrayType { element_type = Ast.Type.Any }, final_env)
      | hd :: tl ->
          List.iter
            (fun t ->
              if not (type_eq t hd) then
                raise
                  (TypeError
                     ("Type error in array expression:\n"
                    ^ "  All elements must have the same type\n"
                    ^ "  Found mismatch: " ^ string_of_type t ^ " vs "
                    ^ string_of_type hd)))
            tl;
          (Ast.Type.ArrayType { element_type = hd }, final_env))
  | Ast.Expr.IndexExpr { array; index } -> (
      let at, env' = check_expr env func_env array in
      let index_type, env'' = check_expr env' func_env index in
      let is_enum_type = function
        | Ast.Type.SymbolType { value } -> Hashtbl.mem enum_variants value
        | _ -> false
      in

      if
        not
          (type_eq index_type (Ast.Type.SymbolType { value = "int" })
          || is_enum_type index_type)
      then
        raise
          (TypeError
             ("Type error in index expression:\n"
            ^ "  Index must be an integer or enum value\n" ^ "  Found: "
            ^ string_of_type index_type));

      match at with
      | Ast.Type.ArrayType { element_type } -> (element_type, env'')
      | Ast.Type.TupleType element_types -> (
          match index with
          | Ast.Expr.IntExpr { value } ->
              let idx = Bigint.to_int value in
              if idx < 0 || idx >= List.length element_types then
                raise
                  (TypeError
                     ("Tuple index out of bounds:\n" ^ "  Index: "
                    ^ string_of_int idx ^ "\n  Tuple size: "
                     ^ string_of_int (List.length element_types)));
              (List.nth element_types idx, env'')
          | _ ->
              raise
                (TypeError
                   ("Invalid tuple index:\n"
                  ^ "  Only constant integer indices (e.g. `t[0]`) are allowed"
                   )))
      | Ast.Type.VarType id -> (
          let subst_tbl = Subst.empty () in
          match index with
          | Ast.Expr.IntExpr { value } ->
              let idx = Bigint.to_int value in
              let fresh_elems = List.init (idx + 1) (fun _ -> fresh_tyvar ()) in
              let tuple_ty = Ast.Type.TupleType fresh_elems in
              unify_with_subst subst_tbl (Ast.Type.VarType id) tuple_ty;
              let element_types =
                List.map (Subst.apply subst_tbl) fresh_elems
              in
              if idx < 0 || idx >= List.length element_types then
                raise
                  (TypeError
                     ("Tuple index out of bounds:\n" ^ "  Index: "
                    ^ string_of_int idx ^ "\n  Tuple size: "
                     ^ string_of_int (List.length element_types)));
              (List.nth element_types idx, env'')
          | _ ->
              let elem_ty = fresh_tyvar () in
              let arr_ty = Ast.Type.ArrayType { element_type = elem_ty } in
              unify_with_subst subst_tbl (Ast.Type.VarType id) arr_ty;
              (Subst.apply subst_tbl elem_ty, env''))
      | _ ->
          raise
            (TypeError
               ("Type error in index expression:\n"
              ^ "  Can only index into arrays or tuples\n" ^ "  Found: "
              ^ string_of_type at)))
  | Ast.Expr.IfExpr { condition; then_branch; else_branch } ->
      let ct, env' = check_expr env func_env condition in
      if not (type_eq ct (Ast.Type.SymbolType { value = "bool" })) then
        raise
          (TypeError
             ("Type error in `if` expression condition:\n"
            ^ "  Expected: bool\n" ^ "  Found:    " ^ string_of_type ct));
      let t_then, env'' = check_expr env' func_env then_branch in
      let t_else, env''' =
        match else_branch with
        | Some e -> check_expr env'' func_env e
        | None -> (t_then, env'')
      in
      if not (type_eq t_then t_else) then
        raise
          (TypeError
             ("Type mismatch in `if` expression branches:\n" ^ "  Then branch: "
            ^ string_of_type t_then ^ "\n" ^ "  Else branch: "
            ^ string_of_type t_else));
      (t_then, env''')
  | Ast.Expr.DotExpr { left; right } -> (
      let left_type, env' = check_expr env func_env left in
      match left with
      | Ast.Expr.VarExpr name -> (
          match List.assoc_opt (name ^ "." ^ right) env' with
          | Some member_type -> (member_type, env')
          | None -> (
              match List.assoc_opt name env' with
              | Some (Ast.Type.RecordType fields) -> (
                  match List.assoc_opt right fields with
                  | Some ty -> (ty, env')
                  | None ->
                      raise
                        (TypeError
                           ("Unknown record field:\n" ^ "  `" ^ right
                          ^ "` is not a field of record `" ^ name ^ "`")))
              | Some _ ->
                  raise
                    (TypeError
                       ("Cannot access `" ^ right ^ "` on non-record value `"
                      ^ name ^ "`"))
              | None ->
                  raise
                    (TypeError
                       ("Unknown enum or record:\n" ^ "  `" ^ name
                      ^ "` is not defined"))))
      | _ -> (
          match left_type with
          | Ast.Type.RecordType fields -> (
              match List.assoc_opt right fields with
              | Some ty -> (ty, env')
              | None ->
                  raise
                    (TypeError
                       ("Unknown record field:\n" ^ "  `" ^ right
                      ^ "` is not a field of the record")))
          | _ ->
              raise
                (TypeError
                   ("Dot access error:\n" ^ "  Cannot access field `" ^ right
                  ^ "` on non-record or non-enum expression"))))
  | Ast.Expr.TernaryExpr { cond; onTrue; onFalse } ->
      let ct, env1 = check_expr env func_env cond in
      if not (type_eq ct (Ast.Type.SymbolType { value = "bool" })) then
        raise
          (TypeError
             ("Type error in ternary condition:\n" ^ "  Expected: bool\n"
            ^ "  Found:    " ^ string_of_type ct));
      let t_true, env2 = check_expr env1 func_env onTrue in
      let t_false, env3 = check_expr env2 func_env onFalse in
      if type_eq t_true t_false then (t_true, env3)
      else
        raise
          (TypeError
             ("Type mismatch in ternary branches:\n" ^ "  True branch:  "
            ^ string_of_type t_true ^ "\n" ^ "  False branch: "
            ^ string_of_type t_false))
  | Ast.Expr.PipelineExpr { left; right } -> (
      let arg_type, env1 = check_expr env func_env left in
      match right with
      | Ast.Expr.VarExpr name -> (
          match List.assoc_opt name func_env with
          | Some schemes -> (
              let instantiated_overloads =
                List.map
                  (fun scheme ->
                    match instantiate_scheme scheme with
                    | Ast.Type.FunctionType (params, ret) -> (params, ret)
                    | _ ->
                        failwith
                          ("Non-function type stored in func_env for " ^ name))
                  schemes
              in
              let matching =
                List.find_opt
                  (fun (params, _) ->
                    match params with
                    | [ param_type ] -> type_eq param_type arg_type
                    | _ -> false)
                  instantiated_overloads
              in
              match matching with
              | Some (_, return_type) -> (return_type, env1)
              | None ->
                  raise
                    (TypeError
                       ("No matching overload for pipeline call to `" ^ name
                      ^ "`:\n" ^ "  Argument type: " ^ string_of_type arg_type))
              )
          | None -> (
              match List.assoc_opt name env with
              | Some (Ast.Type.FunctionType ([ param_type ], return_type)) ->
                  if type_eq param_type arg_type then (return_type, env1)
                  else
                    raise
                      (TypeError
                         ("Function `" ^ name
                        ^ "` called via pipeline expects:\n"
                        ^ "  Parameter type: " ^ string_of_type param_type
                        ^ "\n" ^ "  But got:         " ^ string_of_type arg_type
                         ))
              | Some _ ->
                  raise
                    (TypeError
                       ("Pipeline error:\n" ^ "  `" ^ name
                      ^ "` is not a unary function"))
              | None ->
                  raise
                    (TypeError
                       ("Unknown function in pipeline:\n" ^ "  `" ^ name
                      ^ "` is not declared"))))
      | Ast.Expr.LambdaExpr { parameters; body } -> (
          let param_types = List.map (fun _ -> fresh_tyvar ()) parameters in
          let env_with_params =
            List.fold_left2
              (fun acc_env (param : Ast.Stmt.parameter) ty ->
                (param.Ast.Stmt.name, ty) :: acc_env)
              env parameters param_types
          in
          let body_type, _ = check_expr env_with_params func_env body in
          let _ = Ast.Type.FunctionType (param_types, body_type) in
          match param_types with
          | [ param_type ] ->
              if type_eq param_type arg_type then (body_type, env1)
              else
                raise
                  (TypeError
                     ("Pipeline function parameter type mismatch:\n"
                    ^ "  Expected: " ^ string_of_type param_type
                    ^ "\n  Got:      " ^ string_of_type arg_type))
          | _ ->
              raise
                (TypeError
                   "Pipeline lambda function must have exactly one parameter"))
      | _ ->
          raise
            (TypeError
               ("Invalid pipeline usage:\n"
              ^ "  Right-hand side must be a function identifier (e.g., `value \
                 |> foo`)")))
  | Ast.Expr.MatchExpr { expr; cases } -> (
      let et, env1 = check_expr env func_env expr in
      match cases with
      | [] -> (Ast.Type.SymbolType { value = "unit" }, env1)
      | (pat_opt, case_stmts) :: rest ->
          (match pat_opt with
          | Some pat_expr ->
              let pt, _ = check_expr env func_env pat_expr in
              if not (type_eq et pt) then
                raise
                  (TypeError
                     ("Pattern match type mismatch:\n"
                    ^ "  Match expression type: " ^ string_of_type et ^ "\n"
                    ^ "  Pattern type:          " ^ string_of_type pt))
          | None -> ());
          let final_env, final_func_env =
            List.fold_left
              (fun (e, fe) stmt -> check_stmt e fe stmt)
              (env, func_env) case_stmts
          in
          let first_branch_type =
            match List.rev case_stmts with
            | Ast.Stmt.ExprStmt expr :: _ ->
                fst (check_expr final_env final_func_env expr)
            | _ -> Ast.Type.SymbolType { value = "unit" }
          in

          List.iter
            (fun (pat_opt, case_stmts) ->
              (match pat_opt with
              | Some pat_expr ->
                  let pt, _ = check_expr env func_env pat_expr in
                  if not (type_eq et pt) then
                    raise
                      (TypeError
                         ("Pattern match type mismatch:\n"
                        ^ "  Match expression type: " ^ string_of_type et ^ "\n"
                        ^ "  Pattern type:          " ^ string_of_type pt))
              | None -> ());
              let branch_env, branch_func_env =
                List.fold_left
                  (fun (e, fe) stmt -> check_stmt e fe stmt)
                  (env, func_env) case_stmts
              in
              let branch_type =
                match List.rev case_stmts with
                | Ast.Stmt.ExprStmt expr :: _ ->
                    fst (check_expr branch_env branch_func_env expr)
                | _ -> Ast.Type.SymbolType { value = "unit" }
              in
              if not (type_eq branch_type first_branch_type) then
                raise
                  (TypeError
                     ("Type mismatch in match branches:\n" ^ "  Expected: "
                     ^ string_of_type first_branch_type
                     ^ "\n" ^ "  Found:    " ^ string_of_type branch_type)))
            rest;

          (first_branch_type, env1))
  | Ast.Expr.RangeExpr { start; end_ } -> (
      match (start, end_) with
      | Some s, Some e ->
          let t_s, env1 = check_expr env func_env s in
          let t_e, env2 = check_expr env1 func_env e in
          if
            (not (type_eq t_s (Ast.Type.SymbolType { value = "int" })))
            || not (type_eq t_e (Ast.Type.SymbolType { value = "int" }))
          then
            raise
              (TypeError
                 ("Range expression requires integer bounds:\n" ^ "  Found: "
                ^ string_of_type t_s ^ " and " ^ string_of_type t_e));
          ( Ast.Type.ArrayType
              { element_type = Ast.Type.SymbolType { value = "int" } },
            env2 )
      | Some s, None ->
          let t_s, env1 = check_expr env func_env s in
          if not (type_eq t_s (Ast.Type.SymbolType { value = "int" })) then
            raise
              (TypeError
                 ("Open-ended range `{x..}` requires integer start:\n"
                ^ "  Found: " ^ string_of_type t_s));
          ( Ast.Type.ArrayType
              { element_type = Ast.Type.SymbolType { value = "int" } },
            env1 )
      | None, Some e ->
          let t_e, env1 = check_expr env func_env e in
          if not (type_eq t_e (Ast.Type.SymbolType { value = "int" })) then
            raise
              (TypeError
                 ("Open-start range `{..x}` requires integer end:\n"
                ^ "  Found: " ^ string_of_type t_e));
          Ast.Type.
            ( ArrayType { element_type = Ast.Type.SymbolType { value = "int" } },
              env1 )
      | None, None ->
          raise (TypeError "Invalid range expression: both bounds missing"))
  | Ast.Expr.BlockExpr { body } ->
      let rec check_stmts env func_env stmts =
        match stmts with
        | [] -> (Ast.Type.SymbolType { value = "unit" }, env, func_env)
        | [ last_stmt ] -> (
            let env', func_env' = check_stmt env func_env last_stmt in
            match last_stmt with
            | Ast.Stmt.ExprStmt expr ->
                let t, env'' = check_expr env' func_env' expr in
                (t, env'', func_env')
            | _ -> (Ast.Type.SymbolType { value = "unit" }, env', func_env'))
        | hd :: tl ->
            let env', func_env' = check_stmt env func_env hd in
            check_stmts env' func_env' tl
      in
      let block_type, env_after, _func_env_after =
        check_stmts env func_env body
      in
      (block_type, env_after)
  | Ast.Expr.SliceExpr { array; start; end_ } -> (
      let array_type, env1 = check_expr env func_env array in
      match array_type with
      | Ast.Type.ArrayType _ ->
          let env2 =
            match start with
            | Some s -> (
                let s_ty, env' = check_expr env1 func_env s in
                match s_ty with
                | Ast.Type.SymbolType { value = "int" } -> env'
                | _ -> raise (TypeError "Slice start index must be of type int")
                )
            | None -> env1
          in
          let env3 =
            match end_ with
            | Some e -> (
                let e_ty, env' = check_expr env2 func_env e in
                match e_ty with
                | Ast.Type.SymbolType { value = "int" } -> env'
                | _ -> raise (TypeError "Slice end index must be of type int"))
            | None -> env2
          in
          (array_type, env3)
      | Ast.Type.VarType id ->
          let elem_ty = fresh_tyvar () in
          let arr_ty = Ast.Type.ArrayType { element_type = elem_ty } in
          let subst_tbl = Subst.empty () in
          unify_with_subst subst_tbl (Ast.Type.VarType id) arr_ty;
          let env2 =
            match start with
            | Some s -> (
                let s_ty, env' = check_expr env1 func_env s in
                match s_ty with
                | Ast.Type.SymbolType { value = "int" } -> env'
                | _ -> raise (TypeError "Slice start index must be of type int")
                )
            | None -> env1
          in
          let env3 =
            match end_ with
            | Some e -> (
                let e_ty, env' = check_expr env2 func_env e in
                match e_ty with
                | Ast.Type.SymbolType { value = "int" } -> env'
                | _ -> raise (TypeError "Slice end index must be of type int"))
            | None -> env2
          in
          (Subst.apply subst_tbl arr_ty, env3)
      | _ -> raise (TypeError "Attempted to slice a non-array value"))
  | Ast.Expr.VarDeclExpr { identifier; assigned_value; explicit_type } -> (
      match assigned_value with
      | Some expr ->
          let expr_type, env1 = check_expr env func_env expr in
          let final_type =
            match explicit_type with
            | Ast.Type.Infer -> expr_type
            | _ ->
                if not (type_eq expr_type explicit_type) then
                  raise
                    (TypeError
                       ("Type error in declaration of `" ^ identifier
                      ^ "`: expected "
                       ^ string_of_type explicit_type
                       ^ ", but got " ^ string_of_type expr_type));
                explicit_type
          in
          let env2 = (identifier, final_type) :: env1 in
          (Ast.Type.SymbolType { value = "unit" }, env2)
      | None ->
          if explicit_type = Ast.Type.Infer then
            raise
              (TypeError
                 ("Missing type annotation and initializer for `" ^ identifier
                ^ "`"));
          let env1 = (identifier, explicit_type) :: env in
          (Ast.Type.SymbolType { value = "unit" }, env1))
  | Ast.Expr.MultiVarDeclExpr { identifier; assigned_value; explicit_type } ->
      let values, env1 =
        match assigned_value with
        | [ Ast.Expr.TupleExpr elements ] -> (elements, env)
        | [ Ast.Expr.VarExpr name ] -> (
            match List.assoc_opt name env with
            | Some (Ast.Type.TupleType element_types) ->
                let exprs =
                  List.mapi
                    (fun i _ ->
                      Ast.Expr.IndexExpr
                        {
                          array = Ast.Expr.VarExpr name;
                          index = Ast.Expr.IntExpr { value = Bigint.of_int i };
                        })
                    element_types
                in
                (exprs, env)
            | Some t ->
                raise
                  (TypeError
                     ("Cannot destructure non-tuple variable `" ^ name
                    ^ "`: expected tuple, but got " ^ string_of_type t))
            | None -> raise (TypeError ("Unbound variable `" ^ name ^ "`")))
        | [ expr ] -> (
            let expr_type, env2 = check_expr env func_env expr in
            let subst = Subst.empty () in
            match Subst.apply subst expr_type with
            | Ast.Type.TupleType element_types ->
                let exprs =
                  List.mapi
                    (fun i _ ->
                      Ast.Expr.IndexExpr
                        {
                          array = expr;
                          index = Ast.Expr.IntExpr { value = Bigint.of_int i };
                        })
                    element_types
                in
                (exprs, env2)
            | Ast.Type.VarType _ ->
                let fresh_elems =
                  List.init (List.length identifier) (fun _ -> fresh_tyvar ())
                in
                let tuple_ty = Ast.Type.TupleType fresh_elems in
                unify_with_subst subst expr_type tuple_ty;
                let element_types = List.map (Subst.apply subst) fresh_elems in
                let exprs =
                  List.mapi
                    (fun i _ ->
                      Ast.Expr.IndexExpr
                        {
                          array = expr;
                          index = Ast.Expr.IntExpr { value = Bigint.of_int i };
                        })
                    element_types
                in
                (exprs, env2)
            | _ ->
                raise
                  (TypeError
                     ("Cannot destructure non-tuple expression: expected \
                       tuple, but got " ^ string_of_type expr_type)))
        | _ -> (assigned_value, env)
      in
      let id_count = List.length identifier in
      let val_count = List.length values in
      if id_count <> val_count then
        raise
          (TypeError
             ("Tuple destructure mismatch: expected " ^ string_of_int id_count
            ^ " values, but got " ^ string_of_int val_count));
      let env2, inferred_types =
        fold_left_map2
          (fun acc_env ident expr ->
            let inferred, new_env = check_expr acc_env func_env expr in
            (new_env, (ident, inferred)))
          env1 identifier values
      in
      let env3 =
        match explicit_type with
        | Ast.Type.TupleType declared_types ->
            let type_count = List.length declared_types in
            if type_count <> id_count then
              raise
                (TypeError
                   ("Declared tuple type has " ^ string_of_int type_count
                  ^ " elements, but destructure has " ^ string_of_int id_count));
            List.iteri
              (fun i (ident, inferred) ->
                let expected = List.nth declared_types i in
                if not (type_eq inferred expected) then
                  raise
                    (TypeError
                       ("Type mismatch in destructure of `" ^ ident
                      ^ "`: expected " ^ string_of_type expected ^ ", but got "
                      ^ string_of_type inferred)))
              inferred_types;
            List.fold_left2
              (fun acc_env ident ty -> (ident, ty) :: acc_env)
              env2 identifier declared_types
        | Ast.Type.Infer ->
            List.fold_left
              (fun acc_env (ident, ty) -> (ident, ty) :: acc_env)
              env2 inferred_types
        | _ ->
            List.iter
              (fun (_, inferred) ->
                if not (type_eq inferred explicit_type) then
                  raise
                    (TypeError
                       ("Type mismatch in destructure: expected "
                       ^ string_of_type explicit_type
                       ^ ", but got " ^ string_of_type inferred)))
              inferred_types;
            List.fold_left
              (fun acc_env (ident, _) -> (ident, explicit_type) :: acc_env)
              env2 inferred_types
      in
      (Ast.Type.SymbolType { value = "unit" }, env3)
  | Ast.Expr.ImportExpr { module_name = mod_parts } -> (
      match mod_parts with
      | [ mod_name; symbol ] -> (
          if List.mem_assoc mod_name built_in_modules then stdlib_used := true;
          match List.assoc_opt mod_name built_in_modules with
          | Some mod_entries -> (
              match List.assoc_opt symbol mod_entries with
              | Some ty -> (ty, (symbol, ty) :: env)
              | None ->
                  raise
                    (TypeError
                       ("Import error:\n" ^ "  Module `" ^ mod_name
                      ^ "` does not contain symbol `" ^ symbol ^ "`")))
          | None ->
              raise
                (TypeError
                   ("Import error:\n" ^ "  Unknown module `" ^ mod_name ^ "`")))
      | _ ->
          raise
            (TypeError
               ("Invalid import syntax:\n"
              ^ "  Expected format: import `module.symbol`\n" ^ "  Got: `"
               ^ String.concat "." mod_parts
               ^ "`")))
  | Ast.Expr.ModuleExpr { module_name = _; block } ->
      List.fold_left
        (fun (_, env_acc) stmt -> check_expr env_acc func_env stmt)
        (Ast.Type.SymbolType { value = "unit" }, env)
        block
  | Ast.Expr.EnumExpr { name; members } ->
      Hashtbl.replace enum_variants name members;
      let enum_type = Ast.Type.SymbolType { value = name } in
      let new_env =
        List.fold_left
          (fun acc_env (_, member) ->
            (name ^ "." ^ member, enum_type) :: acc_env)
          env
          (List.mapi (fun i m -> (i, m)) members)
      in
      (enum_type, (name, enum_type) :: new_env)
  | Ast.Expr.LambdaExpr { parameters; body } ->
      let param_types = List.map (fun _ -> fresh_tyvar ()) parameters in
      let env_with_params =
        List.fold_left2
          (fun acc_env (param : Ast.Stmt.parameter) ty ->
            (param.Ast.Stmt.name, ty) :: acc_env)
          env parameters param_types
      in
      let body_type, _ = check_expr env_with_params func_env body in
      (Ast.Type.FunctionType (param_types, body_type), env)
  | _ -> failwith "Not Supported"

and find_return_exprs env func_env expr =
  match expr with
  | Ast.Expr.IfExpr { condition; then_branch; else_branch } ->
      let _ = check_expr env func_env condition in
      let returns_then = find_return_exprs env func_env then_branch in
      let returns_else =
        match else_branch with
        | Some e -> find_return_exprs env func_env e
        | None -> []
      in
      returns_then @ returns_else
  | Ast.Expr.BinaryExpr { left; operator = _; right } ->
      find_return_exprs env func_env left @ find_return_exprs env func_env right
  | Ast.Expr.CallExpr { callee; arguments } ->
      find_return_exprs env func_env callee
      @ List.concat_map (find_return_exprs env func_env) arguments
  | Ast.Expr.ArrayExpr { elements } ->
      List.flatten (List.map (find_return_exprs env func_env) elements)
  | Ast.Expr.UnaryExpr { operand; _ } -> find_return_exprs env func_env operand
  | Ast.Expr.IndexExpr { array; index } ->
      find_return_exprs env func_env array
      @ find_return_exprs env func_env index
  | _ -> []

and collect_functions stmts =
  let rec collect_from_stmt stmt acc =
    match stmt with
    | Ast.Stmt.FunctionDeclStmt { name; parameters; return_type; _ } ->
        let param_types =
          List.map
            (fun (p : Ast.Stmt.parameter) -> p.Ast.Stmt.param_type)
            parameters
        in
        let func_type = Ast.Type.FunctionType (param_types, return_type) in
        let scheme = generalize [] func_type in
        let overload =
          match List.assoc_opt name acc with
          | Some overloads -> (name, scheme :: overloads)
          | None -> (name, [ scheme ])
        in
        overload :: List.remove_assoc name acc
    | Ast.Stmt.BlockStmt { body } -> List.fold_right collect_from_stmt body acc
    | _ -> acc
  in
  List.fold_right collect_from_stmt stmts builtins

let typecheck_program stmts : bool =
  let env =
    [
      ("true", Ast.Type.SymbolType { value = "bool" });
      ("false", Ast.Type.SymbolType { value = "bool" });
    ]
  in
  let func_env = collect_functions stmts in
  let _final_env, _final_func_env =
    List.fold_left
      (fun (e, f) stmt -> check_stmt e f stmt)
      (env, func_env) stmts
  in
  let used = !stdlib_used in
  stdlib_used := false;
  used
