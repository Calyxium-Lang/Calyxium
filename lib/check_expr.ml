type type_error =
  | Mismatch of Ast.Type.t * Ast.Type.t * string
  | UnknownVariable of string
  | UnknownFunction of string
  | ArityMismatch of string * int * int
  | UnsupportedStmt of string
  | GenericError of string

let ( let* ) = Result.bind

let rec fold_left_result f acc = function
  | [] -> Ok acc
  | x :: xs ->
      let* acc' = f acc x in
      fold_left_result f acc' xs

let rec show_type = function
  | Ast.Type.VarType id -> Printf.sprintf "'a%d" id
  | Ast.Type.SymbolType { value } -> value
  | Ast.Type.ArrayType { element_type } ->
      Printf.sprintf "[%s]" (show_type element_type)
  | Ast.Type.TupleType (elems, Some rest) ->
      let elems_str = elems |> List.map show_type |> String.concat " * " in
      Printf.sprintf "(%s * %s)" elems_str (show_type rest)
  | Ast.Type.TupleType (elems, None) ->
      elems |> List.map show_type |> String.concat " * "
  | Ast.Type.FunctionType (args, ret) ->
      let args_str =
        match args with
        | [] -> "unit"
        | [ a ] -> show_type a
        | lst -> "(" ^ String.concat " * " (List.map show_type lst) ^ ")"
      in
      Printf.sprintf "%s -> %s" args_str (show_type ret)
  | Ast.Type.RecordType fields ->
      let fields_str =
        fields
        |> List.map (fun (name, ty) -> name ^ ": " ^ show_type ty)
        |> String.concat "; "
      in
      Printf.sprintf "{ %s }" fields_str
  | Ast.Type.Any -> "any"
  | Ast.Type.Infer -> "_"
  | _ -> ""

let string_of_type_error = function
  | Mismatch (t1, t2, ctx) ->
      Printf.sprintf "Type mismatch: expected %s but got %s in %s"
        (show_type t1) (show_type t2) ctx
  | UnknownVariable v -> Printf.sprintf "Unknown variable: %s" v
  | UnknownFunction f -> Printf.sprintf "Unknown function: %s" f
  | ArityMismatch (fn, exp, got) ->
      Printf.sprintf "Arity mismatch in %s: expected %d args but got %d" fn exp
        got
  | UnsupportedStmt s -> Printf.sprintf "Unsupported statement: %s" s
  | GenericError msg -> msg

let rec check_stmt env func_env stmt =
  match stmt with
  | Ir.IR_Expr e ->
      let* _, env' = check_expr env func_env e in
      Ok (env', func_env)
  | Ir.IR_Block instrs ->
      List.fold_left
        (fun acc i ->
          let* e, fe = acc in
          check_stmt e fe i)
        (Ok (env, func_env))
        instrs
  | Ir.IR_FuncDecl { Ir.name; is_rec; parameters; return_type; body } ->
      let param_types =
        List.map
          (fun p ->
            match p.Ast.Stmt.param_type with
            | Ast.Type.Infer -> Types.fresh_tyvar ()
            | t -> t)
          parameters
      in
      let declared_return =
        match return_type with Ast.Type.Infer -> Types.fresh_tyvar () | t -> t
      in
      let func_type = Ast.Type.FunctionType (param_types, declared_return) in

      let func_env' =
        if is_rec then (name, [ ([], func_type) ]) :: func_env else func_env
      in
      let env_with_params =
        try
          Ok
            (List.map2 (fun p t -> (p.Ast.Stmt.name, t)) parameters param_types
            @ env)
        with Invalid_argument _ ->
          Error
            (ArityMismatch
               (name, List.length parameters, List.length param_types))
      in

      let* env_with_params = env_with_params in
      let* env_final, func_env_final =
        List.fold_left
          (fun acc i ->
            let* e, fe = acc in
            check_stmt e fe i)
          (Ok (env_with_params, func_env'))
          body
      in
      Ok (env_final, (name, [ ([], func_type) ]) :: func_env_final)
  | _ -> Error (UnsupportedStmt "This kind of statement is not yet supported")

and check_expr env func_env expr =
  match expr with
  | Ir.IR_Int _ -> Ok (Ast.Type.SymbolType { value = "int" }, env)
  | Ir.IR_Float _ -> Ok (Ast.Type.SymbolType { value = "float" }, env)
  | Ir.IR_String _ -> Ok (Ast.Type.SymbolType { value = "string" }, env)
  | Ir.IR_Bool _ -> Ok (Ast.Type.SymbolType { value = "bool" }, env)
  | Ir.IR_Byte _ -> Ok (Ast.Type.SymbolType { value = "byte" }, env)
  | Ir.IR_Unit -> Ok (Ast.Type.SymbolType { value = "unit" }, env)
  | Ir.IR_Tuple elements ->
      let* element_types, env' =
        List.fold_right
          (fun elem acc ->
            let* types_acc, env_acc = acc in
            let* ty, env_new = check_expr env_acc func_env elem in
            let ty =
              match ty with Ast.Type.Infer -> Types.fresh_tyvar () | _ -> ty
            in
            Ok (ty :: types_acc, env_new))
          elements
          (Ok ([], env))
      in
      Ok (Ast.Type.TupleType (element_types, None), env')
  | Ir.IR_Var name -> (
      try
        let ty = List.assoc name env in
        Ok (ty, env)
      with Not_found ->
        raise
          (Types.TypeError
             ("Unbound variable reference:\n" ^ "  Variable `" ^ name
            ^ "` is not in scope.")))
  | Ir.IR_Unary (operator, operand_expr) -> (
      match check_expr env func_env operand_expr with
      | Error e -> Error e
      | Ok (operand_type, env') -> (
          match operator with
          | Token.Not ->
              if
                not
                  (Types.type_eq operand_type
                     (Ast.Type.SymbolType { value = "bool" }))
              then
                Error
                  (Mismatch
                     ( Ast.Type.SymbolType { value = "bool" },
                       operand_type,
                       "unary `not` expression" ))
              else Ok (Ast.Type.SymbolType { value = "bool" }, env')
          | Token.BitWiseNOT ->
              if
                not
                  (Types.type_eq operand_type
                     (Ast.Type.SymbolType { value = "int" }))
              then
                Error
                  (Mismatch
                     ( Ast.Type.SymbolType { value = "int" },
                       operand_type,
                       "unary bitwise NOT expression" ))
              else Ok (operand_type, env')
          | Token.Minus ->
              if
                not
                  (Types.type_eq operand_type
                     (Ast.Type.SymbolType { value = "int" })
                  || Types.type_eq operand_type
                       (Ast.Type.SymbolType { value = "float" }))
              then
                Error
                  (Mismatch
                     ( Ast.Type.SymbolType { value = "int or float" },
                       operand_type,
                       "unary minus expression" ))
              else Ok (operand_type, env')
          | _ ->
              Error (UnsupportedStmt "Unsupported unary operator in expression")
          ))
  | Ir.IR_Binary (lhs, op, rhs) -> (
      let* lt, env1 = check_expr env func_env lhs in
      let* rt, env2 = check_expr env1 func_env rhs in
      match op with
      | Token.Eq | Token.Neq -> (
          try
            Unify.unify lt rt;
            if not (Unify.can_compare lt rt) then
              Error
                (Mismatch
                   (lt, rt, "equality expression: operands cannot be compared"))
            else Ok (Ast.Type.SymbolType { value = "bool" }, env2)
          with Types.UnifyError msg ->
            Error (GenericError ("Unify failed: " ^ msg)))
      | Token.Less | Token.Greater | Token.LogicalAnd | Token.LogicalOr -> (
          try
            Unify.unify lt rt;
            Ok (Ast.Type.SymbolType { value = "bool" }, env2)
          with Types.UnifyError msg ->
            Error (GenericError ("Unify failed: " ^ msg)))
      | Token.BitWiseAND | Token.BitWiseOR | Token.BitWiseXOR | Token.LeftShift
      | Token.RightShift | Token.BitWiseANDAssign | Token.BitWiseORAssign
      | Token.BitWiseXORAssign | Token.LeftShiftAssign | Token.RightShiftAssign
        -> (
          try
            Unify.unify lt (Ast.Type.SymbolType { value = "int" });
            Ok (lt, env2)
          with Types.UnifyError msg ->
            Error (GenericError ("Unify failed: " ^ msg)))
      | _ -> (
          try
            Unify.unify lt rt;
            Ok (lt, env2)
          with Types.UnifyError msg ->
            Error (GenericError ("Unify failed: " ^ msg))))
  | Ir.IR_Call (callee_expr, arguments) -> (
      match callee_expr with
      | Ir.IR_Var name -> (
          let* env', arg_types =
            List.fold_left
              (fun acc_res arg ->
                let* acc_env, acc_types = acc_res in
                let* t, new_env = check_expr acc_env func_env arg in
                Ok (new_env, acc_types @ [ t ]))
              (Ok (env, []))
              arguments
          in
          match List.assoc_opt name func_env with
          | Some schemes -> (
              let instantiated_overloads =
                List.map
                  (fun scheme ->
                    match Unify.instantiate_scheme scheme with
                    | Ast.Type.FunctionType (params, ret) -> (params, ret)
                    | _ -> ([], Ast.Type.Any))
                  schemes
              in
              let matching =
                List.find_opt
                  (fun (params, _) ->
                    List.length params = List.length arg_types
                    &&
                    try
                      List.iter2
                        (fun param_ty arg_ty -> Unify.unify param_ty arg_ty)
                        params arg_types;
                      true
                    with Types.UnifyError _ -> false)
                  instantiated_overloads
              in
              match matching with
              | Some (_, return_type) -> Ok (return_type, env')
              | None ->
                  Error
                    (ArityMismatch
                       ( name,
                         List.length arg_types,
                         match instantiated_overloads with
                         | (params, _) :: _ -> List.length params
                         | [] -> 0 )))
          | None -> (
              match List.assoc_opt name env with
              | Some (Ast.Type.FunctionType (param_types, return_type)) ->
                  if
                    List.length param_types = List.length arg_types
                    && List.for_all2 Types.type_eq param_types arg_types
                  then Ok (return_type, env')
                  else
                    Error
                      (ArityMismatch
                         (name, List.length param_types, List.length arg_types))
              | _ -> Error (UnknownFunction name)))
      | _ -> Error (UnsupportedStmt "Function call on non-variable expression"))
  | Ir.IR_Array elements -> (
      let* element_types, final_env =
        List.fold_right
          (fun elem acc ->
            let* types_acc, env_acc = acc in
            let* ty, env_new = check_expr env_acc func_env elem in
            Ok (ty :: types_acc, env_new))
          elements
          (Ok ([], env))
      in
      match element_types with
      | [] -> Ok (Ast.Type.ArrayType { element_type = Ast.Type.Any }, final_env)
      | hd :: tl ->
          List.iter
            (fun t ->
              if not (Types.type_eq t hd) then
                raise
                  (Types.TypeError
                     ("Type error in array expression:\n"
                    ^ "  All elements must have the same type\n"
                    ^ "  Found mismatch: " ^ Types.string_of_type t ^ " vs "
                    ^ Types.string_of_type hd)))
            tl;
          Ok (Ast.Type.ArrayType { element_type = hd }, final_env))
  | Ir.IR_Index (array_expr, index_expr) -> (
      let* at, env' = check_expr env func_env array_expr in
      let* index_type, env'' = check_expr env' func_env index_expr in

      let is_enum_type = function
        | Ast.Type.SymbolType { value } ->
            Hashtbl.mem Builtins.enum_variants value
        | _ -> false
      in

      if
        not
          (Types.type_eq index_type (Ast.Type.SymbolType { value = "int" })
          || is_enum_type index_type)
      then
        raise
          (Types.TypeError
             ("Type error in index expression:\n"
            ^ "  Index must be an integer or enum value\n" ^ "  Found: "
             ^ Types.string_of_type index_type));

      match at with
      | Ast.Type.ArrayType { element_type } -> Ok (element_type, env'')
      | Ast.Type.TupleType (element_types, rest) -> (
          match index_expr with
          | Ir.IR_Int value -> (
              let idx = Bigint.to_int value in
              let len = List.length element_types in
              if idx < 0 then
                raise
                  (Types.TypeError
                     ("Tuple index out of bounds:\n" ^ "  Index: "
                    ^ string_of_int idx ^ "\n  Tuple size: " ^ string_of_int len
                     ))
              else if idx < len then Ok (List.nth element_types idx, env'')
              else
                match rest with
                | Some rest_ty -> Ok (rest_ty, env'')
                | None ->
                    raise
                      (Types.TypeError
                         ("Tuple index out of bounds:\n" ^ "  Index: "
                        ^ string_of_int idx ^ "\n  Tuple size: "
                        ^ string_of_int len)))
          | _ ->
              raise
                (Types.TypeError
                   ("Invalid tuple index:\n"
                  ^ "  Only constant integer indices (e.g. `t[0]`) are allowed"
                   )))
      | Ast.Type.VarType id -> (
          let subst_tbl = Subst.empty () in
          match index_expr with
          | Ir.IR_Int value ->
              let idx = Bigint.to_int value in
              let fresh_elems =
                List.init (idx + 1) (fun _ -> Types.fresh_tyvar ())
              in
              let tuple_ty = Ast.Type.TupleType (fresh_elems, None) in
              Unify.unify_with_subst subst_tbl (Ast.Type.VarType id) tuple_ty;
              let element_types =
                List.map (Subst.apply subst_tbl) fresh_elems
              in
              if idx < 0 || idx >= List.length element_types then
                raise
                  (Types.TypeError
                     ("Tuple index out of bounds:\n" ^ "  Index: "
                    ^ string_of_int idx ^ "\n  Tuple size: "
                     ^ string_of_int (List.length element_types)));
              Ok (List.nth element_types idx, env'')
          | _ ->
              let elem_ty = Types.fresh_tyvar () in
              let arr_ty = Ast.Type.ArrayType { element_type = elem_ty } in
              Unify.unify_with_subst subst_tbl (Ast.Type.VarType id) arr_ty;
              Ok (Subst.apply subst_tbl elem_ty, env''))
      | _ ->
          raise
            (Types.TypeError
               ("Type error in index expression:\n"
              ^ "  Can only index into arrays or tuples\n" ^ "  Found: "
              ^ Types.string_of_type at)))
  | Ir.IR_If (condition, then_branch, else_branch_opt) ->
      let* ct, env' = check_expr env func_env condition in
      if not (Types.type_eq ct (Ast.Type.SymbolType { value = "bool" })) then
        raise
          (Types.TypeError
             ("Type error in `if` expression condition:\n"
            ^ "  Expected: bool\n" ^ "  Found:    " ^ Types.string_of_type ct));
      let* t_then, env'' = check_expr env' func_env then_branch in
      let* t_else, env''' =
        match else_branch_opt with
        | Some e -> check_expr env'' func_env e
        | None -> Ok (t_then, env'')
      in
      if not (Types.type_eq t_then t_else) then
        raise
          (Types.TypeError
             ("Type mismatch in `if` expression branches:\n" ^ "  Then branch: "
             ^ Types.string_of_type t_then
             ^ "\n" ^ "  Else branch: "
             ^ Types.string_of_type t_else));
      Ok (t_then, env''')
  | Ir.IR_Dot (left, right) -> (
      let* left_type, env' = check_expr env func_env left in
      match left with
      | Ir.IR_Var name -> (
          match List.assoc_opt (name ^ "." ^ right) env' with
          | Some member_type -> Ok (member_type, env')
          | None -> (
              match List.assoc_opt name env' with
              | Some (Ast.Type.RecordType fields) -> (
                  match List.assoc_opt right fields with
                  | Some ty -> Ok (ty, env')
                  | None ->
                      raise
                        (Types.TypeError
                           ("Unknown record field:\n  `" ^ right
                          ^ "` is not a field of record `" ^ name ^ "`")))
              | Some _ ->
                  raise
                    (Types.TypeError
                       ("Cannot access `" ^ right ^ "` on non-record value `"
                      ^ name ^ "`"))
              | None ->
                  raise
                    (Types.TypeError
                       ("Unknown enum or record:\n  `" ^ name
                      ^ "` is not defined"))))
      | _ -> (
          match left_type with
          | Ast.Type.RecordType fields -> (
              match List.assoc_opt right fields with
              | Some ty -> Ok (ty, env')
              | None ->
                  raise
                    (Types.TypeError
                       ("Unknown record field:\n  `" ^ right
                      ^ "` is not a field of the record")))
          | _ ->
              raise
                (Types.TypeError
                   ("Dot access error:\n  Cannot access field `" ^ right
                  ^ "` on non-record or non-enum expression"))))
  | Ir.IR_Ternary (cond, onTrue, onFalse) ->
      let* ct, env1 = check_expr env func_env cond in
      if not (Types.type_eq ct (Ast.Type.SymbolType { value = "bool" })) then
        raise
          (Types.TypeError
             ("Type error in ternary condition:\n  Expected: bool\n"
            ^ "  Found:    " ^ Types.string_of_type ct));
      let* t_true, env2 = check_expr env1 func_env onTrue in
      let* t_false, env3 = check_expr env2 func_env onFalse in
      if Types.type_eq t_true t_false then Ok (t_true, env3)
      else
        raise
          (Types.TypeError
             ("Type mismatch in ternary branches:\n  True branch:  "
             ^ Types.string_of_type t_true
             ^ "\n  False branch: "
             ^ Types.string_of_type t_false))
  | Ir.IR_Pipeline (left, right) -> (
      let* arg_type, env1 = check_expr env func_env left in
      match right with
      | Ir.IR_Var name -> (
          match List.assoc_opt name func_env with
          | Some schemes -> (
              let instantiated_overloads =
                List.map
                  (fun scheme ->
                    match Unify.instantiate_scheme scheme with
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
                    | [ param_type ] -> Types.type_eq param_type arg_type
                    | _ -> false)
                  instantiated_overloads
              in
              match matching with
              | Some (_, return_type) -> Ok (return_type, env1)
              | None ->
                  raise
                    (Types.TypeError
                       ("No matching overload for pipeline call to `" ^ name
                      ^ "`:\n  Argument type: "
                       ^ Types.string_of_type arg_type)))
          | None -> (
              match List.assoc_opt name env with
              | Some (Ast.Type.FunctionType ([ param_type ], return_type)) ->
                  if Types.type_eq param_type arg_type then
                    Ok (return_type, env1)
                  else
                    raise
                      (Types.TypeError
                         ("Function `" ^ name
                        ^ "` called via pipeline expects:\n  Parameter type: "
                         ^ Types.string_of_type param_type
                         ^ "\n  But got:         "
                         ^ Types.string_of_type arg_type))
              | Some _ ->
                  raise
                    (Types.TypeError
                       ("Pipeline error:\n  `" ^ name
                      ^ "` is not a unary function"))
              | None ->
                  raise
                    (Types.TypeError
                       ("Unknown function in pipeline:\n  `" ^ name
                      ^ "` is not declared"))))
      | Ir.IR_Lambda { parameters; body } -> (
          let param_types =
            List.map (fun _ -> Types.fresh_tyvar ()) parameters
          in
          let env_with_params =
            List.fold_left2
              (fun acc_env param ty -> (param.Ast.Stmt.name, ty) :: acc_env)
              env parameters param_types
          in
          let* body_type =
            match List.rev body with
            | Ir.IR_Expr expr :: _ -> check_expr env_with_params func_env expr
            | [] -> Ok (Ast.Type.Any, env_with_params)
            | _ -> Ok (Ast.Type.Any, env_with_params)
          in
          match param_types with
          | [ param_type ] ->
              if Types.type_eq param_type arg_type then Ok body_type
              else
                raise
                  (Types.TypeError
                     ("Pipeline function parameter type mismatch:\n  Expected: "
                     ^ Types.string_of_type param_type
                     ^ "\n  Got:      "
                     ^ Types.string_of_type arg_type))
          | _ ->
              raise
                (Types.TypeError
                   "Pipeline lambda function must have exactly one parameter"))
      | _ ->
          raise
            (Types.TypeError
               "Invalid pipeline usage:\n\
               \  Right-hand side must be a function identifier or lambda"))
  | Ir.IR_Match (expr, cases) -> (
      let* et, env1 = check_expr env func_env expr in
      match cases with
      | [] -> Ok (Ast.Type.SymbolType { value = "unit" }, env1)
      | (pat_opt, case_body) :: rest ->
          let* () =
            match pat_opt with
            | Some pat_expr ->
                let* pt, _ = check_expr env func_env pat_expr in
                if not (Types.type_eq et pt) then
                  raise
                    (Types.TypeError
                       ("Pattern match type mismatch:\n"
                      ^ "  Match expression type: " ^ Types.string_of_type et
                      ^ "\n" ^ "  Pattern type:          "
                      ^ Types.string_of_type pt))
                else Ok ()
            | None -> Ok ()
          in

          let* final_env, final_func_env =
            fold_left_result
              (fun (e, fe) stmt -> check_stmt e fe stmt)
              (env, func_env) case_body
          in

          let* first_branch_type =
            match List.rev case_body with
            | Ir.IR_Expr expr :: _ ->
                let* t, _ = check_expr final_env final_func_env expr in
                Ok t
            | _ -> Ok (Ast.Type.SymbolType { value = "unit" })
          in

          let check_branch (pat_opt, case_body) =
            let* () =
              match pat_opt with
              | Some pat_expr ->
                  let* pt, _ = check_expr env func_env pat_expr in
                  if not (Types.type_eq et pt) then
                    raise
                      (Types.TypeError
                         ("Pattern match type mismatch:\n"
                        ^ "  Match expression type: " ^ Types.string_of_type et
                        ^ "\n" ^ "  Pattern type:          "
                        ^ Types.string_of_type pt))
                  else Ok ()
              | None -> Ok ()
            in

            let* branch_env, branch_func_env =
              fold_left_result
                (fun (e, fe) stmt -> check_stmt e fe stmt)
                (env, func_env) case_body
            in

            let* branch_type =
              match List.rev case_body with
              | Ir.IR_Expr expr :: _ ->
                  let* t, _ = check_expr branch_env branch_func_env expr in
                  Ok t
              | _ -> Ok (Ast.Type.SymbolType { value = "unit" })
            in

            if not (Types.type_eq branch_type first_branch_type) then
              raise
                (Types.TypeError
                   ("Type mismatch in match branches:\n" ^ "  Expected: "
                   ^ Types.string_of_type first_branch_type
                   ^ "\n" ^ "  Found:    "
                   ^ Types.string_of_type branch_type));
            Ok ()
          in

          let* () =
            List.fold_left
              (fun acc branch ->
                let* () = acc in
                check_branch branch)
              (Ok ()) rest
          in

          Ok (first_branch_type, env1))
  | Ir.IR_Range (start_opt, end_opt) -> (
      let int_ty = Ast.Type.SymbolType { value = "int" } in
      let array_int_ty = Ast.Type.ArrayType { element_type = int_ty } in

      match (start_opt, end_opt) with
      | Some s, Some e ->
          let* t_s, env1 = check_expr env func_env s in
          let* t_e, env2 = check_expr env1 func_env e in
          if (not (Types.type_eq t_s int_ty)) || not (Types.type_eq t_e int_ty)
          then
            raise
              (Types.TypeError
                 ("Range expression requires integer bounds:\n" ^ "  Found: "
                ^ Types.string_of_type t_s ^ " and " ^ Types.string_of_type t_e
                 ));
          Ok (array_int_ty, env2)
      | Some s, None ->
          let* t_s, env1 = check_expr env func_env s in
          if not (Types.type_eq t_s int_ty) then
            raise
              (Types.TypeError
                 ("Open-ended range `{x..}` requires integer start:\n"
                ^ "  Found: " ^ Types.string_of_type t_s));
          Ok (array_int_ty, env1)
      | None, Some e ->
          let* t_e, env1 = check_expr env func_env e in
          if not (Types.type_eq t_e int_ty) then
            raise
              (Types.TypeError
                 ("Open-start range `{..x}` requires integer end:\n"
                ^ "  Found: " ^ Types.string_of_type t_e));
          Ok (array_int_ty, env1)
      | None, None ->
          raise
            (Types.TypeError "Invalid range expression: both bounds missing"))
  | Ir.IR_BlockExpr instrs ->
      let rec check_stmts env func_env stmts =
        match stmts with
        | [] -> Ok (Ast.Type.SymbolType { value = "unit" }, env, func_env)
        | [ last_stmt ] -> (
            let* env', func_env' = check_stmt env func_env last_stmt in
            match last_stmt with
            | Ir.IR_Expr expr ->
                let* t, env'' = check_expr env' func_env' expr in
                Ok (t, env'', func_env')
            | _ -> Ok (Ast.Type.SymbolType { value = "unit" }, env', func_env'))
        | hd :: tl ->
            let* env', func_env' = check_stmt env func_env hd in
            check_stmts env' func_env' tl
      in

      let* block_type, env_after, _func_env_after =
        check_stmts env func_env instrs
      in

      Ok (block_type, env_after)
  | Ir.IR_Slice (array, start, end_) ->
      let int_ty = Ast.Type.SymbolType { value = "int" } in

      let* array_ty, env1 = check_expr env func_env array in

      let* () =
        match array_ty with
        | Ast.Type.ArrayType _ -> Ok ()
        | Ast.Type.VarType _ -> Ok ()
        | _ ->
            raise
              (Types.TypeError
                 ("Attempted to slice a non-array value: "
                 ^ Types.string_of_type array_ty))
      in

      let* array_ty_resolved =
        match array_ty with
        | Ast.Type.ArrayType _ -> Ok array_ty
        | Ast.Type.VarType id ->
            let elem_ty = Types.fresh_tyvar () in
            let arr_ty = Ast.Type.ArrayType { element_type = elem_ty } in
            let subst_tbl = Subst.empty () in
            Unify.unify_with_subst subst_tbl (Ast.Type.VarType id) arr_ty;
            Ok (Subst.apply subst_tbl arr_ty)
        | _ -> assert false
      in

      let* env2 =
        match start with
        | None -> Ok env1
        | Some s ->
            let* s_ty, env' = check_expr env1 func_env s in
            if Types.type_eq s_ty int_ty then Ok env'
            else
              raise
                (Types.TypeError
                   ("Slice start index must be int, found: "
                  ^ Types.string_of_type s_ty))
      in

      let* env3 =
        match end_ with
        | None -> Ok env2
        | Some e ->
            let* e_ty, env' = check_expr env2 func_env e in
            if Types.type_eq e_ty int_ty then Ok env'
            else
              raise
                (Types.TypeError
                   ("Slice end index must be int, found: "
                  ^ Types.string_of_type e_ty))
      in

      Ok (array_ty_resolved, env3)
  | Ir.IR_VarDecl { Ir.var_name; typ; value } -> (
      let unit_ty = Ast.Type.SymbolType { value = "unit" } in
      match value with
      | Some expr ->
          let* expr_type, env1 = check_expr env func_env expr in

          let* final_type =
            match typ with
            | Ast.Type.Infer -> Ok expr_type
            | _ ->
                if Types.type_eq expr_type typ then Ok typ
                else
                  raise
                    (Types.TypeError
                       ("Type error in declaration of `" ^ var_name
                      ^ "`: expected " ^ Types.string_of_type typ ^ ", but got "
                       ^ Types.string_of_type expr_type))
          in

          let env2 = (var_name, final_type) :: env1 in
          Ok (unit_ty, env2)
      | None ->
          if typ = Ast.Type.Infer then
            raise
              (Types.TypeError
                 ("Missing type annotation and initializer for `" ^ var_name
                ^ "`"))
          else
            let env1 = (var_name, typ) :: env in
            Ok (unit_ty, env1))
  | Ir.IR_MultiVarDecl { names; value; typ } ->
      let unit_ty = Ast.Type.SymbolType { value = "unit" } in

      (* Infer the type of the tuple on the right-hand side *)
      let* value_type, env1 = check_expr env func_env value in

      let* elem_types =
        match value_type with
        | Ast.Type.TupleType (elements, None) -> Ok elements
        | Ast.Type.VarType _ ->
            let fresh_elems =
              List.init (List.length names) (fun _ -> Types.fresh_tyvar ())
            in
            let tuple_ty = Ast.Type.TupleType (fresh_elems, None) in
            Unify.unify value_type tuple_ty;
            Ok fresh_elems
        | _ ->
            raise
              (Types.TypeError
                 ("Cannot destructure non-tuple value: expected tuple, but got "
                 ^ Types.string_of_type value_type))
      in

      if List.length names <> List.length elem_types then
        raise
          (Types.TypeError
             ("Tuple destructure mismatch: expected "
             ^ string_of_int (List.length names)
             ^ " values, but got "
             ^ string_of_int (List.length elem_types)));

      let* env2 =
        match typ with
        | Ast.Type.TupleType (declared_types, None) ->
            if List.length declared_types <> List.length names then
              raise
                (Types.TypeError
                   ("Declared tuple type has "
                   ^ string_of_int (List.length declared_types)
                   ^ " elements, but destructure has "
                   ^ string_of_int (List.length names)));

            List.iter2
              (fun inferred expected ->
                if not (Types.type_eq inferred expected) then
                  raise
                    (Types.TypeError
                       ("Type mismatch in destructure: expected "
                       ^ Types.string_of_type expected
                       ^ ", but got "
                       ^ Types.string_of_type inferred)))
              elem_types declared_types;

            Ok
              (List.fold_left2
                 (fun acc_env name ty -> (name, ty) :: acc_env)
                 env1 names declared_types)
        | Ast.Type.Infer ->
            Ok
              (List.fold_left2
                 (fun acc_env name ty -> (name, ty) :: acc_env)
                 env1 names elem_types)
        | _ ->
            List.iter
              (fun inferred ->
                if not (Types.type_eq inferred typ) then
                  raise
                    (Types.TypeError
                       ("Type mismatch in destructure: expected "
                      ^ Types.string_of_type typ ^ ", but got "
                       ^ Types.string_of_type inferred)))
              elem_types;

            Ok
              (List.fold_left
                 (fun acc_env name -> (name, typ) :: acc_env)
                 env1 names)
      in

      Ok (unit_ty, env2)
  | Ir.IR_Enum { name; members } ->
      let enum_type = Ast.Type.SymbolType { value = name } in
      Hashtbl.replace Builtins.enum_variants name (List.map fst members);

      let env_with_members =
        List.fold_left
          (fun acc_env (member_name, _) ->
            (name ^ "." ^ member_name, enum_type) :: acc_env)
          env members
      in
      let final_env = (name, enum_type) :: env_with_members in
      Ok (enum_type, final_env)
  | Ir.IR_Lambda { parameters; body } ->
      let unit_ty = Ast.Type.SymbolType { value = "unit" } in

      let param_names =
        List.map (fun (p : Ast.Stmt.parameter) -> p.Ast.Stmt.name) parameters
      in

      let param_types = List.map (fun _ -> Types.fresh_tyvar ()) param_names in

      let env_with_params =
        List.fold_left2
          (fun acc_env name ty -> (name, ty) :: acc_env)
          env param_names param_types
      in

      let rec check_block env instrs =
        match instrs with
        | [] -> Ok (unit_ty, env)
        | [ Ir.IR_Expr expr ] ->
            let* t, env' = check_expr env func_env expr in
            Ok (t, env')
        | hd :: tl ->
            let* env', _ = check_stmt env func_env hd in
            check_block env' tl
      in
      let* body_type, _final_env = check_block env_with_params body in
      Ok (Ast.Type.FunctionType (param_types, body_type), env)
  | _ -> failwith "Not Supported"

and find_return_exprs env func_env expr =
  match expr with
  | Ir.IR_If (cond, then_branch, else_branch_opt) ->
      let _ = check_expr env func_env cond in
      let returns_then = find_return_exprs env func_env then_branch in
      let returns_else =
        match else_branch_opt with
        | Some e -> find_return_exprs env func_env e
        | None -> []
      in
      returns_then @ returns_else
  | Ir.IR_Binary (left, _, right) ->
      find_return_exprs env func_env left @ find_return_exprs env func_env right
  | Ir.IR_Call (callee, args) ->
      find_return_exprs env func_env callee
      @ List.concat_map (find_return_exprs env func_env) args
  | Ir.IR_Array elements ->
      List.flatten (List.map (find_return_exprs env func_env) elements)
  | Ir.IR_Unary (_, operand) -> find_return_exprs env func_env operand
  | Ir.IR_Index (array_expr, index_expr) ->
      find_return_exprs env func_env array_expr
      @ find_return_exprs env func_env index_expr
  | _ -> []
(*
  | Ast.Expr.ImportExpr { module_name = mod_parts } -> (
      match mod_parts with
      | [ mod_name; symbol ] -> (
          if List.mem_assoc mod_name Builtins.built_in_modules then
            Builtins.stdlib_used := true;
          match List.assoc_opt mod_name Builtins.built_in_modules with
          | Some mod_entries -> (
              match List.assoc_opt symbol mod_entries with
              | Some ty -> (ty, (symbol, ty) :: env)
              | None ->
                  raise
                    (Types.TypeError
                       ("Import error:\n" ^ "  Module `" ^ mod_name
                      ^ "` does not contain symbol `" ^ symbol ^ "`")))
          | None ->
              raise
                (Types.TypeError
                   ("Import error:\n" ^ "  Unknown module `" ^ mod_name ^ "`")))
      | _ ->
          raise
            (Types.TypeError
               ("Invalid import syntax:\n"
              ^ "  Expected format: import `module.symbol`\n" ^ "  Got: `"
               ^ String.concat "." mod_parts
               ^ "`")))
  | Ast.Expr.ModuleExpr { module_name = _; block } ->
      List.fold_left
        (fun (_, env_acc) stmt -> check_expr env_acc func_env stmt)
        (Ast.Type.SymbolType { value = "unit" }, env)
        block *)

and collect_functions stmts =
  let rec collect_from_stmt stmt acc =
    match stmt with
    | Ir.IR_FuncDecl { Ir.name; parameters; return_type; _ } ->
        let param_types =
          List.map
            (fun (p : Ast.Stmt.parameter) -> p.Ast.Stmt.param_type)
            parameters
        in
        let func_type = Ast.Type.FunctionType (param_types, return_type) in
        let scheme = Unify.generalize [] func_type in
        let overload =
          match List.assoc_opt name acc with
          | Some overloads -> (name, scheme :: overloads)
          | None -> (name, [ scheme ])
        in
        overload :: List.remove_assoc name acc
    | Ir.IR_Block body -> List.fold_right collect_from_stmt body acc
    | _ -> acc
  in
  List.fold_right collect_from_stmt stmts Builtins.builtins
