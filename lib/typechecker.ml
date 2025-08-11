open Ast
open Ast.Type
open Ast.Stmt
open Ast.Expr
open Token

exception TypeError of string

let enum_variants : (string, string list) Hashtbl.t = Hashtbl.create 10
let stdlib_used = ref false

let rec string_of_type = function
  | Any -> "any"
  | Infer -> "infer"
  | SymbolType { value } -> value
  | ArrayType { element_type } -> "[" ^ string_of_type element_type ^ "]"
  | TupleType types ->
      "(" ^ String.concat ", " (List.map string_of_type types) ^ ")"
  | FunctionType (params, ret) ->
      let params_str = String.concat " * " (List.map string_of_type params) in
      Printf.sprintf "(%s -> %s)" params_str (string_of_type ret)
  | RecordType fields ->
      let field_strs =
        List.map (fun (name, ty) -> name ^ ": " ^ string_of_type ty) fields
      in
      "record { " ^ String.concat "; " field_strs ^ " }"

let rec can_compare t1 t2 =
  match (t1, t2) with
  | Type.Any, _ -> true
  | _, Type.Any -> true
  | Type.SymbolType { value = v1 }, Type.SymbolType { value = v2 } -> v1 = v2
  | Type.TupleType ts1, Type.TupleType ts2 ->
      List.length ts1 = List.length ts2 && List.for_all2 can_compare ts1 ts2
  | Type.ArrayType { element_type = et1 }, Type.ArrayType { element_type = et2 }
    ->
      can_compare et1 et2
  | _ -> false

let rec fold_left_map2 f env idents exprs =
  match (idents, exprs) with
  | [], [] -> (env, [])
  | ident :: id_rest, expr :: expr_rest ->
      let env', inferred = f env ident expr in
      let env'', rest = fold_left_map2 f env' id_rest expr_rest in
      (env'', inferred :: rest)
  | _ -> failwith "fold_left_map2: lists have different lengths"

let built_in_modules : (string * (string * Type.t) list) list =
  [
    ( "Math",
      [
        ("pi", SymbolType { value = "float" });
        ("e", SymbolType { value = "float" });
        ("tau", SymbolType { value = "float" });
        ("nan", SymbolType { value = "float" });
        ("inf", SymbolType { value = "float" });
        ("neg_inf", SymbolType { value = "float" });
        ( "sin",
          FunctionType
            ([ SymbolType { value = "float" } ], SymbolType { value = "float" })
        );
      ] );
  ]

let builtins : (string * (Type.t list * Type.t) list) list =
  [
    ("print", [ ([ Any ], SymbolType { value = "unit" }) ]);
    ( "input",
      [ ([ SymbolType { value = "string" } ], SymbolType { value = "unit" }) ]
    );
    ( "to_bytes",
      [
        ( [ SymbolType { value = "string" } ],
          ArrayType { element_type = SymbolType { value = "byte" } } );
      ] );
    ( "to_float",
      [
        ([ SymbolType { value = "*" } ], SymbolType { value = "float" });
        ([ SymbolType { value = "string" } ], SymbolType { value = "float" });
        ([ SymbolType { value = "int" } ], SymbolType { value = "float" });
      ] );
    ( "to_int",
      [
        ([ SymbolType { value = "*" } ], SymbolType { value = "int" });
        ([ SymbolType { value = "byte" } ], SymbolType { value = "int" });
        ([ SymbolType { value = "string" } ], SymbolType { value = "int" });
        ([ SymbolType { value = "float" } ], SymbolType { value = "int" });
      ] );
    ("length", [ ([ Any ], SymbolType { value = "int" }) ]);
    ("to_string", [ ([ Any ], SymbolType { value = "string" }) ]);
    ( "assert",
      [ ([ SymbolType { value = "bool" } ], SymbolType { value = "unit" }) ] );
    ( "panic",
      [ ([ SymbolType { value = "string" } ], SymbolType { value = "unit" }) ]
    );
    ( "to_byte",
      [
        ( [ ArrayType { element_type = SymbolType { value = "int" } } ],
          SymbolType { value = "byte" } );
        ([ SymbolType { value = "int" } ], SymbolType { value = "byte" });
        ([ SymbolType { value = "string" } ], SymbolType { value = "byte" });
      ] );
  ]

let rec type_eq expected actual =
  match (expected, actual) with
  | SymbolType { value = "*" }, SymbolType _ -> true
  | SymbolType { value = "unit" }, _ -> true
  | Any, _ | _, Any -> true
  | SymbolType { value = v1 }, SymbolType { value = v2 } -> v1 = v2
  | ArrayType { element_type = e1 }, ArrayType { element_type = e2 } ->
      type_eq e1 e2
  | SymbolType { value = "tuple" }, TupleType _ -> true
  | TupleType _, SymbolType { value = "tuple" } -> true
  | TupleType xs, TupleType ys ->
      List.length xs = List.length ys && List.for_all2 type_eq xs ys
  | _ -> false

let rec check_stmt env func_env stmt =
  match stmt with
  | ExprStmt expr ->
      let _, env1 = check_expr env func_env expr in
      env1
  | FunctionDeclStmt { name; is_rec; parameters; return_type; body } ->
      let local_funcs = collect_functions body in
      let param_types = List.map (fun p -> p.param_type) parameters in
      let this_func = (name, [ (param_types, return_type) ]) in
      let func_env =
        let base_env = local_funcs @ func_env in
        match List.assoc_opt name base_env with
        | Some overloads ->
            (name, (param_types, return_type) :: overloads)
            :: List.remove_assoc name base_env
        | None -> this_func :: base_env
      in
      let rec contains_recursive_call fname expr =
        let open Expr in
        match expr with
        | CallExpr { callee = VarExpr callee_name; _ } -> callee_name = fname
        | CallExpr { callee; arguments } ->
            contains_recursive_call fname callee
            || List.exists (contains_recursive_call fname) arguments
        | UnaryExpr { operand; _ } -> contains_recursive_call fname operand
        | BinaryExpr { left; right; _ } ->
            contains_recursive_call fname left
            || contains_recursive_call fname right
        | IfExpr { condition; then_branch; else_branch } -> (
            contains_recursive_call fname condition
            || contains_recursive_call fname then_branch
            ||
            match else_branch with
            | Some e -> contains_recursive_call fname e
            | None -> false)
        | ArrayExpr { elements } ->
            List.exists (contains_recursive_call fname) elements
        | IndexExpr { array; index } ->
            contains_recursive_call fname array
            || contains_recursive_call fname index
        | _ -> false
      in
      let rec contains_recursive_call_stmt fname stmt =
        let open Stmt in
        match stmt with
        | ExprStmt e -> contains_recursive_call fname e
        | BlockStmt { body } ->
            List.exists (contains_recursive_call_stmt fname) body
        | _ -> false
      in
      let has_recursive_call =
        List.exists (contains_recursive_call_stmt name) body
      in
      if has_recursive_call && not is_rec then
        raise
          (TypeError
             ("Function `" ^ name ^ "` is missing `rec` keyword:\n"
            ^ "  This function contains a recursive call to itself, but is not \
               marked `rec`.\n" ^ "  Add `rec` to allow recursive behavior."));
      let param_env = List.map (fun p -> (p.name, p.param_type)) parameters in
      let env_with_params = param_env @ env in
      let final_env =
        List.fold_left
          (fun e stmt -> check_stmt e func_env stmt)
          env_with_params body
      in
      let rec gather_return_types env func_env stmts =
        List.concat_map
          (function
            | ExprStmt e -> find_return_exprs env func_env e
            | BlockStmt { body } -> gather_return_types env func_env body
            | _ -> [])
          stmts
      in
      let return_expr_types = gather_return_types final_env func_env body in
      let return_types_only = List.map fst return_expr_types in

      let last_expr_type =
        match
          List.rev body
          |> List.find_opt (function ExprStmt _ -> true | _ -> false)
        with
        | Some (ExprStmt expr) -> Some (check_expr final_env func_env expr)
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
          if not (type_eq return_type actual_type) then
            raise
              (TypeError
                 ("Function `" ^ name ^ "` has mismatched return type:\n"
                ^ "  Expected: " ^ string_of_type return_type ^ "\n"
                ^ "  Found:    " ^ string_of_type actual_type ^ "\n"
                ^ "  All return expressions must match the declared return \
                   type.")))
        all_return_types;
      env
  | BlockStmt { body } ->
      let _final_env =
        List.fold_left (fun e stmt -> check_stmt e func_env stmt) env body
      in
      _final_env
  | RecordStmt { name; fields } ->
      let record_env =
        List.fold_left
          (fun acc_env field_stmt ->
            match field_stmt with
            | VarDeclExpr _ -> check_stmt acc_env func_env (ExprStmt field_stmt)
            | _ ->
                raise
                  (TypeError
                     ("Invalid statement in record `" ^ name
                    ^ "`: only variable delcarations are allowed")))
          [] fields
      in
      let record_type =
        let fields_as_types =
          List.rev_map (fun (id, ty) -> (id, ty)) record_env
        in
        RecordType fields_as_types
      in
      (name, record_type) :: env

and check_expr env func_env expr =
  match expr with
  | IntExpr _ -> (SymbolType { value = "int" }, env)
  | FloatExpr _ -> (SymbolType { value = "float" }, env)
  | StringExpr _ -> (SymbolType { value = "string" }, env)
  | BoolExpr _ -> (SymbolType { value = "bool" }, env)
  | ByteExpr _ -> (SymbolType { value = "byte" }, env)
  | UnitExpr _ -> (SymbolType { value = "unit" }, env)
  | TupleExpr elements ->
      let element_types, env' =
        List.fold_right
          (fun elem (types_acc, env_acc) ->
            let ty, env_new = check_expr env_acc func_env elem in
            (ty :: types_acc, env_new))
          elements ([], env)
      in
      (TupleType element_types, env')
  | VarExpr name -> (
      try
        let ty = List.assoc name env in
        (ty, env)
      with Not_found ->
        raise
          (TypeError
             ("Unbound variable reference:\n" ^ "  Variable `" ^ name
            ^ "` is not in scope.")))
  | UnaryExpr { operator; operand } -> (
      let operand_type, env' = check_expr env func_env operand in
      match operator with
      | Not ->
          if not (type_eq operand_type (SymbolType { value = "bool" })) then
            raise
              (TypeError
                 ("Type error in unary `not` expression:\n"
                ^ "  Expected: bool\n" ^ "  Found:    "
                 ^ string_of_type operand_type));
          (SymbolType { value = "bool" }, env')
      | BitWiseNOT ->
          if not (type_eq operand_type (SymbolType { value = "int" })) then
            raise
              (TypeError
                 ("Type error in unary bitwise NOT expression:\n"
                ^ "  Expected: int\n" ^ "  Found:    "
                 ^ string_of_type operand_type));
          (operand_type, env')
      | Inc | Dec ->
          if
            not
              (type_eq operand_type (SymbolType { value = "int" })
              || type_eq operand_type (SymbolType { value = "float" }))
          then
            raise
              (TypeError
                 ("Type error in increment/decrement expression:\n"
                ^ "  Expected: int or float\n" ^ "  Found:    "
                 ^ string_of_type operand_type));
          (operand_type, env')
      | Minus ->
          if
            not
              (type_eq operand_type (SymbolType { value = "int" })
              || type_eq operand_type (SymbolType { value = "float" }))
          then
            raise
              (TypeError
                 ("Type error in unary minus expression:\n"
                ^ "  Expected: int or float\n" ^ "  Found:    "
                 ^ string_of_type operand_type));
          (operand_type, env')
      | _ -> raise (TypeError "Unsupported unary operator in expression"))
  | BinaryExpr { left; operator; right } -> (
      let lt, env1 = check_expr env func_env left in
      let rt, env2 = check_expr env1 func_env right in
      match operator with
      | Token.Eq | Token.Neq ->
          if not (can_compare lt rt) then
            raise
              (TypeError
                 ("Type error in equality expression:\n"
                ^ "  Left operand type:  " ^ string_of_type lt ^ "\n"
                ^ "  Right operand type: " ^ string_of_type rt ^ "\n"
                ^ "  Operands cannot be compared for equality."));
          (Type.SymbolType { value = "bool" }, env2)
      | Token.Geq | Token.Leq | Token.Less | Token.Greater | Token.LogicalAnd
      | Token.LogicalOr ->
          if not (type_eq lt rt) then
            raise
              (TypeError
                 ("Type error in binary expression:\n"
                ^ "  Left operand type:  " ^ string_of_type lt ^ "\n"
                ^ "  Right operand type: " ^ string_of_type rt ^ "\n"
                ^ "  Both operands must have the same type."));
          (Type.SymbolType { value = "bool" }, env2)
      | Token.BitWiseAND | Token.BitWiseOR | Token.BitWiseXOR | Token.LeftShift
      | Token.RightShift | Token.BitWiseANDAssign | Token.BitWiseORAssign
      | Token.BitWiseXORAssign | Token.LeftShiftAssign | Token.RightShiftAssign
        ->
          if not (type_eq lt (Type.SymbolType { value = "int" })) then
            raise
              (TypeError
                 ("Type error in binary bitwise expression:\n"
                ^ "  Expected: int\n" ^ "  Found:    " ^ string_of_type lt));
          (lt, env2)
      | _ ->
          if not (type_eq lt rt) then
            raise
              (TypeError
                 ("Type error in binary expression:\n"
                ^ "  Left operand type:  " ^ string_of_type lt ^ "\n"
                ^ "  Right operand type: " ^ string_of_type rt ^ "\n"
                ^ "  Both operands must have the same type."));
          (lt, env2))
  | CallExpr { callee = VarExpr name; arguments } -> (
      let env', arg_types =
        List.fold_left_map
          (fun acc_env arg ->
            let t, new_env = check_expr acc_env func_env arg in
            (new_env, t))
          env arguments
      in
      match List.assoc_opt name func_env with
      | Some overloads -> (
          let matching =
            List.find_opt
              (fun (params, _) ->
                List.length params = List.length arg_types
                && List.for_all2 type_eq params arg_types)
              overloads
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
          | Some (FunctionType (param_types, return_type)) ->
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
  | CallExpr _ ->
      raise
        (TypeError
           "Only calls to named functions (e.g. `foo(...)`) are currently \
            supported")
  | ArrayExpr { elements } -> (
      let final_env, element_types =
        List.fold_left_map
          (fun acc_env elem ->
            let t, new_env = check_expr acc_env func_env elem in
            (new_env, t))
          env elements
      in
      match element_types with
      | [] -> (ArrayType { element_type = Any }, final_env)
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
          (ArrayType { element_type = hd }, final_env))
  | IndexExpr { array; index } -> (
      let at, env' = check_expr env func_env array in
      let index_type, env'' = check_expr env' func_env index in
      let is_enum_type = function
        | SymbolType { value } -> Hashtbl.mem enum_variants value
        | _ -> false
      in

      if
        not
          (type_eq index_type (SymbolType { value = "int" })
          || is_enum_type index_type)
      then
        raise
          (TypeError
             ("Type error in index expression:\n"
            ^ "  Index must be an integer or enum value\n" ^ "  Found: "
            ^ string_of_type index_type));

      match at with
      | ArrayType { element_type } -> (element_type, env'')
      | TupleType element_types -> (
          match index with
          | IntExpr { value } ->
              let idx = Bigint.to_int value in
              if idx < 0 || idx >= List.length element_types then
                raise
                  (TypeError
                     ("Tuple index out of bounds:\n" ^ "  Index: "
                    ^ string_of_int idx ^ "\n" ^ "  Tuple size: "
                     ^ string_of_int (List.length element_types)));
              (List.nth element_types idx, env'')
          | _ ->
              raise
                (TypeError
                   ("Invalid tuple index:\n"
                  ^ "  Only constant integer indices (e.g. `t[0]`) are allowed"
                   )))
      | _ ->
          raise
            (TypeError
               ("Type error in index expression:\n"
              ^ "  Can only index into arrays or tuples\n" ^ "  Found: "
              ^ string_of_type at)))
  | IfExpr { condition; then_branch; else_branch } ->
      let ct, env' = check_expr env func_env condition in
      if not (type_eq ct (Type.SymbolType { value = "bool" })) then
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
  | DotExpr { left; right } -> (
      let left_type, env' = check_expr env func_env left in
      match left with
      | VarExpr name -> (
          match List.assoc_opt (name ^ "." ^ right) env' with
          | Some member_type -> (member_type, env')
          | None -> (
              match List.assoc_opt name env' with
              | Some (RecordType fields) -> (
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
          | RecordType fields -> (
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
  | TernaryExpr { cond; onTrue; onFalse } ->
      let ct, env1 = check_expr env func_env cond in
      if not (type_eq ct (SymbolType { value = "bool" })) then
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
  | PipelineExpr { left; right } -> (
      let arg_type, env1 = check_expr env func_env left in
      match right with
      | VarExpr name -> (
          match List.assoc_opt name func_env with
          | Some overloads -> (
              let matching =
                List.find_opt
                  (fun (params, _) ->
                    match params with
                    | [ param_type ] -> type_eq param_type arg_type
                    | _ -> false)
                  overloads
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
              | Some (FunctionType ([ param_type ], return_type)) ->
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
      | _ ->
          raise
            (TypeError
               ("Invalid pipeline usage:\n"
              ^ "  Right-hand side must be a function identifier (e.g., `value \
                 |> foo`)")))
  | MatchExpr { expr; cases } -> (
      let et, env1 = check_expr env func_env expr in
      match cases with
      | [] -> (SymbolType { value = "unit" }, env1)
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
          let final_env =
            List.fold_left
              (fun e stmt -> check_stmt e func_env stmt)
              env case_stmts
          in
          let first_branch_type =
            match List.rev case_stmts with
            | ExprStmt expr :: _ -> fst (check_expr final_env func_env expr)
            | _ -> SymbolType { value = "unit" }
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
              let branch_env =
                List.fold_left
                  (fun e stmt -> check_stmt e func_env stmt)
                  env case_stmts
              in
              let branch_type =
                match List.rev case_stmts with
                | ExprStmt expr :: _ ->
                    fst (check_expr branch_env func_env expr)
                | _ -> SymbolType { value = "unit" }
              in
              if not (type_eq branch_type first_branch_type) then
                raise
                  (TypeError
                     ("Type mismatch in match branches:\n" ^ "  Expected: "
                     ^ string_of_type first_branch_type
                     ^ "\n" ^ "  Found:    " ^ string_of_type branch_type)))
            rest;

          (first_branch_type, env1))
  | RangeExpr { start; end_ } -> (
      match (start, end_) with
      | Some s, Some e ->
          let t_s, env1 = check_expr env func_env s in
          let t_e, env2 = check_expr env1 func_env e in
          if
            (not (type_eq t_s (SymbolType { value = "int" })))
            || not (type_eq t_e (SymbolType { value = "int" }))
          then
            raise
              (TypeError
                 ("Range expression requires integer bounds:\n" ^ "  Found: "
                ^ string_of_type t_s ^ " and " ^ string_of_type t_e));
          (ArrayType { element_type = SymbolType { value = "int" } }, env2)
      | Some s, None ->
          let t_s, env1 = check_expr env func_env s in
          if not (type_eq t_s (SymbolType { value = "int" })) then
            raise
              (TypeError
                 ("Open-ended range `{x..}` requires integer start:\n"
                ^ "  Found: " ^ string_of_type t_s));
          (ArrayType { element_type = SymbolType { value = "int" } }, env1)
      | None, Some e ->
          let t_e, env1 = check_expr env func_env e in
          if not (type_eq t_e (SymbolType { value = "int" })) then
            raise
              (TypeError
                 ("Open-start range `{..x}` requires integer end:\n"
                ^ "  Found: " ^ string_of_type t_e));
          (ArrayType { element_type = SymbolType { value = "int" } }, env1)
      | None, None ->
          raise (TypeError "Invalid range expression: both bounds missing"))
  | BlockExpr { body } ->
      let rec check_stmts env func_env stmts =
        match stmts with
        | [] -> (Type.SymbolType { value = "unit" }, env)
        | [ last_stmt ] -> (
            let env' = check_stmt env func_env last_stmt in
            match last_stmt with
            | Stmt.ExprStmt expr ->
                let t, env'' = check_expr env' func_env expr in
                (t, env'')
            | _ -> (Type.SymbolType { value = "unit" }, env'))
        | hd :: tl ->
            let env' = check_stmt env func_env hd in
            check_stmts env' func_env tl
      in
      let block_type, env_after = check_stmts env func_env body in
      (block_type, env_after)
  | Expr.SliceExpr { array; start; end_ } -> (
      let array_type, env1 = check_expr env func_env array in
      match array_type with
      | Type.ArrayType _ ->
          let env2 =
            match start with
            | Some s -> (
                let s_ty, env' = check_expr env1 func_env s in
                match s_ty with
                | Type.SymbolType { value = "int" } -> env'
                | _ -> raise (TypeError "Slice start index must be of type int")
                )
            | None -> env1
          in
          let env3 =
            match end_ with
            | Some e -> (
                let e_ty, env' = check_expr env2 func_env e in
                match e_ty with
                | Type.SymbolType { value = "int" } -> env'
                | _ -> raise (TypeError "Slice end index must be of type int"))
            | None -> env2
          in
          (array_type, env3)
      | _ -> raise (TypeError "Attempted to slice a non-array value"))
  | VarDeclExpr { identifier; assigned_value; explicit_type } -> (
      match assigned_value with
      | Some expr ->
          let expr_type, env1 = check_expr env func_env expr in
          let final_type =
            match explicit_type with
            | Infer -> expr_type
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
          (SymbolType { value = "unit" }, env2)
      | None ->
          if explicit_type = Infer then
            raise
              (TypeError
                 ("Missing type annotation and initializer for `" ^ identifier
                ^ "`"));
          let env1 = (identifier, explicit_type) :: env in
          (SymbolType { value = "unit" }, env1))
  | MultiVarDeclExpr { identifier; assigned_value; explicit_type } ->
      let values, env1 =
        match assigned_value with
        | [ TupleExpr elements ] -> (elements, env)
        | [ VarExpr name ] -> (
            match List.assoc_opt name env with
            | Some (TupleType element_types) ->
                let exprs =
                  List.mapi
                    (fun i _ ->
                      IndexExpr
                        {
                          array = VarExpr name;
                          index = IntExpr { value = Bigint.of_int i };
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
            match expr_type with
            | TupleType element_types ->
                let exprs =
                  List.mapi
                    (fun i _ ->
                      IndexExpr
                        {
                          array = expr;
                          index = IntExpr { value = Bigint.of_int i };
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
        | TupleType declared_types ->
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
        | Infer ->
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
      (SymbolType { value = "unit" }, env3)
  | ImportExpr { module_name = mod_parts } -> (
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
  | ModuleExpr { module_name = _; block } ->
      List.fold_left
        (fun (_, env_acc) stmt -> check_expr env_acc func_env stmt)
        (SymbolType { value = "unit" }, env)
        block
  | EnumExpr { name; members } ->
      Hashtbl.replace enum_variants name members;
      let enum_type = SymbolType { value = name } in
      let new_env =
        List.fold_left
          (fun acc_env (_, member) ->
            (name ^ "." ^ member, enum_type) :: acc_env)
          env
          (List.mapi (fun i m -> (i, m)) members)
      in
      (enum_type, (name, enum_type) :: new_env)

and find_return_exprs env func_env expr =
  let open Expr in
  match expr with
  | IfExpr { condition; then_branch; else_branch } ->
      let _ = check_expr env func_env condition in
      let returns_then = find_return_exprs env func_env then_branch in
      let returns_else =
        match else_branch with
        | Some e -> find_return_exprs env func_env e
        | None -> []
      in
      returns_then @ returns_else
  | BinaryExpr { left; operator = _; right } ->
      find_return_exprs env func_env left @ find_return_exprs env func_env right
  | CallExpr { callee; arguments } ->
      find_return_exprs env func_env callee
      @ List.concat_map (find_return_exprs env func_env) arguments
  | ArrayExpr { elements } ->
      List.flatten (List.map (find_return_exprs env func_env) elements)
  | UnaryExpr { operand; _ } -> find_return_exprs env func_env operand
  | IndexExpr { array; index } ->
      find_return_exprs env func_env array
      @ find_return_exprs env func_env index
  | _ -> []

and collect_functions stmts =
  let rec collect_from_stmt stmt acc =
    match stmt with
    | FunctionDeclStmt { name; parameters; return_type; _ } ->
        let param_types = List.map (fun p -> p.param_type) parameters in
        let overload =
          match List.assoc_opt name acc with
          | Some overloads -> (name, (param_types, return_type) :: overloads)
          | None -> (name, [ (param_types, return_type) ])
        in
        overload :: List.remove_assoc name acc
    | BlockStmt { body } -> List.fold_right collect_from_stmt body acc
    | _ -> acc
  in
  List.fold_right collect_from_stmt stmts builtins

let typecheck_program stmts : bool =
  let env =
    [
      ("true", Type.SymbolType { value = "bool" });
      ("false", Type.SymbolType { value = "bool" });
    ]
  in
  let func_env = collect_functions stmts in
  ignore (List.fold_left (fun e stmt -> check_stmt e func_env stmt) env stmts);
  let used = !stdlib_used in
  stdlib_used := false;
  used
