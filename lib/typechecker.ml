open Ast
open Ast.Type
open Ast.Stmt
open Ast.Expr
open Token

exception TypeError of string

let enum_variants : (string, string list) Hashtbl.t = Hashtbl.create 10
let stdlib_used = ref false
let float_type = SymbolType { value = "float" }

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
  | StructType fields ->
      let field_strs =
        List.map (fun (name, ty) -> name ^ ": " ^ string_of_type ty) fields
      in
      "struct { " ^ String.concat "; " field_strs ^ " }"

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
        ("sin", FunctionType ([ float_type ], float_type));
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
      let _ = check_expr env func_env expr in
      env
  | VarDeclarationStmt { identifier; assigned_value; explicit_type } -> (
      match assigned_value with
      | Some expr ->
          let expr_type = check_expr env func_env expr in
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
          (identifier, final_type) :: env
      | None ->
          if explicit_type = Infer then
            raise
              (TypeError
                 ("Missing type annotation and initializer for `" ^ identifier
                ^ "`"));
          (identifier, explicit_type) :: env)
  | MultiVarDeclarationStmt { identifier; assigned_value; explicit_type } -> (
      let values =
        match assigned_value with
        | [ TupleExpr elements ] -> elements
        | [ VarExpr name ] -> (
            match List.assoc_opt name env with
            | Some (TupleType element_types) ->
                List.mapi
                  (fun i _ ->
                    IndexExpr
                      {
                        array = VarExpr name;
                        index = Int64Expr { value = Int64.of_int i };
                      })
                  element_types
            | Some t ->
                raise
                  (TypeError
                     ("Cannot destructure non-tuple variable `" ^ name
                    ^ "`: expected tuple, but got " ^ string_of_type t))
            | None -> raise (TypeError ("Unbound variable `" ^ name ^ "`")))
        | [ expr ] -> (
            let expr_type = check_expr env func_env expr in
            match expr_type with
            | TupleType element_types ->
                List.mapi
                  (fun i _ ->
                    IndexExpr
                      {
                        array = expr;
                        index = Int64Expr { value = Int64.of_int i };
                      })
                  element_types
            | _ ->
                raise
                  (TypeError
                     ("Cannot destructure non-tuple expression: expected \
                       tuple, but got " ^ string_of_type expr_type)))
        | _ -> assigned_value
      in
      let id_count = List.length identifier in
      let val_count = List.length values in
      if id_count <> val_count then
        raise
          (TypeError
             ("Tuple destructure mismatch: expected " ^ string_of_int id_count
            ^ " values, but got " ^ string_of_int val_count));
      let inferred_types =
        List.mapi
          (fun i ident ->
            let expr = List.nth values i in
            let inferred = check_expr env func_env expr in
            (ident, inferred))
          identifier
      in
      match explicit_type with
      | TupleType declared_types ->
          let type_count = List.length declared_types in
          if type_count <> id_count then
            raise
              (TypeError
                 ("Declared tuple type has " ^ string_of_int type_count
                ^ " elements, but destructure has " ^ string_of_int id_count
                ^ " identifiers"));
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
            env identifier declared_types
      | Infer ->
          List.fold_left
            (fun acc_env (ident, ty) -> (ident, ty) :: acc_env)
            env inferred_types
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
            env inferred_types)
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
      let _final_env =
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
      let return_expr_types = gather_return_types _final_env func_env body in
      let last_expr_type =
        match
          List.rev body
          |> List.find_opt (function ExprStmt _ -> true | _ -> false)
        with
        | Some (ExprStmt expr) -> Some (check_expr _final_env func_env expr)
        | _ -> None
      in

      let all_return_types =
        match last_expr_type with
        | Some t -> if return_expr_types = [] then [ t ] else return_expr_types
        | None -> return_expr_types
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
  | ImportStmt { module_name = mod_parts } -> (
      match mod_parts with
      | [ mod_name; symbol ] -> (
          if List.mem_assoc mod_name built_in_modules then stdlib_used := true;
          match List.assoc_opt mod_name built_in_modules with
          | Some mod_entries -> (
              match List.assoc_opt symbol mod_entries with
              | Some ty -> (symbol, ty) :: env
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
  | ModuleStmt { module_name = _; block } ->
      let _ =
        List.fold_left (fun e stmt -> check_stmt e func_env stmt) env block
      in
      env
  | EnumStmt { name; members } ->
      Hashtbl.replace enum_variants name members;
      let enum_type = SymbolType { value = name } in
      let new_env =
        List.fold_left
          (fun acc_env (_, member) ->
            (name ^ "." ^ member, enum_type) :: acc_env)
          env
          (List.mapi (fun i m -> (i, m)) members)
      in
      (name, enum_type) :: new_env
  | StructStmt { name; fields } ->
      let struct_env =
        List.fold_left
          (fun acc_env field_stmt ->
            match field_stmt with
            | VarDeclarationStmt _ -> check_stmt acc_env func_env field_stmt
            | _ ->
                raise
                  (TypeError
                     ("Invalid statement in struct `" ^ name
                    ^ "`: only variable delcarations are allowed")))
          [] fields
      in
      let struct_type =
        let fields_as_types =
          List.rev_map (fun (id, ty) -> (id, ty)) struct_env
        in
        StructType fields_as_types
      in
      (name, struct_type) :: env

and check_expr env func_env expr =
  match expr with
  | Int64Expr _ -> SymbolType { value = "int" }
  | FloatExpr _ -> SymbolType { value = "float" }
  | StringExpr _ -> SymbolType { value = "string" }
  | BoolExpr _ -> SymbolType { value = "bool" }
  | ByteExpr _ -> SymbolType { value = "byte" }
  | UnitExpr _ -> SymbolType { value = "unit" }
  | TupleExpr elements ->
      let element_types = List.map (check_expr env func_env) elements in
      TupleType element_types
  | VarExpr name -> (
      try List.assoc name env
      with Not_found ->
        raise
          (TypeError
             ("Unbound variable reference:\n" ^ "  Variable `" ^ name
            ^ "` is not in scope.")))
  | UnaryExpr { operator; operand } -> (
      let operand_type = check_expr env func_env operand in
      match operator with
      | Not ->
          if not (type_eq operand_type (SymbolType { value = "bool" })) then
            raise
              (TypeError
                 ("Type error in unary `not` expression:\n"
                ^ "  Expected: bool\n" ^ "  Found:    "
                 ^ string_of_type operand_type));
          SymbolType { value = "bool" }
      | BitWiseNOT ->
          if not (type_eq operand_type (SymbolType { value = "int" })) then
            raise
              (TypeError
                 ("Type error in unary bitwise NOT expression:\n"
                ^ "  Expected: int\n" ^ "  Found:    "
                 ^ string_of_type operand_type));
          operand_type
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
          operand_type
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
          operand_type
      | _ -> raise (TypeError "Unsupported unary operator in expression"))
  | BinaryExpr { left; operator; right } -> (
      let lt = check_expr env func_env left in
      let rt = check_expr env func_env right in
      if not (type_eq lt rt) then
        raise
          (TypeError
             ("Type error in binary expression:\n" ^ "  Left operand type:  "
            ^ string_of_type lt ^ "\n" ^ "  Right operand type: "
            ^ string_of_type rt ^ "\n"
            ^ "  Both operands must have the same type."));
      match operator with
      | Eq | Neq | Geq | Leq | LogicalAnd | LogicalOr | Less | Greater ->
          SymbolType { value = "bool" }
      | BitWiseAND | BitWiseOR | BitWiseXOR | LeftShift | RightShift
      | RightShiftLogical | BitWiseANDAssign | BitWiseORAssign
      | BitWiseXORAssign | LeftShiftAssign | RightShiftAssign ->
          if not (type_eq lt (SymbolType { value = "int" })) then
            raise
              (TypeError
                 ("Type error in binary bitwise expression:\n"
                ^ "  Expected: int\n" ^ "  Found:    " ^ string_of_type lt));
          lt
      | _ -> lt)
  | CallExpr { callee = VarExpr name; arguments } -> (
      match List.assoc_opt name func_env with
      | Some overloads -> (
          let arg_types = List.map (check_expr env func_env) arguments in
          let matching =
            List.find_opt
              (fun (params, _) ->
                List.length params = List.length arg_types
                && List.for_all2 type_eq params arg_types)
              overloads
          in
          match matching with
          | Some (_, return_type) -> return_type
          | None ->
              raise
                (TypeError
                   ("Function call argument mismatch for `" ^ name ^ "`:\n"
                  ^ "  No matching overload found for arguments:\n" ^ "  "
                   ^ String.concat ", " (List.map string_of_type arg_types))))
      | None -> (
          match List.assoc_opt name env with
          | Some (FunctionType (param_types, return_type)) ->
              let arg_types = List.map (check_expr env func_env) arguments in
              if
                List.length param_types = List.length arg_types
                && List.for_all2 type_eq param_types arg_types
              then return_type
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
      let types = List.map (check_expr env func_env) elements in
      match types with
      | [] -> ArrayType { element_type = Any }
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
          ArrayType { element_type = hd })
  | IndexExpr { array; index } -> (
      let at = check_expr env func_env array in
      let index_type = check_expr env func_env index in
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
      | ArrayType { element_type } -> element_type
      | TupleType element_types -> (
          match index with
          | Int64Expr { value } ->
              let idx = Int64.to_int value in
              if idx < 0 || idx >= List.length element_types then
                raise
                  (TypeError
                     ("Tuple index out of bounds:\n" ^ "  Index: "
                    ^ string_of_int idx ^ "\n" ^ "  Tuple size: "
                     ^ string_of_int (List.length element_types)));
              List.nth element_types idx
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
      let ct = check_expr env func_env condition in
      if not (type_eq ct (Type.SymbolType { value = "bool" })) then
        raise
          (TypeError
             ("Type error in `if` expression condition:\n"
            ^ "  Expected: bool\n" ^ "  Found:    " ^ string_of_type ct));
      let t_then = check_expr env func_env then_branch in
      let t_else =
        match else_branch with
        | Some e -> check_expr env func_env e
        | None -> t_then
      in
      if not (type_eq t_then t_else) then
        raise
          (TypeError
             ("Type mismatch in `if` expression branches:\n" ^ "  Then branch: "
            ^ string_of_type t_then ^ "\n" ^ "  Else branch: "
            ^ string_of_type t_else));
      t_then
  | DotExpr { left; right } -> (
      let left_type = check_expr env func_env left in
      match left with
      | VarExpr name -> (
          match List.assoc_opt (name ^ "." ^ right) env with
          | Some member_type -> member_type
          | None -> (
              match List.assoc_opt name env with
              | Some (StructType fields) -> (
                  match List.assoc_opt right fields with
                  | Some ty -> ty
                  | None ->
                      raise
                        (TypeError
                           ("Unknown struct field:\n" ^ "  `" ^ right
                          ^ "` is not a field of struct `" ^ name ^ "`")))
              | Some _ ->
                  raise
                    (TypeError
                       ("Cannot access `" ^ right ^ "` on non-struct value `"
                      ^ name ^ "`"))
              | None ->
                  raise
                    (TypeError
                       ("Unknown enum or struct:\n" ^ "  `" ^ name
                      ^ "` is not defined"))))
      | _ -> (
          match left_type with
          | StructType fields -> (
              match List.assoc_opt right fields with
              | Some ty -> ty
              | None ->
                  raise
                    (TypeError
                       ("Unknown struct field:\n" ^ "  `" ^ right
                      ^ "` is not a field of the struct")))
          | _ ->
              raise
                (TypeError
                   ("Dot access error:\n" ^ "  Cannot access field `" ^ right
                  ^ "` on non-struct or non-enum expression"))))
  | TernaryExpr { cond; onTrue; onFalse } ->
      let ct = check_expr env func_env cond in
      if not (type_eq ct (SymbolType { value = "bool" })) then
        raise
          (TypeError
             ("Type error in ternary condition:\n" ^ "  Expected: bool\n"
            ^ "  Found:    " ^ string_of_type ct));
      let t_true = check_expr env func_env onTrue in
      let t_false = check_expr env func_env onFalse in
      if type_eq t_true t_false then t_true
      else
        raise
          (TypeError
             ("Type mismatch in ternary branches:\n" ^ "  True branch:  "
            ^ string_of_type t_true ^ "\n" ^ "  False branch: "
            ^ string_of_type t_false))
  | PipelineExpr { left; right } -> (
      let arg_type = check_expr env func_env left in
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
              | Some (_, return_type) -> return_type
              | None ->
                  raise
                    (TypeError
                       ("No matching overload for pipeline call to `" ^ name
                      ^ "`:\n" ^ "  Argument type: " ^ string_of_type arg_type))
              )
          | None -> (
              match List.assoc_opt name env with
              | Some (FunctionType ([ param_type ], return_type)) ->
                  if type_eq param_type arg_type then return_type
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
      let et = check_expr env func_env expr in
      let branch_types =
        List.map
          (fun (pat_opt, case_stmts) ->
            (match pat_opt with
            | Some pat_expr ->
                let pt = check_expr env func_env pat_expr in
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
            match List.rev case_stmts with
            | ExprStmt expr :: _ -> check_expr final_env func_env expr
            | _ -> SymbolType { value = "unit" })
          cases
      in
      match branch_types with
      | [] -> SymbolType { value = "unit" }
      | first :: rest ->
          List.iter
            (fun t ->
              if not (type_eq t first) then
                raise
                  (TypeError
                     ("Type mismatch in match branches:\n" ^ "  Expected: "
                    ^ string_of_type first ^ "\n" ^ "  Found:    "
                    ^ string_of_type t)))
            rest;
          first)
  | RangeExpr { start; end_ } -> (
      match (start, end_) with
      | Some s, Some e ->
          let t_s = check_expr env func_env s in
          let t_e = check_expr env func_env e in
          if
            (not (type_eq t_s (SymbolType { value = "int" })))
            || not (type_eq t_e (SymbolType { value = "int" }))
          then
            raise
              (TypeError
                 ("Range expression requires integer bounds:\n" ^ "  Found: "
                ^ string_of_type t_s ^ " and " ^ string_of_type t_e));
          ArrayType { element_type = SymbolType { value = "int" } }
      | Some s, None ->
          let t_s = check_expr env func_env s in
          if not (type_eq t_s (SymbolType { value = "int" })) then
            raise
              (TypeError
                 ("Open-ended range `{x..}` requires integer start:\n"
                ^ "  Found: " ^ string_of_type t_s));
          ArrayType { element_type = SymbolType { value = "int" } }
      | None, Some e ->
          let t_e = check_expr env func_env e in
          if not (type_eq t_e (SymbolType { value = "int" })) then
            raise
              (TypeError
                 ("Open-start range `{..x}` requires integer end:\n"
                ^ "  Found: " ^ string_of_type t_e));
          ArrayType { element_type = SymbolType { value = "int" } }
      | None, None ->
          raise (TypeError "Invalid range expression: both bounds missing"))
  | BlockExpr { body } ->
      let rec check_stmts env func_env stmts =
        match stmts with
        | [] -> (env, Type.SymbolType { value = "unit" })
        | [ last_stmt ] -> (
            let env' = check_stmt env func_env last_stmt in
            match last_stmt with
            | Stmt.ExprStmt expr -> (env', check_expr env' func_env expr)
            | _ -> (env', Type.SymbolType { value = "unit" }))
        | hd :: tl ->
            let env' = check_stmt env func_env hd in
            check_stmts env' func_env tl
      in
      let _env_after, block_type = check_stmts env func_env body in
      block_type
  | Expr.SliceExpr { array; start; end_ } -> (
      let array_type = check_expr env func_env array in
      match array_type with
      | Type.ArrayType _ ->
          let () =
            match start with
            | Some s -> (
                let s_ty = check_expr env func_env s in
                match s_ty with
                | Type.SymbolType { value = "int" } -> ()
                | _ -> raise (TypeError "Slice start index must be of type int")
                )
            | None -> ()
          in
          let () =
            match end_ with
            | Some e -> (
                let e_ty = check_expr env func_env e in
                match e_ty with
                | Type.SymbolType { value = "int" } -> ()
                | _ -> raise (TypeError "Slice end index must be of type int"))
            | None -> ()
          in
          array_type
      | _ -> raise (TypeError "Attempted to slice a non-array value"))

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
