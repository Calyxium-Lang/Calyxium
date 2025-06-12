open Ast

exception TypeError of string

let builtins : (string * (Type.t list * Type.t) list) list =
  [
    ("println", [ ([ Type.Any ], Type.SymbolType { value = "unit" }) ]);
    ( "input",
      [
        ( [ Type.SymbolType { value = "string" } ],
          Type.SymbolType { value = "unit" } );
      ] );
    ( "to_float",
      [
        ( [ Type.SymbolType { value = "string" } ],
          Type.SymbolType { value = "float" } );
        ( [ Type.SymbolType { value = "int" } ],
          Type.SymbolType { value = "float" } );
      ] );
    ( "to_int",
      [
        ( [ Type.SymbolType { value = "string" } ],
          Type.SymbolType { value = "int" } );
        ( [ Type.SymbolType { value = "float" } ],
          Type.SymbolType { value = "int" } );
      ] );
    ("to_string", [ ([ Type.Any ], Type.SymbolType { value = "string" }) ]);
  ]

let rec string_of_type = function
  | Ast.Type.Any -> "any"
  | Ast.Type.SymbolType { value } -> value
  | Ast.Type.ArrayType { element_type } ->
      "[" ^ string_of_type element_type ^ "]"

let rec type_eq expected actual =
  match (expected, actual) with
  | Type.SymbolType { value = "unit" }, _ -> true
  | Type.Any, _ | _, Type.Any -> true
  | Type.SymbolType { value = v1 }, Type.SymbolType { value = v2 } -> v1 = v2
  | Type.ArrayType { element_type = e1 }, Type.ArrayType { element_type = e2 }
    ->
      type_eq e1 e2
  | _ -> false

let rec check_expr (env : (string * Type.t) list)
    (func_env : (string * (Type.t list * Type.t) list) list) (expr : Expr.t) :
    Type.t =
  let open Expr in
  match expr with
  | IntExpr _ -> Type.SymbolType { value = "int" }
  | FloatExpr _ -> Type.SymbolType { value = "float" }
  | StringExpr _ -> Type.SymbolType { value = "string" }
  | BoolExpr _ -> Type.SymbolType { value = "bool" }
  | ByteExpr _ -> Type.SymbolType { value = "byte" }
  | UnitExpr _ -> Type.SymbolType { value = "unit" }
  | VarExpr name -> (
      try List.assoc name env
      with Not_found -> raise (TypeError ("Unbound variable: " ^ name)))
  | UnaryExpr { operator = _; operand } -> check_expr env func_env operand
  | BinaryExpr { left; operator; right } -> (
      let lt = check_expr env func_env left in
      let rt = check_expr env func_env right in
      if not (type_eq lt rt) then
        raise (TypeError "Binary operands must have same type");
      match operator with
      | Token.Eq | Token.Neq | Token.Geq | Token.Leq | Token.LogicalAnd
      | Token.LogicalOr | Token.Not | Token.Less | Token.Greater ->
          Type.SymbolType { value = "bool" }
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
                (TypeError ("Function argument type mismatch for `" ^ name ^ "`"))
          )
      | None -> raise (TypeError ("Unknown function: " ^ name)))
  | CallExpr _ ->
      raise (TypeError "Only simple function calls supported for now")
  | ArrayExpr { elements } -> (
      let types = List.map (check_expr env func_env) elements in
      match types with
      | [] -> Type.ArrayType { element_type = Type.Any }
      | hd :: tl ->
          List.iter
            (fun t ->
              if not (type_eq t hd) then
                raise (TypeError "Array element type mismatch"))
            tl;
          Type.ArrayType { element_type = hd })
  | IndexExpr { array; index } -> (
      let at = check_expr env func_env array in
      let _ = check_expr env func_env index in
      match at with
      | Type.ArrayType { element_type } -> element_type
      | _ -> raise (TypeError "Can only index into arrays"))
  | IfExpr { condition; then_branch; else_branch } ->
      let ct = check_expr env func_env condition in
      if not (type_eq ct (Type.SymbolType { value = "bool" })) then
        raise (TypeError "If condition must be boolean");
      let t_then = check_expr env func_env then_branch in
      let t_else = check_expr env func_env else_branch in
      if not (type_eq t_then t_else) then
        raise (TypeError "Branches of if must return same type");
      t_then
  | ReturnExpr expr -> check_expr env func_env expr

let rec find_return_exprs env func_env expr =
  let open Expr in
  match expr with
  | ReturnExpr e -> [ check_expr env func_env e ]
  | IfExpr { condition; then_branch; else_branch } ->
      let _ = check_expr env func_env condition in
      find_return_exprs env func_env then_branch
      @ find_return_exprs env func_env else_branch
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

let rec check_stmt (env : (string * Type.t) list)
    (func_env : (string * (Type.t list * Type.t) list) list) (stmt : Stmt.t) :
    (string * Type.t) list =
  let open Stmt in
  match stmt with
  | ExprStmt expr ->
      let _ = check_expr env func_env expr in
      env
  | VarDeclarationStmt { identifier; assigned_value; explicit_type } -> (
      match assigned_value with
      | Some expr ->
          let expr_type = check_expr env func_env expr in
          if not (type_eq expr_type explicit_type) then
            raise (TypeError ("Type mismatch in declaration of " ^ identifier));
          (identifier, explicit_type) :: env
      | None -> (identifier, explicit_type) :: env)
  | FunctionDeclStmt { name; parameters; return_type; body; _ } ->
      let param_types = List.map (fun p -> p.param_type) parameters in
      let new_func = (name, [ (param_types, return_type) ]) in
      let func_env =
        match List.assoc_opt name func_env with
        | Some overloads ->
            (name, (param_types, return_type) :: overloads)
            :: List.remove_assoc name func_env
        | None -> new_func :: func_env
      in
      let param_env = List.map (fun p -> (p.name, p.param_type)) parameters in
      let rec gather_return_types stmts =
        List.concat_map
          (function
            | Stmt.ExprStmt e -> find_return_exprs (param_env @ env) func_env e
            | Stmt.BlockStmt { body } -> gather_return_types body
            | Stmt.IfStmt { condition = _; then_branch; else_branch } ->
                let then_returns = gather_return_types [ then_branch ] in
                let else_returns =
                  match else_branch with
                  | Some b -> gather_return_types [ b ]
                  | None -> []
                in
                then_returns @ else_returns
            | _ -> [])
          stmts
      in
      let return_expr_types = gather_return_types body in
      List.iter
        (fun actual_type ->
          if not (type_eq return_type actual_type) then
            raise
              (TypeError
                 ("Function `" ^ name
                ^ "` has mismatched return type: expected "
                ^ string_of_type return_type ^ ", got "
                ^ string_of_type actual_type)))
        return_expr_types;
      env
  | BlockStmt { body } ->
      List.fold_left (fun e stmt -> check_stmt e func_env stmt) env body
  | IfStmt { condition; then_branch; else_branch } ->
      let ct = check_expr env func_env condition in
      if not (type_eq ct (Type.SymbolType { value = "bool" })) then
        raise (TypeError "If condition must be boolean");
      let _ = check_stmt env func_env then_branch in
      let _ =
        match else_branch with
        | Some b -> check_stmt env func_env b
        | None -> env
      in
      env
  | ForStmt { init; condition; increment; body } ->
      let env =
        match init with
        | Some stmt -> check_stmt env func_env stmt
        | None -> env
      in
      let ct = check_expr env func_env condition in
      if not (type_eq ct (Type.SymbolType { value = "bool" })) then
        raise (TypeError "For loop condition must be boolean");
      let _ = Option.map (check_stmt env func_env) increment in
      let _ = check_stmt env func_env body in
      env
  | ImportStmt { module_name = _ } -> env
  | ModuleStmt { module_name = _; block } ->
      let _ =
        List.fold_left (fun e stmt -> check_stmt e func_env stmt) env block
      in
      env
  | MatchStmt { expr; cases } ->
      let et = check_expr env func_env expr in
      List.iter
        (fun (pat_opt, case_stmts) ->
          (match pat_opt with
          | Some pat_expr ->
              let pt = check_expr env func_env pat_expr in
              if not (type_eq et pt) then
                raise (TypeError "Pattern type does not match match expression")
          | None -> ());
          ignore
            (List.fold_left
               (fun e stmt -> check_stmt e func_env stmt)
               env case_stmts))
        cases;
      env

let rec collect_functions stmts =
  let rec collect_from_stmt stmt acc =
    match stmt with
    | Stmt.FunctionDeclStmt { name; parameters; return_type; _ } ->
        let param_types = List.map (fun p -> p.Stmt.param_type) parameters in
        let overload =
          match List.assoc_opt name acc with
          | Some overloads -> (name, (param_types, return_type) :: overloads)
          | None -> (name, [ (param_types, return_type) ])
        in
        overload :: List.remove_assoc name acc
    | Stmt.BlockStmt { body } -> collect_functions body @ acc
    | Stmt.IfStmt { then_branch; else_branch; _ } -> (
        let acc = collect_from_stmt then_branch acc in
        match else_branch with
        | Some else_stmt -> collect_from_stmt else_stmt acc
        | None -> acc)
    | _ -> acc
  in
  List.fold_right collect_from_stmt stmts builtins

let typecheck_program (stmts : Stmt.t list) =
  let env =
    [
      ("true", Type.SymbolType { value = "bool" });
      ("false", Type.SymbolType { value = "bool" });
    ]
  in
  let func_env = collect_functions stmts in
  ignore (List.fold_left (fun e stmt -> check_stmt e func_env stmt) env stmts)
