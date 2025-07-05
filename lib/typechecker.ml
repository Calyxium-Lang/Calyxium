open Ast
open Ast.Type
open Ast.Stmt
open Token

exception TypeError of string

let builtins : (string * (Type.t list * Type.t) list) list =
  [
    ("println", [ ([ Any ], SymbolType { value = "unit" }) ]);
    ( "input",
      [ ([ SymbolType { value = "string" } ], SymbolType { value = "unit" }) ]
    );
    ( "to_float",
      [
        ([ SymbolType { value = "string" } ], SymbolType { value = "float" });
        ([ SymbolType { value = "int" } ], SymbolType { value = "float" });
      ] );
    ( "to_int",
      [
        ([ SymbolType { value = "string" } ], SymbolType { value = "int" });
        ([ SymbolType { value = "float" } ], SymbolType { value = "int" });
      ] );
    ("to_string", [ ([ Any ], SymbolType { value = "string" }) ]);
  ]

let rec string_of_type = function
  | Any -> "any"
  | SymbolType { value } -> value
  | ArrayType { element_type } -> "[" ^ string_of_type element_type ^ "]"

let rec type_eq expected actual =
  match (expected, actual) with
  | SymbolType { value = "unit" }, _ -> true
  | Any, _ | _, Any -> true
  | SymbolType { value = v1 }, SymbolType { value = v2 } -> v1 = v2
  | ArrayType { element_type = e1 }, ArrayType { element_type = e2 } ->
      type_eq e1 e2
  | _ -> false

let rec check_expr (env : (string * Type.t) list)
    (func_env : (string * (Type.t list * Type.t) list) list) (expr : Expr.t) :
    Type.t =
  let open Expr in
  match expr with
  | IntExpr _ -> SymbolType { value = "int" }
  | FloatExpr _ -> SymbolType { value = "float" }
  | StringExpr _ -> SymbolType { value = "string" }
  | BoolExpr _ -> SymbolType { value = "bool" }
  | ByteExpr _ -> SymbolType { value = "byte" }
  | UnitExpr _ -> SymbolType { value = "unit" }
  | VarExpr name -> (
      try List.assoc name env
      with Not_found -> raise (TypeError ("Unbound variable: " ^ name)))
  | UnaryExpr { operator; operand } -> (
      let operand_type = check_expr env func_env operand in
      match operator with
      | Not ->
          if not (type_eq operand_type (SymbolType { value = "bool" })) then
            raise (TypeError "Unary `not` operator requires a boolean operand");
          SymbolType { value = "bool" }
      | Inc | Dec ->
          if
            not
              (type_eq operand_type (SymbolType { value = "int" })
              || type_eq operand_type (SymbolType { value = "float" }))
          then
            raise
              (TypeError "Increment/Decrement requires int or float operand");
          operand_type
      | Minus ->
          if
            not
              (type_eq operand_type (SymbolType { value = "int" })
              || type_eq operand_type (SymbolType { value = "float" }))
          then
            raise
              (TypeError "Increment/Decrement requires int or float operand");
          operand_type
      | _ -> raise (TypeError "Unsupported unary operator"))
  | BinaryExpr { left; operator; right } -> (
      let lt = check_expr env func_env left in
      let rt = check_expr env func_env right in
      if not (type_eq lt rt) then
        raise (TypeError "Binary operands must have the same type");
      match operator with
      | Eq | Neq | Geq | Leq | LogicalAnd | LogicalOr | Less | Greater ->
          SymbolType { value = "bool" }
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
      | [] -> ArrayType { element_type = Any }
      | hd :: tl ->
          List.iter
            (fun t ->
              if not (type_eq t hd) then
                raise (TypeError "Array element type mismatch"))
            tl;
          ArrayType { element_type = hd })
  | IndexExpr { array; index } -> (
      let at = check_expr env func_env array in
      let _ = check_expr env func_env index in
      match at with
      | ArrayType { element_type } -> element_type
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
  | DotExpr _ -> raise (TypeError "Dot Expression not implemented")
  | TernaryExpr { cond; onTrue; onFalse } ->
      let ct = check_expr env func_env cond in
      if not (type_eq ct (SymbolType { value = "bool" })) then
        raise (TypeError "Ternary condition must be boolean");
      let t_true = check_expr env func_env onTrue in
      let t_false = check_expr env func_env onFalse in
      if type_eq t_true t_false then t_true
      else
        raise
          (TypeError
             ("Ternary branches must return same type, but got "
            ^ string_of_type t_true ^ " and " ^ string_of_type t_false))

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
        | IfExpr { condition; then_branch; else_branch } ->
            contains_recursive_call fname condition
            || contains_recursive_call fname then_branch
            || contains_recursive_call fname else_branch
        | ArrayExpr { elements } ->
            List.exists (contains_recursive_call fname) elements
        | IndexExpr { array; index } ->
            contains_recursive_call fname array
            || contains_recursive_call fname index
        | ReturnExpr e -> contains_recursive_call fname e
        | _ -> false
      in

      let rec contains_recursive_call_stmt fname stmt =
        let open Stmt in
        match stmt with
        | ExprStmt e -> contains_recursive_call fname e
        | BlockStmt { body } ->
            List.exists (contains_recursive_call_stmt fname) body
        | IfStmt { condition; then_branch; else_branch } -> (
            contains_recursive_call fname condition
            || contains_recursive_call_stmt fname then_branch
            ||
            match else_branch with
            | Some b -> contains_recursive_call_stmt fname b
            | None -> false)
        | ForStmt { init; condition; increment; body } ->
            (match init with
            | Some s -> contains_recursive_call_stmt fname s
            | None -> false)
            || contains_recursive_call fname condition
            || (match increment with
               | Some s -> contains_recursive_call_stmt fname s
               | None -> false)
            || contains_recursive_call_stmt fname body
        | _ -> false
      in

      let has_recursive_call =
        List.exists (contains_recursive_call_stmt name) body
      in

      if has_recursive_call && not is_rec then
        raise
          (TypeError
             ("Function `" ^ name
            ^ "` calls itself recursively but is not marked `rec`. Please add \
               `rec`."));

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
            | IfStmt { condition = _; then_branch; else_branch } ->
                let then_returns =
                  gather_return_types env func_env [ then_branch ]
                in
                let else_returns =
                  match else_branch with
                  | Some b -> gather_return_types env func_env [ b ]
                  | None -> []
                in
                then_returns @ else_returns
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
                 ("Function `" ^ name
                ^ "` has mismatched return type: expected "
                ^ string_of_type return_type ^ ", got "
                ^ string_of_type actual_type)))
        all_return_types;
      env
  | BlockStmt { body } ->
      let _final_env =
        List.fold_left (fun e stmt -> check_stmt e func_env stmt) env body
      in
      _final_env
  | IfStmt { condition; then_branch; else_branch } ->
      let ct = check_expr env func_env condition in
      if not (type_eq ct (SymbolType { value = "bool" })) then
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
      if not (type_eq ct (SymbolType { value = "bool" })) then
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
    | IfStmt { then_branch; else_branch; _ } -> (
        let acc = collect_from_stmt then_branch acc in
        match else_branch with
        | Some else_stmt -> collect_from_stmt else_stmt acc
        | None -> acc)
    | ForStmt { init; body; increment; _ } ->
        let acc =
          match init with Some s -> collect_from_stmt s acc | None -> acc
        in
        let acc =
          match increment with Some s -> collect_from_stmt s acc | None -> acc
        in
        collect_from_stmt body acc
    | ModuleStmt { block; _ } -> List.fold_right collect_from_stmt block acc
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
