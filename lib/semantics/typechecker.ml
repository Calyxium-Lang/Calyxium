open Syntax

module TypeChecker = struct
  module Env = Map.Make (String)

  type func_sig = { param_types : Ast.Type.t list; return_type : Ast.Type.t }

  let print_func_sig =
    {
      param_types = [ Ast.Type.Any ];
      return_type = Ast.Type.SymbolType { value = "void" };
    }

  let input_func_sig =
    { param_types = [ Ast.Type.Any ]; return_type = Ast.Type.Any }

  let println_func_sig =
    {
      param_types = [ Ast.Type.Any ];
      return_type = Ast.Type.SymbolType { value = "void" };
    }

  type class_info = {
    class_type : Ast.Type.t;
    properties : (string * Ast.Type.t) list;
  }

  type env = {
    var_type : Ast.Type.t Env.t;
    func_env : func_sig Env.t;
    class_env : class_info Env.t;
    modules : string list;
    exports : string list;
  }

  let len_func_sig =
    {
      param_types = [ Ast.Type.SymbolType { value = "string" } ];
      return_type = Ast.Type.SymbolType { value = "int" };
    }

  let to_string_func_sig =
    {
      param_types = [ Ast.Type.Any ];
      return_type = Ast.Type.SymbolType { value = "string" };
    }

  let to_int_func_sig =
    {
      param_types = [ Ast.Type.SymbolType { value = "float" } ];
      return_type = Ast.Type.SymbolType { value = "int" };
    }

  let to_float_func_sig =
    {
      param_types = [ Ast.Type.SymbolType { value = "int" } ];
      return_type = Ast.Type.SymbolType { value = "float" };
    }

  let register_builtin_functions env =
    let builtin_functions =
      [
        ("print", print_func_sig);
        ("println", println_func_sig);
        ("input", input_func_sig);
        ("len", len_func_sig);
        ("ToString", to_string_func_sig);
        ("ToInt", to_int_func_sig);
        ("ToFloat", to_float_func_sig);
      ]
    in
    {
      env with
      func_env =
        List.fold_left
          (fun acc (name, f_sig) -> Env.add name f_sig acc)
          env.func_env builtin_functions;
    }

  let empty_env =
    let base_env =
      {
        var_type = Env.empty;
        func_env = Env.empty;
        class_env = Env.empty;
        modules = [];
        exports = [];
      }
    in
    register_builtin_functions base_env

  let register_module_functions env module_name =
    match module_name with
    | "Time" ->
        let time_functions =
          [
            ( "current",
              {
                param_types = [];
                return_type = Ast.Type.SymbolType { value = "float" };
              } );
          ]
        in
        List.fold_left
          (fun acc (fname, f_sig) -> Env.add fname f_sig acc)
          env.func_env time_functions
    | _ -> env.func_env

  let load_module env module_name =
    let updated_func_env = register_module_functions env module_name in
    {
      env with
      func_env = updated_func_env;
      modules = module_name :: env.modules;
    }

  let check_function_call env function_name =
    try Env.find function_name env.func_env
    with Not_found ->
      let module_name, func_name =
        match String.split_on_char '.' function_name with
        | [ mod_name; fun_name ] -> (mod_name, fun_name)
        | _ -> failwith ("Unsupported function call: " ^ function_name)
      in
      if List.mem module_name env.modules then
        let updated_env = load_module env module_name in
        try Env.find func_name updated_env.func_env
        with Not_found ->
          failwith
            ("Function '" ^ func_name ^ "' not found in module '" ^ module_name
           ^ "'")
      else failwith ("Module '" ^ module_name ^ "' not imported")

  let lookup_var env name =
    try Env.find name env.var_type
    with Not_found -> failwith ("TypeChecker: Unbound variable " ^ name)

  let lookup_func env name =
    try Env.find name env.func_env
    with Not_found -> failwith ("TypeChecker: Unbound function " ^ name)

  let lookup_class env name =
    try Env.find name env.class_env
    with Not_found -> failwith ("TypeChecker: Unbound class " ^ name)

  let check_import env module_name =
    if List.mem module_name env.modules then
      failwith ("Module " ^ module_name ^ " already imported")
    else { env with modules = module_name :: env.modules }

  let check_export env identifier =
    if Env.mem identifier env.var_type then
      { env with exports = identifier :: env.exports }
    else
      failwith ("TypeChecker: Cannot export undefined identifier " ^ identifier)

  let rec check_expr env = function
    | Ast.Expr.IntExpr _ -> Ast.Type.SymbolType { value = "int" }
    | Ast.Expr.FloatExpr _ -> Ast.Type.SymbolType { value = "float" }
    | Ast.Expr.StringExpr _ -> Ast.Type.SymbolType { value = "string" }
    | Ast.Expr.ByteExpr _ -> Ast.Type.SymbolType { value = "byte" }
    | Ast.Expr.BoolExpr _ -> Ast.Type.SymbolType { value = "bool" }
    | Ast.Expr.VarExpr "true" | Ast.Expr.VarExpr "false" ->
        Ast.Type.SymbolType { value = "bool" }
    | Ast.Expr.VarExpr name -> lookup_var env name
    | Ast.Expr.ArrayExpr { elements } -> (
        match elements with
        | [] -> failwith "TypeChecker: Cannot infer type of an empty array"
        | first_elem :: _ ->
            let elem_type = check_expr env first_elem in
            List.iter
              (fun elem ->
                let t = check_expr env elem in
                if t <> elem_type then
                  failwith "TypeChecker: Type mismatch in array elements")
              elements;
            Ast.Type.ArrayType { element_type = elem_type })
    | Ast.Expr.IndexExpr { array; index } -> (
        let array_type = check_expr env array in
        let index_type = check_expr env index in
        if index_type <> Ast.Type.SymbolType { value = "int" } then
          failwith "TypeChecker: Array index must be an integer";
        match array_type with
        | Ast.Type.ArrayType { element_type } -> element_type
        | _ -> failwith "TypeChecker: Cannot index non-array type")
    | Ast.Expr.CallExpr { callee; arguments } ->
        let func_name =
          match callee with
          | Ast.Expr.VarExpr name -> name
          | _ -> failwith "TypeChecker: Unsupported function call"
        in
        let { param_types; return_type } = check_function_call env func_name in
        if List.length arguments <> List.length param_types then
          failwith
            ("TypeChecker: Incorrect number of arguments for function "
           ^ func_name);
        List.iter2
          (fun arg param_type ->
            let arg_type = check_expr env arg in
            if param_type <> Ast.Type.Any && param_type <> arg_type then
              failwith
                ("TypeChecker: Argument type mismatch in function call "
               ^ func_name))
          arguments param_types;
        return_type
    | Ast.Expr.BinaryExpr { left; operator; right } -> (
        let left_type = check_expr env left in
        let right_type = check_expr env right in
        match operator with
        | Ast.Assign -> (
            match left with
            | Ast.Expr.PropertyAccessExpr { object_name; property_name } -> (
                let obj_type = check_expr env object_name in
                match obj_type with
                | Ast.Type.ClassType { properties; _ } -> (
                    try
                      let expected_type = List.assoc property_name properties in
                      if expected_type <> right_type then
                        failwith
                          ("TypeChecker: Type mismatch in assignment to \
                            property " ^ property_name)
                      else left_type
                    with Not_found ->
                      failwith
                        ("TypeChecker: Undefined property: " ^ property_name))
                | _ -> failwith "TypeChecker: Assignment to non-object property"
                )
            | Ast.Expr.VarExpr name ->
                let var_type = lookup_var env name in
                if var_type <> right_type then
                  failwith
                    ("TypeChecker: Type mismatch in assignment to variable "
                   ^ name)
                else var_type
            | _ -> failwith "TypeChecker: Invalid left-hand side in assignment")
        | Ast.Eq | Ast.Neq | Ast.Less | Ast.Greater | Ast.Leq | Ast.Geq ->
            if left_type = right_type then
              Ast.Type.SymbolType { value = "bool" }
            else failwith "TypeChecker: Type mismatch in comparison expression"
        | Ast.Plus | Ast.Minus | Ast.Star | Ast.Slash | Ast.Mod | Ast.Pow ->
            if left_type = right_type then left_type
            else failwith "TypeChecker: Type mismatch in arithmetic expression"
        | Ast.Carot ->
            if
              left_type = Ast.Type.SymbolType { value = "string" }
              && right_type = Ast.Type.SymbolType { value = "string" }
            then Ast.Type.SymbolType { value = "string" }
            else
              failwith
                "TypeChecker: Type mismatch in string concatenation, both \
                 operands must be strings"
        | Ast.PlusAssign | Ast.MinusAssign | Ast.StarAssign | Ast.SlashAssign ->
            failwith
              "TypeChecker: Assignment operation cannot be used as a condition \
               in an if statement"
        | Ast.LogicalAnd | Ast.LogicalOr ->
            if
              (left_type = Ast.Type.SymbolType { value = "bool" }
              || left_type = Ast.Type.SymbolType { value = "int" })
              && (right_type = Ast.Type.SymbolType { value = "bool" }
                 || right_type = Ast.Type.SymbolType { value = "int" })
            then Ast.Type.SymbolType { value = "bool" }
            else failwith "TypeChecker: Type mismatch in logical expression"
        | _ -> failwith "TypeChecker: Unsupported operator in binary expression"
        )
    | Ast.Expr.PropertyAccessExpr { object_name; property_name } -> (
        let obj_type = check_expr env object_name in
        match obj_type with
        | Ast.Type.ClassType { properties; _ } -> (
            try List.assoc property_name properties
            with Not_found ->
              failwith ("TypeChecker: Undefined property: " ^ property_name))
        | _ -> failwith "TypeChecker: Property access on non-object type")
    | Ast.Expr.UnaryExpr { operator; operand } -> (
        let operand_type = check_expr env operand in
        match operator with
        | Ast.Not ->
            if operand_type = Ast.Type.SymbolType { value = "bool" } then
              Ast.Type.SymbolType { value = "bool" }
            else
              failwith "TypeChecker: Operand of NOT operator must be a boolean"
        | Ast.Inc ->
            if
              operand_type = Ast.Type.SymbolType { value = "int" }
              || operand_type = Ast.Type.SymbolType { value = "float" }
            then operand_type
            else
              failwith
                "TypeChecker: Operand of unary minus must be an integer or \
                 float"
        | Ast.Dec ->
            if
              operand_type = Ast.Type.SymbolType { value = "int" }
              || operand_type = Ast.Type.SymbolType { value = "float" }
            then operand_type
            else
              failwith
                "TypeChecker: Operand of unary plus must be an integer or float"
        | _ -> failwith "TypeChecker: Unsupported unary operator")
    | expr ->
        failwith ("TypeChecker: Unsupported expression: " ^ Ast.Expr.show expr)

  let check_var_decl env identifier explicit_type assigned_value =
    match assigned_value with
    | Some expr ->
        let value_type = check_expr env expr in
        if
          value_type = Ast.Type.Any
          || explicit_type = Ast.Type.Any
          || explicit_type = value_type
        then
          { env with var_type = Env.add identifier explicit_type env.var_type }
        else
          failwith
            ("TypeChecker: Type mismatch in variable declaration: " ^ identifier)
    | None -> failwith ("Variable " ^ identifier ^ " has no value assigned")

  let rec check_func_decl env name parameters return_type body =
    let param_types =
      List.map (fun param -> param.Ast.Stmt.param_type) parameters
    in
    let var_env =
      List.fold_left
        (fun var_env param ->
          Env.add param.Ast.Stmt.name param.Ast.Stmt.param_type var_env)
        env.var_type parameters
    in
    let func_sig = { param_types; return_type } in
    let func_env = Env.add name func_sig env.func_env in
    let new_env =
      {
        var_type = var_env;
        func_env;
        class_env = env.class_env;
        modules = env.modules;
        exports = env.exports;
      }
    in
    let _ = check_block new_env body ~expected_return_type:(Some return_type) in
    { env with func_env }

  and check_stmt env ~expected_return_type = function
    | Ast.Stmt.VarDeclarationStmt
        { identifier; constant = _; assigned_value; explicit_type } ->
        check_var_decl env identifier explicit_type assigned_value
    | Ast.Stmt.NewVarDeclarationStmt
        { identifier; constant = _; assigned_value; arguments } ->
        let class_name =
          match assigned_value with
          | Some (Ast.Expr.NewExpr { class_name; _ }) -> class_name
          | _ ->
              failwith
                ("TypeChecker: Expected a class instantiation for variable: "
               ^ identifier)
        in
        let class_info = lookup_class env class_name in

        if
          List.length arguments > 0
          && List.length arguments <> List.length class_info.properties
        then
          failwith
            ("TypeChecker: Incorrect number of arguments for class \
              instantiation: " ^ identifier);

        if List.length arguments > 0 then
          List.iter2
            (fun arg (prop_name, prop_type) ->
              let arg_type = check_expr env arg in
              if arg_type <> prop_type then
                failwith
                  ("TypeChecker: Type mismatch for property " ^ prop_name
                 ^ " in class " ^ class_name))
            arguments class_info.properties;

        {
          env with
          var_type = Env.add identifier class_info.class_type env.var_type;
        }
    | Ast.Stmt.FunctionDeclStmt { name; parameters; return_type; body } ->
        let return_type =
          match return_type with
          | Some t -> t
          | None ->
              failwith
                ("TypeChecker: Function " ^ name ^ " must have a return type")
        in
        check_func_decl env name parameters return_type body
    | Ast.Stmt.ClassDeclStmt { name; properties; methods = _ } ->
        let prop_list =
          List.map
            (fun param -> (param.Ast.Stmt.name, param.Ast.Stmt.param_type))
            properties
        in
        let class_info =
          {
            class_type = Ast.Type.ClassType { name; properties = prop_list };
            properties = prop_list;
          }
        in
        let class_env = Env.add name class_info env.class_env in
        { env with class_env }
    | Ast.Stmt.BlockStmt { body } -> check_block env body ~expected_return_type
    | Ast.Stmt.ReturnStmt expr -> (
        let return_type = check_expr env expr in
        match expected_return_type with
        | Some expected_type ->
            if return_type <> expected_type then
              failwith
                ("TypeChecker: Return type mismatch: expected "
                ^ Ast.Type.show expected_type
                ^ ", got " ^ Ast.Type.show return_type)
            else env
        | None -> env)
    | Ast.Stmt.ExprStmt expr ->
        let _ = check_expr env expr in
        env
    | Ast.Stmt.IfStmt { condition; then_branch; else_branch } ->
        let cond_type = check_expr env condition in
        if cond_type <> Ast.Type.SymbolType { value = "bool" } then
          failwith "TypeChecker: Condition in if statement must be a boolean"
        else
          let env_then = check_stmt env ~expected_return_type then_branch in
          let env_final =
            match else_branch with
            | Some else_branch ->
                check_stmt env_then ~expected_return_type else_branch
            | None -> env_then
          in
          env_final
    | Ast.Stmt.ForStmt { init; condition; increment; body } ->
        let env =
          match init with
          | Some stmt -> check_stmt env ~expected_return_type:None stmt
          | None -> env
        in
        let _ =
          let cond_type = check_expr env condition in
          if cond_type <> Ast.Type.SymbolType { value = "bool" } then
            failwith "TypeChecker: Condition in for statement must be a boolean"
        in
        let env =
          match increment with
          | Some stmt -> check_stmt env ~expected_return_type:None stmt
          | None -> env
        in
        check_block env [ body ] ~expected_return_type
    | Ast.Stmt.SwitchStmt { expr; cases; default_case } ->
        let switch_type = check_expr env expr in
        List.iter
          (fun (case_expr, case_body) ->
            let case_type = check_expr env case_expr in
            if case_type <> switch_type then
              failwith
                "TypeChecker: Case expression type does not match switch \
                 expression";
            ignore (check_block env case_body ~expected_return_type))
          cases;
        (match default_case with
        | Some body -> ignore (check_block env body ~expected_return_type)
        | None -> ());
        env
    | Ast.Stmt.ImportStmt { module_name } -> check_import env module_name
    | Ast.Stmt.ExportStmt { identifier } -> check_export env identifier

  and check_block env stmts ~expected_return_type =
    List.fold_left
      (fun env stmt -> check_stmt env stmt ~expected_return_type)
      env stmts
end
