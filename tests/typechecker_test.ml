open Calyxiumlib

let var_decl ?assigned_type name expr =
  Calyxiumlib.Ast.Expr.VarDeclExpr
    {
      identifier = name;
      assigned_value = expr;
      explicit_type =
        (match assigned_type with
        | Some t -> t
        | None -> Calyxiumlib.Ast.Type.Infer);
    }

let int_expr i =
  Calyxiumlib.Ast.Expr.IntExpr { value = Calyxiumlib.Bigint.of_int i }

let test_case name f =
  try
    f ();
    Printf.printf "Passed: %s\n" name
  with
  | Types.TypeError msg ->
      Printf.printf "[FAIL] %s: %s\n" name msg;
      exit 1
  | e ->
      Printf.printf "[ERROR] %s: unexpected exception: %s\n" name
        (Printexc.to_string e)

let empty_env = []
let empty_func_env = []

let int_expr_infer i =
  Calyxiumlib.Ast.Expr.IntExpr { value = Calyxiumlib.Bigint.of_int i }

let () =
  print_endline "Typechecker Testing";

  test_case "int var declaration" (fun () ->
      let stmt =
        Calyxiumlib.Ast.Stmt.ExprStmt (var_decl "x" (Some (int_expr 42)))
      in
      let env, _ =
        Calyxiumlib.Check_expr.check_stmt empty_env empty_func_env stmt
      in
      match List.assoc_opt "x" env with
      | Some (Calyxiumlib.Ast.Type.SymbolType { value = "int" }) -> ()
      | _ -> failwith "x has wrong type");

  test_case "mismatched type declaration" (fun () ->
      let stmt =
        Calyxiumlib.Ast.Stmt.ExprStmt
          (var_decl "y"
             (Some (Calyxiumlib.Ast.Expr.StringExpr { value = "oops" })))
      in
      let _ =
        Calyxiumlib.Check_expr.check_stmt
          [ ("y", Calyxiumlib.Ast.Type.SymbolType { value = "int" }) ]
          empty_func_env stmt
      in
      ());

  test_case "simple function" (fun () ->
      let fn_stmt =
        Calyxiumlib.Ast.Stmt.FunctionDeclStmt
          {
            name = "add";
            is_rec = false;
            parameters =
              [
                {
                  Calyxiumlib.Ast.Stmt.name = "a";
                  param_type = Calyxiumlib.Ast.Type.SymbolType { value = "int" };
                };
                {
                  Calyxiumlib.Ast.Stmt.name = "b";
                  param_type = Calyxiumlib.Ast.Type.SymbolType { value = "int" };
                };
              ];
            return_type = Calyxiumlib.Ast.Type.SymbolType { value = "int" };
            body =
              [
                Calyxiumlib.Ast.Stmt.ExprStmt
                  (Calyxiumlib.Ast.Expr.BinaryExpr
                     {
                       left = Calyxiumlib.Ast.Expr.VarExpr "a";
                       operator = Calyxiumlib.Token.Plus;
                       right = Calyxiumlib.Ast.Expr.VarExpr "b";
                     });
              ];
          }
      in
      let _, func_env =
        Calyxiumlib.Check_expr.check_stmt empty_env empty_func_env fn_stmt
      in
      match List.assoc_opt "add" func_env with
      | Some _ -> ()
      | None -> failwith "Function not added to func_env");

  test_case "int literal type" (fun () ->
      let expr = int_expr 100 in
      let typ, _env =
        Calyxiumlib.Check_expr.check_expr empty_env empty_func_env expr
      in
      match typ with
      | Calyxiumlib.Ast.Type.SymbolType { value = "int" } -> ()
      | _ -> failwith "Int literal inferred incorrectly");

  test_case "string literal type" (fun () ->
      let expr = Calyxiumlib.Ast.Expr.StringExpr { value = "hello" } in
      let typ, _env =
        Calyxiumlib.Check_expr.check_expr empty_env empty_func_env expr
      in
      match typ with
      | Calyxiumlib.Ast.Type.SymbolType { value = "string" } -> ()
      | _ -> failwith "String literal inferred incorrectly");

  test_case "bool literal" (fun () ->
      let expr = Calyxiumlib.Ast.Expr.BoolExpr { value = true } in
      let typ, _env =
        Calyxiumlib.Check_expr.check_expr empty_env empty_func_env expr
      in
      match typ with
      | Calyxiumlib.Ast.Type.SymbolType { value = "bool" } -> ()
      | _ -> failwith "Bool literal inferred incorrectly");

  test_case "int addition" (fun () ->
      let expr =
        Calyxiumlib.Ast.Expr.BinaryExpr
          {
            left = int_expr 1;
            operator = Calyxiumlib.Token.Plus;
            right = int_expr 2;
          }
      in
      let typ, _env =
        Calyxiumlib.Check_expr.check_expr empty_env empty_func_env expr
      in
      match typ with
      | Calyxiumlib.Ast.Type.SymbolType { value = "int" } -> ()
      | _ -> failwith "Int addition type incorrect");

  test_case "if expression" (fun () ->
      let expr =
        Calyxiumlib.Ast.Expr.IfExpr
          {
            condition = Calyxiumlib.Ast.Expr.BoolExpr { value = true };
            then_branch = int_expr 1;
            else_branch = Some (int_expr 0);
          }
      in
      let typ, _env =
        Calyxiumlib.Check_expr.check_expr empty_env empty_func_env expr
      in
      match typ with
      | Calyxiumlib.Ast.Type.SymbolType { value = "int" } -> ()
      | _ -> failwith "If expression type incorrect");

  test_case "mismatched binary operation" (fun () ->
      let expr =
        Calyxiumlib.Ast.Expr.BinaryExpr
          {
            left = int_expr 1;
            operator = Calyxiumlib.Token.Plus;
            right = Calyxiumlib.Ast.Expr.StringExpr { value = "oops" };
          }
      in
      try
        ignore (Calyxiumlib.Check_expr.check_expr empty_env empty_func_env expr);
        failwith "Expected type error"
      with
      | Types.TypeError _ -> ()
      | _ -> failwith "Unexpected exception");

  test_case "binary op inference" (fun () ->
      let expr =
        Calyxiumlib.Ast.Expr.BinaryExpr
          {
            left =
              Calyxiumlib.Ast.Expr.IntExpr
                { value = Calyxiumlib.Bigint.of_int 3 };
            operator = Calyxiumlib.Token.Plus;
            right =
              Calyxiumlib.Ast.Expr.IntExpr
                { value = Calyxiumlib.Bigint.of_int 4 };
          }
      in
      let typ, _env =
        Calyxiumlib.Check_expr.check_expr empty_env empty_func_env expr
      in
      match typ with
      | Calyxiumlib.Ast.Type.SymbolType { value = "int" } -> ()
      | _ -> failwith "Binary op inference failed");

  test_case "variable shadowing" (fun () ->
      let stmt1 =
        Calyxiumlib.Ast.Stmt.ExprStmt (var_decl "x" (Some (int_expr 10)))
      in
      let env1, _ =
        Calyxiumlib.Check_expr.check_stmt empty_env empty_func_env stmt1
      in

      let stmt2 =
        Calyxiumlib.Ast.Stmt.ExprStmt (var_decl "x" (Some (int_expr 20)))
      in
      let env2, _ =
        Calyxiumlib.Check_expr.check_stmt env1 empty_func_env stmt2
      in

      match List.assoc_opt "x" env2 with
      | Some (Calyxiumlib.Ast.Type.SymbolType { value = "int" }) -> ()
      | _ -> failwith "Variable shadowing failed");

  test_case "binary op type error" (fun () ->
      let expr =
        Calyxiumlib.Ast.Expr.BinaryExpr
          {
            left = int_expr 5;
            operator = Calyxiumlib.Token.Plus;
            right = Calyxiumlib.Ast.Expr.BoolExpr { value = true };
          }
      in
      try
        ignore (Calyxiumlib.Check_expr.check_expr empty_env empty_func_env expr);
        failwith "Expected type error not raised"
      with
      | Types.TypeError _ -> ()
      | _ -> failwith "Unexpected exception on binary op type error");

  test_case "if expression inference" (fun () ->
      let expr =
        Calyxiumlib.Ast.Expr.IfExpr
          {
            condition = Calyxiumlib.Ast.Expr.BoolExpr { value = true };
            then_branch = int_expr_infer 1;
            else_branch = Some (int_expr_infer 0);
          }
      in
      let typ, _env =
        Calyxiumlib.Check_expr.check_expr empty_env empty_func_env expr
      in
      match typ with
      | Calyxiumlib.Ast.Type.SymbolType { value = "int" } -> ()
      | _ -> failwith "If expression inference failed");

  test_case "recursive factorial" (fun () ->
      let fact_stmt =
        Calyxiumlib.Ast.Stmt.FunctionDeclStmt
          {
            name = "fact";
            is_rec = true;
            parameters =
              [
                {
                  Calyxiumlib.Ast.Stmt.name = "n";
                  param_type = Calyxiumlib.Ast.Type.SymbolType { value = "int" };
                };
              ];
            return_type = Calyxiumlib.Ast.Type.SymbolType { value = "int" };
            body =
              [
                Calyxiumlib.Ast.Stmt.ExprStmt
                  (Calyxiumlib.Ast.Expr.IfExpr
                     {
                       condition =
                         Calyxiumlib.Ast.Expr.BinaryExpr
                           {
                             left = Calyxiumlib.Ast.Expr.VarExpr "n";
                             operator = Calyxiumlib.Token.Eq;
                             right = int_expr 0;
                           };
                       then_branch = int_expr 1;
                       else_branch =
                         Some
                           (Calyxiumlib.Ast.Expr.BinaryExpr
                              {
                                left = Calyxiumlib.Ast.Expr.VarExpr "n";
                                operator = Calyxiumlib.Token.Star;
                                right =
                                  Calyxiumlib.Ast.Expr.CallExpr
                                    {
                                      callee =
                                        Calyxiumlib.Ast.Expr.VarExpr "fact";
                                      arguments =
                                        [
                                          Calyxiumlib.Ast.Expr.BinaryExpr
                                            {
                                              left =
                                                Calyxiumlib.Ast.Expr.VarExpr "n";
                                              operator = Calyxiumlib.Token.Minus;
                                              right = int_expr 1;
                                            };
                                        ];
                                    };
                              });
                     });
              ];
          }
      in
      let _, func_env =
        Calyxiumlib.Check_expr.check_stmt empty_env empty_func_env fact_stmt
      in
      match List.assoc_opt "fact" func_env with
      | Some _ -> ()
      | None -> failwith "Recursive function not added to func_env");

  print_endline "All Typechecker tests done.\n"
