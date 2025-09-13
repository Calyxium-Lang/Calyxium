let typecheck_program stmts : bool =
  let env =
    [
      ("true", Ast.Type.SymbolType { value = "bool" });
      ("false", Ast.Type.SymbolType { value = "bool" });
    ]
  in
  let func_env = Check_expr.collect_functions stmts in
  let _final_env, _final_func_env =
    List.fold_left
      (fun (e, f) stmt -> Check_expr.check_stmt e f stmt)
      (env, func_env) stmts
  in
  let used = !Builtins.stdlib_used in
  Builtins.stdlib_used := false;
  used
