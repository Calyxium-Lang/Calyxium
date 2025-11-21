let ( let* ) = Result.bind

let typecheck_program stmts : bool =
  let env =
    [
      ("true", Ast.Type.SymbolType { value = "bool" });
      ("false", Ast.Type.SymbolType { value = "bool" });
    ]
  in
  let func_env = Check_expr.collect_functions stmts in

  let final_result =
    List.fold_left
      (fun acc stmt ->
        let* e, f = acc in
        Check_expr.check_stmt e f stmt)
      (Ok (env, func_env))
      stmts
  in

  match final_result with
  | Ok (_final_env, _final_func_env) ->
      let used = !Builtins.stdlib_used in
      Builtins.stdlib_used := false;
      used
  | Error err ->
      Printf.eprintf "Type error: %s\n" (Check_expr.string_of_type_error err);
      false
