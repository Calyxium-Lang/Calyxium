let () =
  match Sys.argv |> Array.to_list |> List.tl with
  | args
    when List.exists (fun f -> not (String.starts_with ~prefix:"--" f)) args ->
      let env = Calyxiumlib.Typechecker.TypeChecker.empty_env in
      args
      |> List.filter (fun f -> not (String.starts_with ~prefix:"--" f))
      |> List.iter (fun file ->
             try
               file |> open_in |> Lexing.from_channel
               |> Calyxiumlib.Parser.program Calyxiumlib.Lexer.token
               |> fun ast ->
               Calyxiumlib.Typechecker.TypeChecker.check_block env [ ast ]
                 ~expected_return_type:None
               |> ignore;
               ast |> Calyxiumlib.Bytecode.compile_stmt |> Calyxiumlib.Vm.run
               |> ignore
             with
             | Failure msg ->
                 Printf.fprintf stderr "%s\n" msg;
                 exit (-1)
             | e ->
                 Printf.fprintf stderr "An unexpected error occurred: %s\n"
                   (Printexc.to_string e);
                 exit (-1))
  | _ -> Calyxiumlib.Repl.repl ()
