let () =
  let args = Sys.argv |> Array.to_list |> List.tl in
  let debug_mode = List.exists (( = ) "--debug") args in
  let files =
    args |> List.filter (fun f -> not (String.starts_with ~prefix:"--" f))
  in
  if files = [] then Calyxiumlib.Repl.repl ()
  else
    let env = Calyxiumlib.Typechecker.TypeChecker.empty_env in
    files
    |> List.iter (fun file ->
           try
             let chan = open_in file in
             let lexbuf = Lexing.from_channel chan in
             let ast =
               Calyxiumlib.Parser.program Calyxiumlib.Lexer.token lexbuf
             in

             print_endline (Calyxiumlib.Ast.Stmt.show ast);

             ignore
               (Calyxiumlib.Typechecker.TypeChecker.check_block env [ ast ]
                  ~expected_return_type:None);

             let bytecode = Calyxiumlib.Bytecode.compile_stmt ast in

             if debug_mode then
               List.iter
                 (fun op ->
                   Calyxiumlib.Bytecode.pp_opcode Format.str_formatter op;
                   let opcode_str = Format.flush_str_formatter () in
                   Printf.printf "Generated opcode: %s\n" opcode_str)
                 bytecode;

             ignore (Calyxiumlib.Vm.run bytecode);
             close_in chan
           with
           | Failure msg ->
               Printf.eprintf "%s\n" msg;
               exit 1
           | e ->
               Printf.eprintf "An unexpected error occurred: %s\n"
                 (Printexc.to_string e);
               exit 1)
