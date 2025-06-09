let () =
  let args = Sys.argv |> Array.to_list |> List.tl in
  let files =
    args |> List.filter (fun f -> not (String.starts_with ~prefix:"--" f))
  in
  if files = [] then exit 1
  else
    files
    |> List.iter (fun file ->
           try
             let chan = open_in file in
             let lexbuf = Lexing.from_channel chan in
             let ast =
               Calyxiumlib.Parser.program Calyxiumlib.Lexer.token lexbuf
             in

             print_endline (Calyxiumlib.Ast.Stmt.show ast)
           with
           | Failure msg ->
               Printf.eprintf "%s\n" msg;
               exit 1
           | e ->
               Printf.eprintf "An unexpected error occurred: %s\n"
                 (Printexc.to_string e);
               exit 1)
