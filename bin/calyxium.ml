open ANSITerminal

let parse_file file =
  let lexbuf = Lexing.from_channel (open_in file) in
  match Calyxiumlib.Parser.program Calyxiumlib.Lexer.token lexbuf with
  | ast -> (
      try Calyxiumlib.Typechecker.typecheck_program [ ast ]
      with Calyxiumlib.Typechecker.TypeError msg ->
        eprintf [ red ] "%s: %s\n" file msg;
        exit 1)
  | exception Calyxiumlib.Lexer.LexerError msg ->
      let pos = lexbuf.lex_curr_p in
      Calyxiumlib.Error.print_error ~file ~msg ~line:pos.pos_lnum
        ~col:(pos.pos_cnum - pos.pos_bol)
        ~source:(Calyxiumlib.Error.get_line file pos.pos_lnum);
      exit 1
  | exception Calyxiumlib.Parser.Error ->
      let pos = lexbuf.lex_curr_p in
      Calyxiumlib.Error.print_error ~file ~msg:"Syntax Error" ~line:pos.pos_lnum
        ~col:(pos.pos_cnum - pos.pos_bol)
        ~source:(Calyxiumlib.Error.get_line file pos.pos_lnum);
      exit 1
  | exception Failure msg ->
      eprintf [ red ] "%s\n" msg;
      exit 1
  | exception e ->
      eprintf [ red ] "Unexpected error: %s\n" (Printexc.to_string e);
      exit 1

let () =
  let argv = Array.to_list Sys.argv in
  let has_flag flag = List.mem flag argv in
  match List.tl argv with
  | _ when has_flag "--help" -> print_endline Calyxiumlib.Help.usage
  | _ when has_flag "--version" ->
      Printf.printf "Calyxium version %s\n"
        (Calyxiumlib.Version.version_string ());
      exit 0
  | files -> (
      files |> List.filter (fun a -> not (String.starts_with ~prefix:"--" a))
      |> function
      | [] ->
          prerr_endline "No input files provided.";
          exit 1
      | _ -> List.iter parse_file files)
