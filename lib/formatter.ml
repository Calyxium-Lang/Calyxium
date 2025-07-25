let format_file (file : string) : unit =
  try
    let contents = In_channel.with_open_bin file In_channel.input_all in
    let normalized = String.concat "\n" (String.split_on_char '\r' contents) in
    let lexbuf = Lexing.from_string normalized in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = file };

    let ast =
      try Parser.program Lexer.token lexbuf
      with e ->
        Printf.eprintf "Failed to parse %s: %s\n%!" file (Printexc.to_string e);
        raise e
    in

    let formatted =
      match ast with
      | Ast.Stmt.BlockStmt { body } -> Pretty.string_of_program body ^ "\n"
      | _ -> Pretty.string_of_program [ ast ] ^ "\n"
    in

    Out_channel.with_open_bin file (fun oc ->
        Out_channel.output_string oc formatted);
    Printf.printf "Formatted %s\n%!" file
  with e ->
    Printf.eprintf "Failed to format %s: %s\n%!" file (Printexc.to_string e)
