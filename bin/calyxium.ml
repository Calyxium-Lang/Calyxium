open ANSITerminal

let get_line filename n =
  let ic = open_in filename in
  let rec aux i =
    match input_line ic with
    | l when i = 1 ->
        close_in ic;
        Some l
    | _ -> aux (i - 1)
    | exception End_of_file ->
        close_in ic;
        None
  in
  aux n

let print_error ~file ~line ~col ~msg ~source =
  print_string [ red; Bold ] "Error: ";
  print_string [ white ] msg;
  Printf.printf "\n  --> %s:%d:%d\n" file line col;
  match source with
  | Some txt ->
      Printf.printf "   %d | %s\n     | %s" line txt
        (String.make (max 0 (col - 1)) ' ');
      print_string [ red; Bold ] "^\n"
  | None -> ()

let parse_file file =
  let chan = open_in file in
  let lexbuf = Lexing.from_channel chan in
  try
    let ast = Calyxiumlib.Parser.program Calyxiumlib.Lexer.token lexbuf in
    (try
       Calyxiumlib.Typechecker.typecheck_program [ ast ];
       print_string [ green ] "Typecheck OK!\n"
     with Calyxiumlib.Typechecker.TypeError msg ->
       eprintf [ red ] "Type error in %s: %s\n" file msg;
       exit 1);
    print_endline (Calyxiumlib.Ast.Stmt.show ast)
  with
  | Calyxiumlib.Lexer.LexerError msg ->
      let pos = lexbuf.lex_curr_p in
      let line = pos.pos_lnum in
      let col = pos.pos_cnum - pos.pos_bol in
      print_error ~file ~line ~col ~msg ~source:(get_line file line);
      exit 1
  | Calyxiumlib.Parser.Error ->
      let pos = lexbuf.lex_curr_p in
      let line = pos.pos_lnum in
      let col = pos.pos_cnum - pos.pos_bol in
      print_error ~file ~line ~col ~msg:"Syntax error"
        ~source:(get_line file line);
      exit 1
  | Failure msg ->
      eprintf [ red ] "%s\n" msg;
      exit 1
  | e ->
      eprintf [ red ] "Unexpected error: %s\n" (Printexc.to_string e);
      exit 1

let () =
  Sys.argv |> Array.to_list |> List.tl
  |> List.filter (fun a -> not (String.starts_with ~prefix:"--" a))
  |> function
  | [] -> exit 1
  | files -> List.iter parse_file files
