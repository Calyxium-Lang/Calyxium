open Semantics
open Syntax

let print_error_position filename _lexbuf =
  let line_num = Lexer.get_line () in
  let col_num = Lexer.get_column () in

  let file_channel = open_in filename in
  let rec read_lines i =
    try
      let line = input_line file_channel in
      if i = line_num then Some line else read_lines (i + 1)
    with End_of_file -> None
  in
  let line_content = match read_lines 1 with Some line -> line | None -> "" in
  close_in file_channel;

  Printf.printf "Parser Error at Line %d, Column %d\n-> %s\n" line_num col_num
    line_content

let () =
  if Array.length Sys.argv = 2 then (
    let filename = Sys.argv.(1) in
    let file_channel = open_in filename in
    let lexbuf = Lexing.from_channel file_channel in
    try
      let ast = Parser.program Lexer.token lexbuf in

      let initial_env = Typechecker.TypeChecker.empty_env in

      let _ =
        Typechecker.TypeChecker.check_block initial_env [ ast ]
          ~expected_return_type:None
      in

      let bytecode = Bytecode.compile_stmt ast in
      List.iter
        (fun op ->
          Bytecode.pp_opcode Format.str_formatter op;
          let opcode_str = Format.flush_str_formatter () in
          Printf.printf "Generated opcode: %s\n" opcode_str)
        bytecode;
      let _result = Vm.run bytecode in

      close_in file_channel
    with
    | Parser.Error ->
        print_error_position filename lexbuf;
        Printf.fprintf stderr "Missing ';' at the end of the statement.\n";
        close_in file_channel;
        exit (-1)
    | Failure msg ->
        Printf.fprintf stderr "%s\n" msg;
        close_in file_channel;
        exit (-1)
    | e ->
        Printf.fprintf stderr "An unexpected error occurred: %s\n"
          (Printexc.to_string e);
        close_in file_channel;
        exit (-1))
  else Repl.repl ()
