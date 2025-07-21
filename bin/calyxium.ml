open Calyxiumlib.Color
open Calyxiumlib.Bytecode
open Calyxiumlib.Lexer
open Calyxiumlib.Parser
open Calyxiumlib.Typechecker
open Calyxiumlib.Vm
open Calyxiumlib.Error
open Calyxiumlib.Version
open Calyxiumlib.Help
open Calyxiumlib.Bytegen

let parse_file ~flags file =
  let bytecode_file = Filename.remove_extension file ^ ".cxc" in
  let lexbuf = Lexing.from_channel (open_in file) in
  match program token lexbuf with
  | ast -> (
      try
        ignore (typecheck_program [ ast ]);
        let bytecode = compile_stmt ast in

        if List.mem "--emit-bytecode" flags then (
          save_bytecode_to_file bytecode_file bytecode;
          Printf.printf "Bytecode saved to %s\n" bytecode_file);

        if not (List.mem "--emit-bytecode" flags || List.mem "--no-run" flags)
        then init_stdlib ();
        ignore (run bytecode)
      with TypeError msg ->
        Printf.eprintf "%s%s: %s%s\n" red file msg reset;
        exit 1)
  | exception LexerError msg ->
      let pos = lexbuf.lex_curr_p in
      print_error ~file ~msg ~line:pos.pos_lnum
        ~col:(pos.pos_cnum - pos.pos_bol)
        ~source:(get_line file pos.pos_lnum);
      exit 1
  | exception Parsing.Parse_error ->
      let pos = lexbuf.lex_curr_p in
      print_error ~file ~msg:"Syntax Error" ~line:pos.pos_lnum
        ~col:(pos.pos_cnum - pos.pos_bol)
        ~source:(get_line file pos.pos_lnum);
      exit 1
  | exception Failure msg ->
      Printf.eprintf "%s%s%s\n" red msg reset;
      exit 1
  | exception e ->
      Printf.eprintf "Unexpected error: %s%s%s\n" red (Printexc.to_string e)
        reset;
      exit 1

let () =
  let argv = Array.to_list Sys.argv in
  let args = List.tl argv in
  let flags, files = List.partition (String.starts_with ~prefix:"--") args in

  match (flags, files) with
  | [ "--help" ], _ -> print_endline usage
  | [ "--version" ], _ ->
      Printf.printf "Calyxium version %s\n" (version_string ());
      exit 0
  | "--run-bytecode" :: _, bytecode_file :: _ ->
      let bytecode = load_bytecode_from_file bytecode_file in
      ignore (run bytecode)
  | _, [] ->
      prerr_endline "No input files provided.";
      exit 1
  | _, files -> List.iter (parse_file ~flags) files
