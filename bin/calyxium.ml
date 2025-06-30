open Calyxiumlib.Color
open Calyxiumlib.Bytecode
open Calyxiumlib.Lexer
open Calyxiumlib.Parser
open Calyxiumlib.Typechecker
open Calyxiumlib.Vm
open Calyxiumlib.Error
open Calyxiumlib.Version
open Calyxiumlib.Help
open Calyxiumlib.Opcode

let parse_file file =
  let lexbuf = Lexing.from_channel (open_in file) in
  match program token lexbuf with
  | ast -> (
      try
        typecheck_program [ ast ];
        let bytecode = compile_stmt ast in
        List.iter
          (fun op ->
            pp_opcode Format.str_formatter op;
            let str = Format.flush_str_formatter () in
            Printf.printf "%s\n" str)
          bytecode;
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
  | exception Error ->
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
  let has_flag flag = List.mem flag argv in
  match List.tl argv with
  | _ when has_flag "--help" -> print_endline usage
  | _ when has_flag "--version" ->
      Printf.printf "Calyxium version %s\n" (version_string ());
      exit 0
  | files -> (
      files |> List.filter (fun a -> not (String.starts_with ~prefix:"--" a))
      |> function
      | [] ->
          prerr_endline "No input files provided.";
          exit 1
      | _ -> List.iter parse_file files)
