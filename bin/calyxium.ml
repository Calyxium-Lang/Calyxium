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
open Calyxiumlib.Formatter

let rec ensure_dir_exists_rec path =
  if Sys.file_exists path then ()
  else
    let parent = Filename.dirname path in
    if parent <> path then ensure_dir_exists_rec parent;
    Unix.mkdir path 0o755

let parse_emit_flag flags file =
  let rec find = function
    | "--emit-bytecode" :: path :: _
      when not (String.starts_with ~prefix:"--" path) ->
        Some path
    | "--emit-bytecode" :: _ -> Some (Filename.remove_extension file ^ ".cxc")
    | _ :: tl -> find tl
    | [] -> None
  in
  find flags

let parse_file ~flags file =
  let in_channel = open_in file in
  let file_size = in_channel_length in_channel in
  if file_size = 0 then (
    close_in in_channel;
    exit 1);
  let lexbuf = Lexing.from_channel in_channel in
  match program token lexbuf with
  | ast -> (
      close_in in_channel;
      try
        let stdlib_used = typecheck_program [ ast ] in
        let bytecode = compile_stmt ast in

        (match parse_emit_flag flags file with
        | Some output_path ->
            save_bytecode_to_file output_path bytecode;
            Printf.printf "Bytecode saved to %s\n" output_path
        | None -> ());

        if stdlib_used && not (List.mem "--no-run" flags) then init_stdlib ();
        if not (List.mem "--no-run" flags) then ignore (run bytecode)
      with TypeError msg ->
        Printf.eprintf "%s%s%s: %s%s\n" red file reset msg reset;
        exit 1)
  | exception LexerError (msg, pos) ->
      close_in in_channel;
      print_error ~file ~msg ~line:pos.pos_lnum
        ~col:(pos.pos_cnum - pos.pos_bol + 1)
        ~source:(get_line file pos.pos_lnum);
      exit 1
  | exception Parsing.Parse_error ->
      close_in in_channel;
      let pos = Lexing.lexeme_start_p lexbuf in
      print_error ~file ~msg:"Syntax Error" ~line:pos.pos_lnum
        ~col:(pos.pos_cnum - pos.pos_bol + 1)
        ~source:(get_line file pos.pos_lnum);
      exit 1
  | exception Failure msg ->
      close_in in_channel;
      Printf.eprintf "%s%s%s\n" red msg reset;
      exit 1
  | exception e ->
      close_in in_channel;
      Printf.eprintf "Unexpected error: %s%s%s\n" red (Printexc.to_string e)
        reset;
      exit 1

let split_flags_and_files args =
  let rec aux flags files = function
    | ("--emit-bytecode" as flag) :: path :: tl
      when not (String.starts_with ~prefix:"--" path) ->
        aux (path :: flag :: flags) files tl
    | flag :: tl when String.starts_with ~prefix:"--" flag ->
        aux (flag :: flags) files tl
    | file :: tl -> aux flags (file :: files) tl
    | [] -> (List.rev flags, List.rev files)
  in
  aux [] [] args

let check_file file =
  let in_channel = open_in file in
  let lexbuf = Lexing.from_channel in_channel in
  try
    let ast = program token lexbuf in
    close_in in_channel;
    ignore (typecheck_program [ ast ]);
    Printf.printf "%sTypecheck successful:%s %s\n" green reset file
  with
  | TypeError msg ->
      close_in in_channel;
      Printf.eprintf "%sTypecheck error in %s:%s %s\n" red file reset msg;
      exit 1
  | LexerError (msg, pos) ->
      close_in in_channel;
      print_error ~file ~msg ~line:pos.pos_lnum
        ~col:(pos.pos_cnum - pos.pos_bol + 1)
        ~source:(get_line file pos.pos_lnum);
      exit 1
  | Parsing.Parse_error ->
      close_in in_channel;
      let pos = Lexing.lexeme_start_p lexbuf in
      print_error ~file ~msg:"Syntax Error" ~line:pos.pos_lnum
        ~col:(pos.pos_cnum - pos.pos_bol + 1)
        ~source:(get_line file pos.pos_lnum);
      exit 1
  | Failure msg ->
      close_in in_channel;
      Printf.eprintf "%s%s%s\n" red msg reset;
      exit 1
  | e ->
      close_in in_channel;
      Printf.eprintf "Unexpected error: %s%s%s\n" red (Printexc.to_string e)
        reset;
      exit 1

let () =
  let argv = Array.to_list Sys.argv in
  let args = List.tl argv in

  match args with
  | "format" :: paths ->
      let rec collect_cx_files dir =
        let files = Sys.readdir dir |> Array.to_list in
        List.fold_left
          (fun acc name ->
            let path = Filename.concat dir name in
            if Sys.is_directory path then acc @ collect_cx_files path
            else if Filename.check_suffix name ".cx" then path :: acc
            else acc)
          [] files
      in
      let cx_files =
        match paths with
        | [] -> collect_cx_files "."
        | _ ->
            List.flatten
              (List.map
                 (fun path ->
                   if Sys.file_exists path then
                     if Sys.is_directory path then collect_cx_files path
                     else if Filename.check_suffix path ".cx" then [ path ]
                     else []
                   else (
                     prerr_endline
                       ("Failed to format " ^ path
                      ^ ": No such file or directory");
                     []))
                 paths)
      in
      List.iter format_file cx_files
  | "check" :: files ->
      if files = [] then (
        prerr_endline "No files provided for type checking.";
        exit 1);
      List.iter check_file files
  | "new" :: name :: _ -> Calyxiumlib.Toml_config.create_project name
  | "new" :: [] ->
      prerr_endline "Please provide a project name. Usage: calyxium new <name>";
      exit 1
  | _ -> (
      let flags, files = split_flags_and_files args in
      match (flags, files) with
      | [ "--help" ], _ -> print_endline usage
      | [ "--version" ], _ ->
          Printf.printf "Calyxium version %s\n" (version_string ());
          exit 0
      | "--run-bytecode" :: _, bytecode_file :: _ ->
          let bytecode = load_bytecode_from_file bytecode_file in
          ignore (run bytecode)
      | [], [] ->
          if Sys.file_exists "calyxium.toml" then (
            match Calyxiumlib.Toml_config.parse_file "calyxium.toml" with
            | Some { run = { main; emit; run = run_flag }; _ } ->
                let bytecode =
                  let in_channel = open_in main in
                  let lexbuf = Lexing.from_channel in_channel in
                  let ast = program token lexbuf in
                  close_in in_channel;
                  let _ = typecheck_program [ ast ] in
                  compile_stmt ast
                in
                (match emit with
                | Some path ->
                    ensure_dir_exists_rec (Filename.dirname path);
                    save_bytecode_to_file path bytecode;
                    Printf.printf "Bytecode saved to %s\n" path
                | None -> ());
                if run_flag then (
                  init_stdlib ();
                  ignore (run bytecode))
            | None ->
                prerr_endline "Invalid or incomplete calyxium.toml file.";
                exit 1)
          else (
            prerr_endline "No input files or calyxium.toml provided.\n";
            print_endline usage;
            exit 1)
      | _, [] ->
          prerr_endline "No input files provided.";
          exit 1
      | _, files -> List.iter (parse_file ~flags) files)
