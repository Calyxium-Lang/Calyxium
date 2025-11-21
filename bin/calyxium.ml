let rec ensure_dir_exists_rec path =
  if Sys.file_exists path then ()
  else
    let parent = Filename.dirname path in
    if parent <> path then ensure_dir_exists_rec parent;
    try Unix.mkdir path 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()

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
  let has_flag flag flags = List.exists (( = ) flag) flags in
  let in_channel = open_in file in
  let file_size = in_channel_length in_channel in
  if file_size = 0 then (
    close_in in_channel;
    exit 1);
  let lexbuf = Lexing.from_channel in_channel in
  match Calyxiumlib.Parser.program Calyxiumlib.Lexer.token lexbuf with
  | ast -> (
      close_in in_channel;
      try
        let ir = Calyxiumlib.Ir.ast_to_ir ast in
        let ir =
          if has_flag "--no-opt" flags then ir
          else Calyxiumlib.Optimize.optimize ir
        in
        let stdlib_used = Calyxiumlib.Typechecker.typecheck_program ir in
        let bytecode =
          List.concat_map Calyxiumlib.Bytecode.ir_compile_stmt ir
        in

        (match parse_emit_flag flags file with
        | Some output_path ->
            Calyxiumlib.Bytegen.save_bytecode_to_file output_path bytecode;
            Printf.printf "Bytecode saved to %s\n" output_path
        | None -> ());

        if stdlib_used && not (List.mem "--no-run" flags) then
          Calyxiumlib.Vm.init_stdlib ();
        if not (List.mem "--no-run" flags) then
          ignore (Calyxiumlib.Vm.run bytecode)
      with Calyxiumlib.Types.TypeError msg ->
        Printf.eprintf "%s%s%s: %s%s\n" Calyxiumlib.Color.red file
          Calyxiumlib.Color.reset msg Calyxiumlib.Color.reset;
        exit 1)
  | exception Calyxiumlib.Lexer.LexerError (msg, pos) ->
      close_in in_channel;
      Calyxiumlib.Error.print_error ~file ~msg ~line:pos.Lexing.pos_lnum
        ~col:(pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)
        ~source:(Calyxiumlib.Error.get_line file pos.Lexing.pos_lnum);
      exit 1
  | exception Calyxiumlib.Parser.Error ->
      close_in in_channel;
      let pos = Lexing.lexeme_start_p lexbuf in
      Calyxiumlib.Error.print_error ~file ~msg:"Syntax Error"
        ~line:pos.Lexing.pos_lnum
        ~col:(pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)
        ~source:(Calyxiumlib.Error.get_line file pos.Lexing.pos_lnum);
      exit 1
  | exception Failure msg ->
      close_in in_channel;
      Printf.eprintf "%s%s%s\n" Calyxiumlib.Color.red msg
        Calyxiumlib.Color.reset;
      exit 1
  | exception e ->
      close_in in_channel;
      Printf.eprintf "Unexpected error: %s%s%s\n" Calyxiumlib.Color.red
        (Printexc.to_string e) Calyxiumlib.Color.reset;
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
    let ast = Calyxiumlib.Parser.program Calyxiumlib.Lexer.token lexbuf in
    let ir = Calyxiumlib.Ir.ast_to_ir ast in
    close_in in_channel;
    ignore (Calyxiumlib.Typechecker.typecheck_program ir);
    Printf.printf "%sTypecheck successful:%s %s\n" Calyxiumlib.Color.green
      Calyxiumlib.Color.reset file
  with
  | Calyxiumlib.Types.TypeError msg ->
      close_in in_channel;
      Printf.eprintf "%sTypecheck error in %s:%s %s\n" Calyxiumlib.Color.red
        file Calyxiumlib.Color.reset msg;
      exit 1
  | Calyxiumlib.Lexer.LexerError (msg, pos) ->
      close_in in_channel;
      Calyxiumlib.Error.print_error ~file ~msg ~line:pos.Lexing.pos_lnum
        ~col:(pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)
        ~source:(Calyxiumlib.Error.get_line file pos.Lexing.pos_lnum);
      exit 1
  | Calyxiumlib.Parser.Error ->
      close_in in_channel;
      let pos = Lexing.lexeme_start_p lexbuf in
      Calyxiumlib.Error.print_error ~file ~msg:"Syntax Error"
        ~line:pos.Lexing.pos_lnum
        ~col:(pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)
        ~source:(Calyxiumlib.Error.get_line file pos.Lexing.pos_lnum);
      exit 1
  | Failure msg ->
      close_in in_channel;
      Printf.eprintf "%s%s%s\n" Calyxiumlib.Color.red msg
        Calyxiumlib.Color.reset;
      exit 1
  | e ->
      close_in in_channel;
      Printf.eprintf "Unexpected error: %s%s%s\n" Calyxiumlib.Color.red
        (Printexc.to_string e) Calyxiumlib.Color.reset;
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
      List.iter Calyxiumlib.Formatter.format_file cx_files
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
      | [ "--help" ], _ -> print_endline Calyxiumlib.Help.usage
      | [ "--version" ], _ ->
          Printf.printf "Calyxium version %s\n"
            (Calyxiumlib.Version.version_string ());
          exit 0
      | "--run-bytecode" :: _, bytecode_file :: _ ->
          let bytecode =
            Calyxiumlib.Bytegen.load_bytecode_from_file bytecode_file
          in
          ignore (Calyxiumlib.Vm.run bytecode)
      | [], [] ->
          if Sys.file_exists "calyxium.toml" then (
            match Calyxiumlib.Toml_config.parse_file "calyxium.toml" with
            | Some config ->
                let { Calyxiumlib.Toml_config.run_cfg; _ } = config in
                let {
                  Calyxiumlib.Toml_config.main;
                  Calyxiumlib.Toml_config.emit;
                  Calyxiumlib.Toml_config.should_run = run_flag;
                } =
                  run_cfg
                in

                let bytecode =
                  let in_channel = open_in main in
                  let lexbuf = Lexing.from_channel in_channel in
                  let ast =
                    Calyxiumlib.Parser.program Calyxiumlib.Lexer.token lexbuf
                  in
                  close_in in_channel;
                  let ir = Calyxiumlib.Ir.ast_to_ir ast in
                  let _ = Calyxiumlib.Typechecker.typecheck_program ir in
                  Calyxiumlib.Bytecode.ir_compile_stmt
                    (Calyxiumlib.Ir.IR_Block ir)
                in

                (match emit with
                | Some path ->
                    ensure_dir_exists_rec (Filename.dirname path);
                    Calyxiumlib.Bytegen.save_bytecode_to_file path bytecode;
                    Printf.printf "Bytecode saved to %s\n" path
                | None -> ());

                if run_flag then (
                  Calyxiumlib.Vm.init_stdlib ();
                  ignore (Calyxiumlib.Vm.run bytecode))
            | None ->
                prerr_endline "Invalid or incomplete calyxium.toml file.";
                exit 1)
          else (
            prerr_endline "No input files or calyxium.toml provided.\n";
            print_endline Calyxiumlib.Help.usage;
            exit 1)
      | _, [] ->
          prerr_endline "No input files provided.";
          exit 1
      | _, files -> List.iter (parse_file ~flags) files)
