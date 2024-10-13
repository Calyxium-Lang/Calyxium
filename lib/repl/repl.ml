open Semantics
open Syntax

let eval_input input =
  let lexbuf = Lexing.from_string input in
  try
    let ast = Parser.program Lexer.token lexbuf in

    let initial_env = Typechecker.TypeChecker.empty_env in

    let _ =
      Typechecker.TypeChecker.check_block initial_env [ ast ]
        ~expected_return_type:None
    in

    let bytecode = Bytecode.compile_stmt ast in

    List.iter (fun op -> Bytecode.pp_opcode Format.str_formatter op) bytecode;

    let _result = Vm.run bytecode in
    ()
  with e ->
    Printf.printf "Repl: An unexpected error occurred: %s\n"
      (Printexc.to_string e)

let get_version () = "0.0.1"

let print_repl_info () =
  let version = get_version () in
  let platform =
    match Sys.os_type with
    | "Unix" -> "Unix"
    | "Win32" -> "Windows"
    | _ -> "Unknown"
  in
  Printf.printf "Calyxium %s on %s\n" version platform

let rec repl () =
  Printf.printf ">> ";
  let input = read_line () in
  if input <> "help()" && input <> "copyright()" then (
    eval_input input;
    repl ())
  else Printf.printf "Exiting REPL.\n"
