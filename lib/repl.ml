let eval_input input =
  let env = Typechecker.TypeChecker.empty_env in
  try
    input |> Lexing.from_string |> Parser.program Lexer.token |> fun ast ->
    Typechecker.TypeChecker.check_block env [ ast ] ~expected_return_type:None
    |> ignore;
    ast |> Bytecode.compile_stmt |> Vm.run |> ignore
  with e ->
    Printf.eprintf "Repl: An unexpected error occurred: %s\n"
      (Printexc.to_string e)

let print_repl_info () =
  let major, minor, patch = Version.version in
  let system = Version.detect_system () in
  Printf.printf "Calyxium %d.%d.%d (%s) on %s\n" major minor patch
    Version.codename system

let rec repl () =
  print_string ">> ";
  let input = read_line () in
  if input <> "help()" && input <> "copyright()" then (
    eval_input input;
    repl ())
  else print_endline "Exiting REPL."
