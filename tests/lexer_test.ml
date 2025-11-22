[@@@ocaml.warning "-4"]

open Printf
open Calyxium_parser

let run_lexer input =
  let lexbuf = Lexing.from_string input in
  let rec loop acc =
    try
      let tok = Lexer.token lexbuf in
      if tok = Parser.EOF then List.rev acc else loop (tok :: acc)
    with e ->
      failwith (Printf.sprintf "Lexer exception: %s" (Printexc.to_string e))
  in
  loop []

let is_ident token s = match token with Parser.Ident x -> x = s | _ -> false

let test_case name f =
  try
    f ();
    printf "Passed: %s\n" name
  with
  | Failure msg ->
      printf "[FAIL] %s: %s\n" name msg;
      exit 1
  | e ->
      printf "[ERROR] %s: unexpected exception: %s\n" name
        (Printexc.to_string e)

let () =
  print_endline "Lexer Testing";

  test_case "let keyword" (fun () ->
      let tokens = run_lexer "let x = 42 in" in
      match tokens with
      | t1 :: t2 :: _ ->
          if t1 = Parser.Let && is_ident t2 "x" then ()
          else failwith "Lexer failed to tokenize let expressions"
      | _ -> failwith "Lexer failed to tokenize let expressions");

  test_case "if keyword" (fun () ->
      let tokens = run_lexer "if true then 1 else 0" in
      match tokens with
      | t1 :: t2 :: _ :: _ ->
          if t1 = Parser.If && t2 = Parser.True then ()
          else failwith "Lexer failed to tokenize if expression"
      | _ -> failwith "Lexer failed to tokenize if expression");

  test_case "function keyword" (fun () ->
      let tokens = run_lexer "let add(a, b) { a + b }" in
      match tokens with
      | t1 :: _ ->
          if t1 = Parser.Let then () else failwith "Lexer failed for fn"
      | _ -> failwith "Lexer failed for function");

  test_case "int literal" (fun () ->
      let tokens = run_lexer "12345" in
      match tokens with
      | Parser.Int _ :: _ -> ()
      | _ -> failwith "Lexer failed for int literal");

  test_case "float literal" (fun () ->
      let tokens = run_lexer "3.14" in
      match tokens with
      | Parser.Float _ :: _ -> ()
      | _ -> failwith "Lexer failed for float literal");

  test_case "string literal" (fun () ->
      let tokens = run_lexer "\"hello\"" in
      match tokens with
      | Parser.String _ :: _ -> ()
      | _ -> failwith "Lexer failed for string literal");

  test_case "bool literal" (fun () ->
      let tokens = run_lexer "true false" in
      match tokens with
      | Parser.True :: Parser.False :: _ -> ()
      | _ -> failwith "Lexer failed for bool literals");

  test_case "binary operators" (fun () ->
      let tokens = run_lexer "a + b - c * d / e" in
      let ops =
        List.filter
          (function
            | Parser.Plus | Parser.Minus | Parser.Star | Parser.Slash -> true
            | _ -> false)
          tokens
      in
      if List.length ops = 4 then ()
      else failwith "Lexer failed for binary operators");

  test_case "parentheses and commas" (fun () ->
      let tokens = run_lexer "(a, b)" in
      match tokens with
      | Parser.LParen :: Parser.Ident _ :: Parser.Comma :: Parser.Ident _
        :: Parser.RParen :: _ ->
          ()
      | _ -> failwith "Lexer failed for parentheses and commas");

  print_endline ""
