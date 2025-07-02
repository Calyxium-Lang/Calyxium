open Opcode

exception RuntimeError of string

type trace_entry = { pc : int; instr : string }

let trace : trace_entry list ref = ref []
let push_trace pc instr = trace := { pc; instr } :: !trace
let clear_trace () = trace := []

let print_trace () =
  match !trace with
  | { pc; instr } :: _ ->
      prerr_endline "Stack Trace (most recent call last):";
      prerr_endline
        ("  at instruction '" ^ instr ^ "' (pc=" ^ string_of_int pc ^ ")")
  | [] -> ()

let runtime_error msg =
  print_trace ();
  raise (RuntimeError ("Runtime Error: " ^ msg))

let stack : float Stack.t = Stack.create ()
let string_table = Hashtbl.create 16

let add_string str =
  let id = Hashtbl.hash str in
  Hashtbl.replace string_table id str;
  id

let escape_sequences =
  [ ("\\n", '\n'); ("\\t", '\t'); ("\\r", '\r'); ("\\\\", '\\') ]

let replace_escape_sequences str =
  let buffer = Buffer.create (String.length str) in
  let len = String.length str in
  let rec aux i =
    if i >= len then Buffer.contents buffer
    else if i + 1 < len && str.[i] = '\\' then (
      let esc_seq = "\\" ^ String.make 1 str.[i + 1] in
      match List.assoc_opt esc_seq escape_sequences with
      | Some ch ->
          Buffer.add_char buffer ch;
          aux (i + 2)
      | None ->
          Buffer.add_char buffer str.[i];
          aux (i + 1))
    else (
      Buffer.add_char buffer str.[i];
      aux (i + 1))
  in
  aux 0

let pop1 name =
  match Stack.pop_opt stack with
  | Some v -> v
  | None -> runtime_error ("Stack underflow during '" ^ name ^ "'")

let pop2 name =
  try
    let a = Stack.pop stack in
    let b = Stack.pop stack in
    (b, a)
  with Stack.Empty ->
    runtime_error ("Stack underflow during '" ^ name ^ "' (need 2 values)")

let binary_op name op =
  let b, a = pop2 name in
  Stack.push (op b a) stack

let compare_op name cmp =
  let b, a = pop2 name in
  Stack.push (if cmp b a then 1.0 else 0.0) stack

let logic_op name op =
  let b, a = pop2 name in
  Stack.push (if op b a then 1.0 else 0.0) stack

let unary_logic_op name op =
  let a = pop1 ("unary logic op '" ^ name ^ "'") in
  Stack.push (if op a then 1.0 else 0.0) stack

let unary_op name op =
  let x = pop1 ("unary op '" ^ name ^ "'") in
  Stack.push (op x) stack

let get_var env name =
  match List.assoc_opt name env with
  | Some (v, _) -> v
  | None -> runtime_error ("Variable '" ^ name ^ "' not found in environment")

let get_string_from_stack_value value =
  let id = int_of_float value in
  match Hashtbl.find_opt string_table id with
  | Some s -> s
  | None ->
      runtime_error
        ("Expected string on stack, but no string with ID " ^ string_of_int id)

let resolve_function_body function_name =
  try Hashtbl.find Bytecode.function_table function_name
  with Not_found ->
    runtime_error
      ("Function '" ^ function_name ^ "' not found in function table")

let extract_param_names = function
  | FUNCTION _ :: rest ->
      let rec collect acc = function
        | STORE_VAR name :: tl -> collect (name :: acc) tl
        | _ -> List.rev acc
      in
      collect [] rest
  | _ -> runtime_error "Malformed function body during parameter extraction"

let rec execute instructions env pc =
  let next () = execute instructions env (pc + 1) in
  if pc >= Array.length instructions then
    match Stack.top_opt stack with Some r -> r | None -> 0.0
  else
    match instructions.(pc) with
    | LOAD_INT v ->
        push_trace pc ("LOAD_INT " ^ Int64.to_string v);
        Stack.push (Int64.to_float v) stack;
        next ()
    | LOAD_FLOAT v ->
        push_trace pc ("LOAD_FLOAT " ^ string_of_float v);
        Stack.push v stack;
        next ()
    | LOAD_STRING s ->
        push_trace pc ("LOAD_STRING " ^ s);
        Stack.push (float_of_int (add_string s)) stack;
        next ()
    | LOAD_BYTE c ->
        push_trace pc ("LOAD_BYTE " ^ String.make 1 c);
        Stack.push (float_of_int (add_string (String.make 1 c))) stack;
        next ()
    | LOAD_BOOL b ->
        push_trace pc ("LOAD_BOOL " ^ string_of_bool b);
        Stack.push (if b then 1.0 else 0.0) stack;
        next ()
    | LOAD_UNIT _ ->
        push_trace pc "LOAD_UNIT";
        Stack.push nan stack;
        next ()
    | LOAD_VAR name ->
        push_trace pc ("LOAD_VAR " ^ name);
        Stack.push (get_var env name) stack;
        next ()
    | PLUS ->
        push_trace pc "PLUS";
        binary_op "PLUS" ( +. );
        next ()
    | MINUS ->
        push_trace pc "MINUS";
        binary_op "MINUS" ( -. );
        next ()
    | STAR ->
        push_trace pc "STAR";
        binary_op "STAR" ( *. );
        next ()
    | SLASH ->
        push_trace pc "SLASH";
        let a = Stack.pop stack in
        let b = Stack.pop stack in
        if a = 0.0 then runtime_error "Division by zero in SLASH"
        else Stack.push (b /. a) stack;
        next ()
    | MOD ->
        push_trace pc "MOD";
        binary_op "MOD" mod_float;
        next ()
    | POW ->
        push_trace pc "POW";
        binary_op "POW" ( ** );
        next ()
    | CONCAT ->
        push_trace pc "CONCAT";
        let b, a = pop2 "CONCAT" in
        let result =
          get_string_from_stack_value b ^ get_string_from_stack_value a
        in
        Stack.push (float_of_int (add_string result)) stack;
        next ()
    | JUMP_IF_FALSE offset -> (
        push_trace pc ("JUMP_IF_FALSE " ^ string_of_int offset);
        match Stack.pop_opt stack with
        | Some cond when cond = 0.0 -> execute instructions env (pc + offset)
        | Some _ -> next ()
        | None -> runtime_error "JUMP_IF_FALSE with empty stack")
    | JUMP n ->
        push_trace pc ("JUMP " ^ string_of_int n);
        execute instructions env (pc + n)
    | LESS ->
        push_trace pc "LESS";
        compare_op "LESS" ( < );
        next ()
    | GREATER ->
        push_trace pc "GREATER";
        compare_op "GREATER" ( > );
        next ()
    | LESS_EQUAL ->
        push_trace pc "LESS_EQUAL";
        compare_op "LESS_EQUAL" ( <= );
        next ()
    | GREATER_EQUAL ->
        push_trace pc "GREATER_EQUAL";
        compare_op "GREATER_EQUAL" ( >= );
        next ()
    | NOT_EQUAL ->
        push_trace pc "NOT_EQUAL";
        compare_op "NOT_EQUAL" ( <> );
        next ()
    | EQUAL ->
        push_trace pc "EQUAL";
        compare_op "EQUAL" ( = );
        next ()
    | AND ->
        push_trace pc "AND";
        logic_op "AND" (fun x y -> x <> 0.0 && y <> 0.0);
        next ()
    | OR ->
        push_trace pc "OR";
        logic_op "OR" (fun x y -> x <> 0.0 || y <> 0.0);
        next ()
    | NOT ->
        push_trace pc "NOT";
        unary_logic_op "NOT" (fun x -> x = 0.0);
        next ()
    | INC ->
        push_trace pc "INC";
        unary_op "INC" (fun x -> x +. 1.0);
        next ()
    | DEC ->
        push_trace pc "DEC";
        unary_op "DEC" (fun x -> x -. 1.0);
        next ()
    | DUP ->
        push_trace pc "DUP";
        Stack.top_opt stack |> Option.iter (fun v -> Stack.push v stack);
        if Stack.is_empty stack then runtime_error "Stack underflow during DUP";
        next ()
    | POP ->
        push_trace pc "POP";
        if Stack.pop_opt stack = None then
          runtime_error "POP attempted on empty stack"
        else next ()
    | LOAD_ARRAY length ->
        push_trace pc ("LOAD_ARRAY " ^ string_of_int length);
        if Stack.length stack < length then
          runtime_error
            ("LOAD_ARRAY expects " ^ string_of_int length ^ " elements on stack")
        else
          let array = Stack.create () in
          for _ = 1 to length do
            Stack.push (Stack.pop stack) array
          done;
          Stack.push (Obj.magic array : float) stack;
          next ()
    | LOAD_INDEX ->
        push_trace pc "LOAD_INDEX";
        if Stack.length stack < 2 then
          runtime_error
            "LOAD_INDEX requires two values on the stack (array, index)";
        let index = int_of_float (Stack.pop stack) in
        let array_stack =
          try (Obj.magic (Stack.pop stack) : float Stack.t)
          with _ -> runtime_error "LOAD_INDEX failed to cast array"
        in
        let array_list =
          Stack.fold (fun acc x -> x :: acc) [] array_stack |> List.rev
        in
        if index < 0 || index >= List.length array_list then
          runtime_error
            ("Index out of bounds in LOAD_INDEX: " ^ string_of_int index)
        else Stack.push (List.nth array_list index) stack;
        next ()
    | FUNCTION _ ->
        let rec skip_function pc =
          if pc >= Array.length instructions then
            runtime_error "Unterminated FUNCTION block"
          else
            match instructions.(pc) with
            | RETURN -> pc + 1
            | _ -> skip_function (pc + 1)
        in
        execute instructions env (skip_function (pc + 1))
    | CALL function_name ->
        push_trace pc ("CALL " ^ function_name);
        let function_body = resolve_function_body function_name in
        let param_names = extract_param_names function_body in
        if Stack.length stack < List.length param_names then
          runtime_error
            ("CALL to '" ^ function_name ^ "' requires "
            ^ string_of_int (List.length param_names)
            ^ " arguments, but stack has only "
            ^ string_of_int (Stack.length stack));
        let args =
          List.rev
            (List.init (List.length param_names) (fun _ -> Stack.pop stack))
        in
        let local_env =
          List.combine param_names (List.map (fun v -> (v, true)) args)
        in
        let skip_header = 1 + List.length param_names in
        let body = List.drop skip_header function_body in
        let return_value = execute (Array.of_list body) local_env 0 in
        Stack.push return_value stack;
        next ()
    | RETURN ->
        push_trace pc "RETURN";
        if Stack.is_empty stack then
          runtime_error "RETURN attempted with empty stack"
        else Stack.pop stack
    | STORE_VAR name ->
        push_trace pc ("STORE_VAR " ^ name);
        if Stack.is_empty stack then
          runtime_error ("STORE_VAR '" ^ name ^ "' failed: stack is empty")
        else if List.mem_assoc name env then
          runtime_error ("STORE_VAR: Variable '" ^ name ^ "' already declared")
        else
          let value = Stack.pop stack in
          let env = (name, (value, true)) :: env in
          execute instructions env (pc + 1)
    | PRINTLN ->
        push_trace pc "PRINTLN";
        if Stack.is_empty stack then
          runtime_error "PRINTLN attempted with empty stack"
        else
          let value = Stack.pop stack in
          if Float.is_nan value then (
            Printf.printf "unit\n";
            next ())
          else if value = Float.infinity then (
            Printf.printf "inf\n";
            next ())
          else if value = Float.neg_infinity then (
            Printf.printf "-inf\n";
            next ())
          else
            let int_value = int_of_float value in
            if Hashtbl.mem string_table int_value then
              let str = Hashtbl.find string_table int_value in
              let processed_str = replace_escape_sequences str in
              Printf.printf "%s\n" processed_str
            else if floor value = value then
              Printf.printf "%Ld\n" (Int64.of_float value)
            else Printf.printf "%.10f\n" value;
            next ()
    | INPUT ->
        push_trace pc "INPUT";
        if Stack.is_empty stack then
          runtime_error "Stack underflow during INPUT"
        else
          let value = Stack.pop stack in
          let int_value = int_of_float value in
          if Hashtbl.mem string_table int_value then (
            let prompt = Hashtbl.find string_table int_value in
            Printf.printf "%s" prompt;
            let input_value = read_line () in
            let processed_value =
              try float_of_string input_value
              with Failure _ ->
                let id = add_string input_value in
                float_of_int id
            in
            Stack.push processed_value stack;
            next ())
          else runtime_error "Invalid prompt ID for INPUT"

let run instructions =
  try
    let result = execute (Array.of_list instructions) [] 0 in
    clear_trace ();
    result
  with RuntimeError msg ->
    prerr_endline msg;
    exit 1
