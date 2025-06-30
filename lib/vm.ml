open Opcode

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

let pop2 name f =
  if Stack.length stack < 2 then
    failwith ("Runtime Error: Stack underflow during " ^ name)
  else
    let a = Stack.pop stack in
    let b = Stack.pop stack in
    f b a

let binary_op name op = pop2 name (fun b a -> Stack.push (op b a) stack)

let compare_op name cmp =
  pop2 name (fun b a -> Stack.push (if cmp b a then 1.0 else 0.0) stack)

let logic_op name op =
  pop2 name (fun b a -> Stack.push (if op b a then 1.0 else 0.0) stack)

let get_var env name =
  match List.assoc_opt name env with
  | Some (v, _) -> v
  | None -> failwith ("Runtime Error: Variable \"" ^ name ^ "\" not found")

let get_string_from_stack_value value =
  let id = int_of_float value in
  match Hashtbl.find_opt string_table id with
  | Some s -> s
  | None -> failwith "Runtime Error: Operand for CONCAT is not a valid string"

let resolve_function_body function_name =
  try Hashtbl.find Bytecode.function_table function_name
  with Not_found -> failwith ("Function '" ^ function_name ^ "' not found")

let extract_param_names = function
  | FUNCTION _ :: rest ->
      let rec collect acc = function
        | STORE_VAR name :: tl -> collect (name :: acc) tl
        | _ -> List.rev acc
      in
      collect [] rest
  | _ -> failwith "Invalid function body"

let rec execute instructions env pc =
  let next () = execute instructions env (pc + 1) in
  if pc >= Array.length instructions then
    match Stack.top_opt stack with Some r -> r | None -> 0.0
  else
    match instructions.(pc) with
    | LOAD_INT v ->
        Stack.push (Int64.to_float v) stack;
        next ()
    | LOAD_FLOAT v ->
        Stack.push v stack;
        next ()
    | LOAD_STRING s ->
        Stack.push (float_of_int (add_string s)) stack;
        next ()
    | LOAD_BYTE c ->
        Stack.push (float_of_int (add_string (String.make 1 c))) stack;
        next ()
    | LOAD_BOOL b ->
        Stack.push (if b then 1.0 else 0.0) stack;
        next ()
    | LOAD_VAR name ->
        Stack.push (get_var env name) stack;
        next ()
    | PLUS ->
        binary_op "PLUS" ( +. );
        next ()
    | MINUS ->
        binary_op "MINUS" ( -. );
        next ()
    | STAR ->
        binary_op "STAR" ( *. );
        next ()
    | SLASH ->
        let a = Stack.pop stack in
        let b = Stack.pop stack in
        if a = 0.0 then failwith "Runtime Error: Division by zero"
        else Stack.push (b /. a) stack;
        next ()
    | MOD ->
        binary_op "MOD" mod_float;
        next ()
    | POW ->
        binary_op "POW" ( ** );
        next ()
    | CONCAT ->
        pop2 "CONCAT" (fun b a ->
            let result =
              get_string_from_stack_value b ^ get_string_from_stack_value a
            in
            Stack.push (float_of_int (add_string result)) stack);
        next ()
    | JUMP_IF_FALSE n ->
        if Stack.pop stack = 0.0 then execute instructions env (pc + n)
        else next ()
    | JUMP n -> execute instructions env (pc + n)
    | LESS ->
        compare_op "LESS" ( < );
        next ()
    | GREATER ->
        compare_op "GREATER" ( > );
        next ()
    | LESS_EQUAL ->
        compare_op "LESS_EQUAL" ( <= );
        next ()
    | GREATER_EQUAL ->
        compare_op "GREATER_EQUAL" ( >= );
        next ()
    | NOT_EQUAL ->
        compare_op "NOT_EQUAL" ( <> );
        next ()
    | EQUAL ->
        compare_op "EQUAL" ( = );
        next ()
    | AND ->
        logic_op "AND" (fun x y -> x <> 0.0 && y <> 0.0);
        next ()
    | OR ->
        logic_op "OR" (fun x y -> x <> 0.0 || y <> 0.0);
        next ()
    | LOAD_ARRAY length ->
        let array = Stack.create () in
        for _ = 1 to length do
          Stack.push (Stack.pop stack) array
        done;
        Stack.push (Obj.magic array : float) stack;
        next ()
    | LOAD_INDEX ->
        let index = int_of_float (Stack.pop stack) in
        let array_stack = (Obj.magic (Stack.pop stack) : float Stack.t) in
        let array_list =
          Stack.fold (fun acc x -> x :: acc) [] array_stack |> List.rev
        in
        if index < 0 || index >= List.length array_list then
          failwith "Runtime Error: index out of bounds"
        else Stack.push (List.nth array_list index) stack;
        next ()
    | FUNCTION _ ->
        let rec skip_function pc =
          match instructions.(pc) with
          | RETURN -> pc + 1
          | _ -> skip_function (pc + 1)
        in
        execute instructions env (skip_function (pc + 1))
    | CALL function_name ->
        let function_body = resolve_function_body function_name in
        let param_names = extract_param_names function_body in
        let args =
          List.rev
            (List.init (List.length param_names) (fun _ ->
                 if Stack.is_empty stack then
                   failwith "Stack underflow during CALL";
                 Stack.pop stack))
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
        if Stack.is_empty stack then
          failwith "Runtime Error: Stack underflow during RETURN"
        else Stack.pop stack
    | STORE_VAR name ->
        if Stack.is_empty stack then
          failwith "Runtime Error: Stack is empty when trying to store variable"
        else if List.mem_assoc name env then
          failwith ("Runtime Error: Variable " ^ name ^ " is already declared")
        else
          let value = Stack.pop stack in
          let env = (name, (value, true)) :: env in
          execute instructions env (pc + 1)
    | PRINTLN -> (
        if Stack.is_empty stack then
          failwith "Runtime Error: Stack underflow during PRINTLN"
        else
          match Stack.pop stack with
          | v when v = Float.infinity ->
              Printf.printf "inf\n";
              next ()
          | v when v = Float.neg_infinity ->
              Printf.printf "-inf\n";
              next ()
          | v ->
              let id = int_of_float v in
              if Hashtbl.mem string_table id then
                Printf.printf "%s\n"
                  (replace_escape_sequences (Hashtbl.find string_table id))
              else if floor v = v then Printf.printf "%Ld\n" (Int64.of_float v)
              else Printf.printf "%.10f\n" v;
              next ())

let run instructions =
  try execute (Array.of_list instructions) [] 0
  with Failure msg ->
    prerr_endline ("Execution failed: " ^ msg);
    raise (Failure msg)
