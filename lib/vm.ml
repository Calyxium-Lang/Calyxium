open Opcode
open Gc

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

let stack : Gc.value Stack.t = Stack.create ()
let global_env : (string * (Gc.value * bool)) list ref = ref []

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

let binary_op name op op_int64 =
  let a, b = pop2 name in
  match (a, b) with
  | VFloat a, VFloat b -> Stack.push (VFloat (op a b)) stack
  | VInt64 a, VInt64 b -> Stack.push (VInt64 (op_int64 a b)) stack
  | _ -> runtime_error ("binary_op '" ^ name ^ "' expected two floats")

let compare_op name cmp cmp_int64 =
  let a, b = pop2 name in
  match (a, b) with
  | VFloat a, VFloat b ->
      Stack.push (if cmp a b then VFloat 1.0 else VFloat 0.0) stack
  | VInt64 a, VInt64 b ->
      Stack.push (if cmp_int64 a b then VInt64 1L else VInt64 0L) stack
  | _ -> runtime_error ("compare_op '" ^ name ^ "' expected two floats")

let logic_op name op op_int64 =
  let a, b = pop2 name in
  match (a, b) with
  | VFloat a, VFloat b ->
      Stack.push (if op a b then VFloat 1.0 else VFloat 0.0) stack
  | VInt64 a, VInt64 b ->
      Stack.push (if op_int64 a b then VInt64 1L else VInt64 0L) stack
  | _ -> runtime_error ("logic_op '" ^ name ^ "' expected two floats")

let unary_logic_op name op op_int64 =
  let a = pop1 ("unary logic op '" ^ name ^ "'") in
  match a with
  | VFloat x -> Stack.push (if op x then VFloat 1.0 else VFloat 0.0) stack
  | VInt64 x -> Stack.push (if op_int64 x then VInt64 1L else VInt64 0L) stack
  | _ -> runtime_error ("unary_logic_op '" ^ name ^ "' expected float")

let unary_op name op op_int64 =
  let x = pop1 ("unary op '" ^ name ^ "'") in
  match x with
  | VFloat f -> Stack.push (VFloat (op f)) stack
  | VInt64 i -> Stack.push (VInt64 (op_int64 i)) stack
  | _ -> runtime_error ("unary_op '" ^ name ^ "' expected float")

let get_var env name =
  match List.assoc_opt name env with
  | Some (v, _) -> v
  | None -> (
      match List.assoc_opt name !global_env with
      | Some (v, _) -> v
      | None ->
          runtime_error ("Variable '" ^ name ^ "' not found in environment"))

let get_string_from_stack_value = function
  | VHeapRef id -> (
      match Gc.get_string id with
      | Some s -> s
      | None ->
          runtime_error
            ("Expected string on stack, but no string with ID "
           ^ string_of_int id))
  | _ -> runtime_error "Expected a string heap reference on stack"

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

let rec int64_pow base exp =
  if exp < 0L then invalid_arg "int64_pow: negative exponent"
  else if exp = 0L then Int64.one
  else if Int64.rem exp 2L = 0L then
    let half = int64_pow base (Int64.div exp 2L) in
    Int64.mul half half
  else Int64.mul base (int64_pow base (Int64.sub exp 1L))

let rec execute instructions env pc =
  let next () = execute instructions env (pc + 1) in
  if pc >= Array.length instructions then
    match Stack.top_opt stack with Some r -> r | None -> VFloat 0.0
  else
    match instructions.(pc) with
    | LOAD_INT64 v ->
        push_trace pc ("LOAD_INT64 " ^ Int64.to_string v);
        Stack.push (VInt64 v) stack;
        next ()
    | LOAD_FLOAT v ->
        push_trace pc ("LOAD_FLOAT " ^ string_of_float v);
        Stack.push (VFloat v) stack;
        next ()
    | LOAD_STRING s ->
        push_trace pc ("LOAD_STRING " ^ s);
        let v = Gc.alloc_string_with_gc stack env s in
        Stack.push v stack;
        next ()
    | LOAD_BYTE c ->
        push_trace pc ("LOAD_BYTE " ^ String.make 1 c);
        let v = Gc.alloc_string_with_gc stack env (String.make 1 c) in
        Stack.push v stack;
        next ()
    | LOAD_BOOL b ->
        push_trace pc ("LOAD_BOOL " ^ string_of_bool b);
        Stack.push (VBool b) stack;
        next ()
    | LOAD_UNIT _ ->
        push_trace pc "LOAD_UNIT";
        Stack.push (VFloat nan) stack;
        next ()
    | LOAD_TUPLE n ->
        push_trace pc ("LOAD_TUPLE " ^ string_of_int n);
        if Stack.length stack < n then
          runtime_error
            ("LOAD_TUPLE expects " ^ string_of_int n ^ " values on the stack")
        else
          let items = List.init n (fun _ -> Stack.pop stack) |> List.rev in
          Stack.push (VTuple items) stack;
          next ()
    | LOAD_VAR name ->
        push_trace pc ("LOAD_VAR " ^ name);
        Stack.push (get_var env name) stack;
        next ()
    | PLUS ->
        push_trace pc "PLUS";
        binary_op "PLUS" ( +. ) Int64.add;
        next ()
    | MINUS ->
        push_trace pc "MINUS";
        binary_op "MINUS" ( -. ) Int64.sub;
        next ()
    | STAR ->
        push_trace pc "STAR";
        binary_op "STAR" ( *. ) Int64.mul;
        next ()
    | SLASH ->
        push_trace pc "SLASH";
        let a = Stack.pop stack in
        let b = Stack.pop stack in
        (match (a, b) with
        | VFloat a_val, VFloat b_val ->
            if a_val = 0.0 then runtime_error "Division by zero in SLASH"
            else Stack.push (VFloat (b_val /. a_val)) stack
        | _ -> runtime_error "SLASH operation requires two floats");
        next ()
    | MOD ->
        push_trace pc "MOD";
        binary_op "MOD" mod_float Int64.rem;
        next ()
    | POW ->
        push_trace pc "POW";
        binary_op "POW" ( ** ) int64_pow;
        next ()
    | CONCAT ->
        push_trace pc "CONCAT";
        let b, a = pop2 "CONCAT" in
        let result =
          get_string_from_stack_value b ^ get_string_from_stack_value a
        in
        let v = Gc.alloc_string_with_gc stack env result in
        Stack.push v stack;
        next ()
    | JUMP_IF_FALSE offset -> (
        push_trace pc ("JUMP_IF_FALSE " ^ string_of_int offset);
        match Stack.pop_opt stack with
        | Some cond when cond = VFloat 0.0 ->
            execute instructions env (pc + offset)
        | Some _ -> next ()
        | None -> runtime_error "JUMP_IF_FALSE with empty stack")
    | JUMP n ->
        push_trace pc ("JUMP " ^ string_of_int n);
        execute instructions env (pc + n)
    | LESS ->
        push_trace pc "LESS";
        compare_op "LESS" ( < ) (fun a b -> Int64.compare a b < 0);
        next ()
    | GREATER ->
        push_trace pc "GREATER";
        compare_op "GREATER" ( > ) (fun a b -> Int64.compare a b > 0);
        next ()
    | LESS_EQUAL ->
        push_trace pc "LESS_EQUAL";
        compare_op "LESS_EQUAL" ( <= ) (fun a b -> Int64.compare a b <= 0);
        next ()
    | GREATER_EQUAL ->
        push_trace pc "GREATER_EQUAL";
        compare_op "GREATER_EQUAL" ( >= ) (fun a b -> Int64.compare a b >= 0);
        next ()
    | NOT_EQUAL ->
        push_trace pc "NOT_EQUAL";
        compare_op "NOT_EQUAL" ( <> ) (fun a b -> a <> b);
        next ()
    | EQUAL ->
        push_trace pc "EQUAL";
        let b, a = pop2 "EQUAL" in
        let result =
          match (a, b) with
          | VHeapRef id1, VHeapRef id2 -> (
              match (Gc.get_string id1, Gc.get_string id2) with
              | Some sa, Some sb -> sa = sb
              | _ -> id1 = id2)
          | VFloat f1, VFloat f2 -> f1 = f2
          | VInt64 i1, VInt64 i2 -> i1 = i2
          | _ -> false
        in
        Stack.push (VFloat (if result then 1.0 else 0.0)) stack;
        next ()
    | AND ->
        push_trace pc "AND";
        let a, b = pop2 "AND" in
        let bool_val = function
          | VFloat f -> f <> 0.0
          | VInt64 i -> i <> 0L
          | _ -> runtime_error "AND expects float or int64"
        in
        Stack.push
          (VFloat (if bool_val a && bool_val b then 1.0 else 0.0))
          stack;
        next ()
    | OR ->
        push_trace pc "OR";
        let a, b = pop2 "OR" in
        let bool_val = function
          | VFloat f -> f <> 0.0
          | VInt64 i -> i <> 0L
          | _ -> runtime_error "OR expects float or int64"
        in
        Stack.push
          (VFloat (if bool_val a || bool_val b then 1.0 else 0.0))
          stack;
        next ()
    | NOT ->
        push_trace pc "NOT";
        let v = pop1 "NOT" in
        let bool_val =
          match v with
          | VFloat f -> f <> 0.0
          | VInt64 i -> i <> 0L
          | _ -> runtime_error "NOT expects float or int64"
        in
        Stack.push (VFloat (if not bool_val then 1.0 else 0.0)) stack;
        next ()
    | INC ->
        push_trace pc "INC";
        let v = pop1 "INC" in
        let result =
          match v with
          | VFloat f -> VFloat (f +. 1.0)
          | VInt64 i -> VInt64 Int64.(add i 1L)
          | _ -> runtime_error "INC expects float or int64"
        in
        Stack.push result stack;
        next ()
    | DEC ->
        push_trace pc "DEC";
        let v = pop1 "DEC" in
        let result =
          match v with
          | VFloat f -> VFloat (f -. 1.0)
          | VInt64 i -> VInt64 Int64.(sub i 1L)
          | _ -> runtime_error "DEC expects float or int64"
        in
        Stack.push result stack;
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
          let items = List.init length (fun _ -> Stack.pop stack) |> List.rev in
          Stack.push (VArray items) stack;
          next ()
    | LOAD_INDEX ->
        push_trace pc "LOAD_INDEX";
        if Stack.length stack < 2 then
          runtime_error
            "LOAD_INDEX requires two values on the stack (collection, index)";
        let index =
          match Stack.pop stack with
          | VFloat f -> int_of_float f
          | VInt64 i -> Int64.to_int i
          | _ -> runtime_error "Expected float or int for index in LOAD_INDEX"
        in
        let collection =
          match Stack.pop stack with
          | VArray values -> values
          | VTuple values -> values
          | _ -> runtime_error "Expected array or tuple for LOAD_INDEX"
        in
        if index < 0 || index >= List.length collection then
          runtime_error
            ("Index out of bounds in LOAD_INDEX: " ^ string_of_int index)
        else Stack.push (List.nth collection index) stack;
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
        let arg_count = List.length param_names in

        if Stack.length stack < arg_count then
          runtime_error
            ("CALL to '" ^ function_name ^ "' requires "
           ^ string_of_int arg_count ^ " arguments, but stack has only "
            ^ string_of_int (Stack.length stack));

        let args = List.rev (List.init arg_count (fun _ -> Stack.pop stack)) in
        let local_env =
          List.combine param_names (List.map (fun v -> (v, true)) args)
        in

        let roots = Gc.get_stack_roots stack in
        Gc.maybe_collect_gc roots local_env;

        let skip_header = 1 + arg_count in
        let body = List.drop skip_header function_body in
        let return_value = execute (Array.of_list body) local_env 0 in
        Stack.push return_value stack;
        next ()
    | TAIL_CALL function_name ->
        push_trace pc ("TAIL_CALL " ^ function_name);
        let function_body = resolve_function_body function_name in
        let param_names = extract_param_names function_body in
        let arg_count = List.length param_names in

        if Stack.length stack < arg_count then
          runtime_error
            ("TAIL_CALL to '" ^ function_name ^ "' requires "
           ^ string_of_int arg_count ^ " arguments, but stack has only "
            ^ string_of_int (Stack.length stack));

        let args = List.rev (List.init arg_count (fun _ -> Stack.pop stack)) in
        let local_env =
          List.combine param_names (List.map (fun v -> (v, true)) args)
        in

        let skip_header = 1 + arg_count in
        let body = List.drop skip_header function_body in
        execute (Array.of_list body) local_env 0
    | RETURN ->
        push_trace pc "RETURN";
        if Stack.is_empty stack then (
          Stack.push (VFloat nan) stack;
          Stack.pop stack)
        else Stack.pop stack
    | STORE_VAR name ->
        push_trace pc ("STORE_VAR " ^ name);
        if Stack.is_empty stack then
          runtime_error ("STORE_VAR '" ^ name ^ "' failed: stack is empty")
        else
          let value = Stack.pop stack in
          if List.mem_assoc name env then
            let env =
              List.remove_assoc name env |> fun e -> (name, (value, true)) :: e
            in
            execute instructions env (pc + 1)
          else (
            global_env :=
              List.remove_assoc name !global_env @ [ (name, (value, true)) ];
            execute instructions env (pc + 1))
    | PRINTLN -> (
        push_trace pc "PRINTLN";
        if Stack.is_empty stack then
          runtime_error "PRINTLN attempted with empty stack"
        else
          let value = Stack.pop stack in
          match value with
          | VFloat f when Float.is_nan f ->
              Printf.printf "unit\n";
              next ()
          | VBool true ->
              Printf.printf "true\n";
              next ()
          | VBool false ->
              Printf.printf "false\n";
              next ()
          | VFloat f when Float.is_infinite f ->
              Printf.printf "%s\n" (if f > 0.0 then "inf" else "-inf");
              next ()
          | VFloat f ->
              Printf.printf "%.12g\n" f;
              next ()
          | VInt64 i ->
              Printf.printf "%Ld\n" i;
              next ()
          | VHeapRef id -> (
              match Gc.get_string id with
              | Some str ->
                  Printf.printf "%s\n" (replace_escape_sequences str);
                  next ()
              | None ->
                  runtime_error
                    ("PRINTLN failed: no string with id " ^ string_of_int id))
          | VArray items ->
              let string_of_value = function
                | VFloat f when Float.is_nan f -> "unit"
                | VBool true -> "true"
                | VBool false -> "false"
                | VFloat f when Float.is_infinite f ->
                    if f > 0.0 then "inf" else "-inf"
                | VFloat f -> Printf.sprintf "%.12g" f
                | VInt64 i -> Printf.sprintf "%Ld" i
                | VHeapRef id -> (
                    match Gc.get_string id with
                    | Some s -> "\"" ^ replace_escape_sequences s ^ "\""
                    | None -> "<invalid ref>")
                | VArray _ -> "[...]"
                | VTuple _ -> "(...)"
              in
              let contents =
                items |> List.map string_of_value |> String.concat ", "
              in
              Printf.printf "[%s]\n" contents;
              next ()
          | VTuple items ->
              let string_of_value = function
                | VFloat f when Float.is_nan f -> "unit"
                | VBool true -> "true"
                | VBool false -> "false"
                | VFloat f when Float.is_infinite f ->
                    if f > 0.0 then "inf" else "-inf"
                | VFloat f -> Printf.sprintf "%.12g" f
                | VInt64 i -> Printf.sprintf "%Ld" i
                | VHeapRef id -> (
                    match Gc.get_string id with
                    | Some s -> "\"" ^ replace_escape_sequences s ^ "\""
                    | None -> "<invalid ref>")
                | VArray _ -> "[...]"
                | VTuple _ -> "(...)"
              in
              let contents =
                items |> List.map string_of_value |> String.concat ", "
              in
              Printf.printf "(%s)\n" contents;
              next ())
    | INPUT -> (
        push_trace pc "INPUT";
        if Stack.is_empty stack then
          runtime_error "Stack underflow during INPUT"
        else
          let value = Stack.pop stack in
          let id =
            match value with
            | VHeapRef id -> id
            | _ -> runtime_error "INPUT expected a heap reference ID"
          in
          match Gc.get_string id with
          | Some prompt ->
              Printf.printf "%s" prompt;
              let input_value = read_line () in
              let processed_value =
                try VFloat (float_of_string input_value)
                with Failure _ ->
                  Gc.alloc_string_with_gc stack env input_value
              in
              Stack.push processed_value stack;
              next ()
          | None -> runtime_error "Invalid prompt ID for INPUT")
    | NEG ->
        push_trace pc "NEG";
        let v = Stack.pop stack in
        (match v with
        | VFloat f -> Stack.push (VFloat (-.f)) stack
        | _ -> runtime_error "NEG expects a float");
        next ()
    | FLOAT ->
        push_trace pc "FLOAT";
        let v = Stack.pop stack in
        let float_val =
          match v with
          | VFloat f -> f
          | VHeapRef id -> (
              match Gc.get_string id with
              | Some s -> (
                  try float_of_string s
                  with Failure _ ->
                    runtime_error ("FLOAT: invalid float string: " ^ s))
              | None -> runtime_error "FLOAT: invalid heap reference for string"
              )
          | _ -> runtime_error "FLOAT: unsupported type for float conversion"
        in
        Stack.push (VFloat float_val) stack;
        next ()
    | PLUSASSIGN ->
        push_trace pc "PLUSASSIGN";
        let value = pop1 "PLUSASSIGN" in
        let var = pop1 "PLUSASSIGN target" in
        let name =
          match var with
          | VHeapRef id -> (
              match Gc.get_string id with
              | Some name -> name
              | None ->
                  runtime_error "PLUSASSIGN: invalid variable name reference")
          | _ -> runtime_error "PLUSASSIGN: expected variable name"
        in
        let old_value = get_var env name in
        (match (old_value, value) with
        | VFloat oldf, VFloat newf ->
            let result = VFloat (oldf +. newf) in
            global_env :=
              List.remove_assoc name !global_env @ [ (name, (result, true)) ];
            Stack.push result stack
        | _ -> runtime_error "PLUSASSIGN: only float types are supported");
        next ()
    | MINUSASSIGN ->
        push_trace pc "MINUSASSIGN";
        let value = pop1 "MINUSASSIGN" in
        let var = pop1 "MINUSASSIGN target" in
        let name =
          match var with
          | VHeapRef id -> (
              match Gc.get_string id with
              | Some name -> name
              | None ->
                  runtime_error "MINUSASSIGN: invalid variable name reference")
          | _ -> runtime_error "MINUSASSIGN: expected variable name"
        in
        let old_value = get_var env name in
        (match (old_value, value) with
        | VFloat oldf, VFloat newf ->
            let result = VFloat (oldf -. newf) in
            global_env :=
              List.remove_assoc name !global_env @ [ (name, (result, true)) ];
            Stack.push result stack
        | _ -> runtime_error "MINUSASSIGN: only float types are supported");
        next ()
    | STARASSIGN ->
        push_trace pc "STARASSIGN";
        let value = pop1 "STARASSIGN" in
        let var = pop1 "STARASSIGN target" in
        let name =
          match var with
          | VHeapRef id -> (
              match Gc.get_string id with
              | Some name -> name
              | None ->
                  runtime_error "STARASSIGN: invalid variable name reference")
          | _ -> runtime_error "STARASSIGN: expected variable name"
        in
        let old_value = get_var env name in
        (match (old_value, value) with
        | VFloat oldf, VFloat newf ->
            let result = VFloat (oldf *. newf) in
            global_env :=
              List.remove_assoc name !global_env @ [ (name, (result, true)) ];
            Stack.push result stack
        | _ -> runtime_error "STARASSIGN: only float types are supported");
        next ()
    | SLASHASSIGN ->
        push_trace pc "SLASHASSIGN";
        let value = pop1 "SLASHASSIGN" in
        let var = pop1 "SLASHASSIGN target" in
        let name =
          match var with
          | VHeapRef id -> (
              match Gc.get_string id with
              | Some name -> name
              | None ->
                  runtime_error "SLASHASSIGN: invalid variable name reference")
          | _ -> runtime_error "SLASHASSIGN: expected variable name"
        in
        let old_value = get_var env name in
        (match (old_value, value) with
        | VFloat oldf, VFloat newf ->
            if newf = 0.0 then runtime_error "SLASHASSIGN: division by zero";
            let result = VFloat (oldf /. newf) in
            global_env :=
              List.remove_assoc name !global_env @ [ (name, (result, true)) ];
            Stack.push result stack
        | _ -> runtime_error "SLASHASSIGN: only float types are supported");
        next ()
    | LOAD_VAR_REF name ->
        push_trace pc ("LOAD_VAR_REF " ^ name);
        let v = Gc.alloc_string_with_gc stack env name in
        Stack.push v stack;
        next ()

let run instructions =
  try
    let result = execute (Array.of_list instructions) [] 0 in
    clear_trace ();
    result
  with RuntimeError msg ->
    prerr_endline msg;
    exit 1
