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
let bool_to_float b = if b then 1.0 else 0.0
let bool_to_int64 b = if b then 1L else 0L

let escape_sequences =
  [
    ("\\n", '\n'); ("\\t", '\t'); ("\\r", '\r'); ("\\\\", '\\'); ("\\0", '\000');
  ]

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

let rec pop_n acc n =
  if n <= 0 then acc
  else
    match Stack.pop_opt stack with
    | Some v -> pop_n (v :: acc) (n - 1)
    | None -> runtime_error "Stack underflow in pop_n"

let rec pop_n_rev acc n =
  if n = 0 then acc else pop_n_rev (Stack.pop stack :: acc) (n - 1)

let pop1 stack =
  match Stack.pop_opt stack with
  | Some v -> v
  | None -> runtime_error "Stack underflow"

let pop2 stack =
  match (Stack.pop_opt stack, Stack.pop_opt stack) with
  | Some b, Some a -> (a, b)
  | _ -> runtime_error "Stack underflow"

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
    | LOAD_BINARY v ->
        push_trace pc ("LOAD_BINARY " ^ string_of_int v);
        Stack.push (VInt v) stack;
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
          let items = pop_n_rev [] n in
          Stack.push (VTuple items) stack;
          next ()
    | LOAD_VAR name ->
        push_trace pc ("LOAD_VAR " ^ name);
        Stack.push (get_var env name) stack;
        next ()
    | PLUS ->
        push_trace pc "PLUS";
        let a, b = pop2 stack in
        (match (a, b) with
        | VFloat a, VFloat b -> Stack.push (VFloat (a +. b)) stack
        | VInt64 a, VInt64 b -> Stack.push (VInt64 (Int64.add a b)) stack
        | _ -> runtime_error "PLUS expects numbers");
        next ()
    | MINUS ->
        push_trace pc "MINUS";
        let a, b = pop2 stack in
        (match (a, b) with
        | VFloat a, VFloat b -> Stack.push (VFloat (a -. b)) stack
        | VInt64 a, VInt64 b -> Stack.push (VInt64 (Int64.sub a b)) stack
        | _ -> runtime_error "MINUS expects numbers");
        next ()
    | STAR ->
        push_trace pc "STAR";
        let a, b = pop2 stack in
        (match (a, b) with
        | VFloat a, VFloat b -> Stack.push (VFloat (a *. b)) stack
        | VInt64 a, VInt64 b -> Stack.push (VInt64 (Int64.mul a b)) stack
        | _ -> runtime_error "STAR expects numbers");
        next ()
    | SLASH ->
        push_trace pc "SLASH";
        let a, b = pop2 stack in
        (match (a, b) with
        | VFloat _, VFloat 0.0 -> runtime_error "Division by zero"
        | VInt64 _, VInt64 0L -> runtime_error "Division by zero"
        | VFloat a, VFloat b -> Stack.push (VFloat (a /. b)) stack
        | VInt64 a, VInt64 b -> Stack.push (VInt64 (Int64.div a b)) stack
        | _ -> runtime_error "SLASH expects numbers");
        next ()
    | MOD ->
        push_trace pc "MOD";
        let a, b = pop2 stack in
        (match (a, b) with
        | VFloat _, VFloat 0.0 -> runtime_error "Modulo by zero"
        | VInt64 _, VInt64 0L -> runtime_error "Modulo by zero"
        | VFloat a, VFloat b -> Stack.push (VFloat (mod_float a b)) stack
        | VInt64 a, VInt64 b -> Stack.push (VInt64 (Int64.rem a b)) stack
        | _ -> runtime_error "MOD expects numbers");
        next ()
    | POW ->
        push_trace pc "POW";
        let a, b = pop2 stack in
        (match (a, b) with
        | VFloat a, VFloat b -> Stack.push (VFloat (a ** b)) stack
        | VInt64 a, VInt64 b -> (
            if b < 0L then runtime_error "POW expects non-negative exponent"
            else
              try Stack.push (VInt64 (int64_pow a b)) stack
              with _ -> runtime_error "POW overflow")
        | _ -> runtime_error "POW expects numbers");
        next ()
    | CONCAT ->
        push_trace pc "CONCAT";
        let b, a = pop2 stack in
        let result =
          get_string_from_stack_value b ^ get_string_from_stack_value a
        in
        let v = Gc.alloc_string_with_gc stack env result in
        Stack.push v stack;
        next ()
    | JUMP_IF_FALSE offset -> (
        push_trace pc ("JUMP_IF_FALSE " ^ string_of_int offset);
        match Stack.pop_opt stack with
        | Some (VFloat 0.0) | Some (VInt64 0L) | Some (VBool false) ->
            execute instructions env (pc + offset)
        | Some _ -> next ()
        | None -> runtime_error "JUMP_IF_FALSE with empty stack")
    | JUMP n ->
        push_trace pc ("JUMP " ^ string_of_int n);
        execute instructions env (pc + n)
    | LESS ->
        push_trace pc "LESS";
        let a, b = pop2 stack in
        let result =
          match (a, b) with
          | VFloat a, VFloat b -> VFloat (bool_to_float (a < b))
          | VInt64 a, VInt64 b -> VInt64 (bool_to_int64 (Int64.compare a b < 0))
          | _ -> runtime_error "LESS expects two floats or int64"
        in
        Stack.push result stack;
        next ()
    | GREATER ->
        push_trace pc "GREATER";
        let a, b = pop2 stack in
        let result =
          match (a, b) with
          | VFloat a, VFloat b -> VFloat (bool_to_float (a > b))
          | VInt64 a, VInt64 b -> VInt64 (bool_to_int64 (Int64.compare a b > 0))
          | _ -> runtime_error "GREATER expects two floats or int64"
        in
        Stack.push result stack;
        next ()
    | LESS_EQUAL ->
        push_trace pc "LESS_EQUAL";
        let a, b = pop2 stack in
        let result =
          match (a, b) with
          | VFloat a, VFloat b -> VFloat (bool_to_float (a <= b))
          | VInt64 a, VInt64 b ->
              VInt64 (bool_to_int64 (Int64.compare a b <= 0))
          | _ -> runtime_error "LESS_EQUAL expects two floats or int64"
        in
        Stack.push result stack;
        next ()
    | GREATER_EQUAL ->
        push_trace pc "GREATER_EQUAL";
        let a, b = pop2 stack in
        let result =
          match (a, b) with
          | VFloat a, VFloat b -> VFloat (bool_to_float (a >= b))
          | VInt64 a, VInt64 b ->
              VInt64 (bool_to_int64 (Int64.compare a b >= 0))
          | _ -> runtime_error "GREATER_EQUAL expects two floats or int64"
        in
        Stack.push result stack;
        next ()
    | NOT_EQUAL ->
        push_trace pc "NOT_EQUAL";
        let a, b = pop2 stack in
        let result =
          match (a, b) with
          | VFloat a, VFloat b -> VFloat (bool_to_float (a <> b))
          | VInt64 a, VInt64 b -> VInt64 (bool_to_int64 (a <> b))
          | _ -> runtime_error "NOT_EQUAL expects two floats or int64"
        in
        Stack.push result stack;
        next ()
    | EQUAL ->
        push_trace pc "EQUAL";
        let b, a = pop2 stack in
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
        let a, b = pop2 stack in
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
        let a, b = pop2 stack in
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
        let v = pop1 stack in
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
        let v = pop1 stack in
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
        let v = pop1 stack in
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
          let items = pop_n_rev [] length in
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
        let collection = Stack.pop stack in
        let values =
          match collection with
          | VArray items -> items
          | VTuple items -> items
          | _ -> runtime_error "Expected array or tuple for LOAD_INDEX"
        in
        if index < 0 || index >= List.length values then
          runtime_error
            ("Index out of bounds in LOAD_INDEX: " ^ string_of_int index)
        else
          let item = List.nth values index in
          Stack.push item stack;
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

        let args = pop_n [] arg_count in
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

        let args = pop_n [] arg_count in
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
    | PRINTLN ->
        push_trace pc "PRINTLN";
        if Stack.is_empty stack then
          runtime_error "PRINTLN attempted with empty stack"
        else
          let rec string_of_value = function
            | VUnit -> "unit"
            | VByte c -> Printf.sprintf "'%c'" c
            | VBool true -> "true"
            | VBool false -> "false"
            | VFloat f when Float.is_nan f -> "unit"
            | VFloat 1.0 -> "true"
            | VFloat 0.0 -> "false"
            | VFloat f when Float.is_infinite f ->
                if f > 0.0 then "inf" else "-inf"
            | VFloat f -> Printf.sprintf "%.12f" f
            | VInt64 i -> Printf.sprintf "%Ld" i
            | VInt i -> Printf.sprintf "%d" i
            | VHeapRef id -> (
                match Gc.get_string id with
                | Some s -> "" ^ replace_escape_sequences s ^ ""
                | None -> "<invalid ref>")
            | VArray items ->
                let contents =
                  items |> List.map string_of_value |> String.concat ", "
                in
                "[" ^ contents ^ "]"
            | VTuple items ->
                let contents =
                  items |> List.map string_of_value |> String.concat ", "
                in
                "(" ^ contents ^ ")"
          in
          let value = Stack.pop stack in
          Printf.printf "%s\n" (string_of_value value);
          next ()
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
        | VInt64 i -> Stack.push (VInt64 (Int64.neg i)) stack
        | _ -> runtime_error "NEG expects a float or int");
        next ()
    | FLOAT ->
        push_trace pc "FLOAT";
        let v = Stack.pop stack in
        let float_val =
          match v with
          | VFloat f -> f
          | VInt64 i -> Int64.to_float i
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
    | INT ->
        push_trace pc "INT";
        let v = Stack.pop stack in
        let int_val =
          match v with
          | VInt64 i -> i
          | VFloat f -> Int64.of_float f
          | VHeapRef id -> (
              match Gc.get_string id with
              | Some s -> (
                  try Int64.of_string s
                  with Failure _ ->
                    runtime_error ("INT: invalid int string: " ^ s))
              | None -> runtime_error "INT: invalid heap reference for string")
          | _ -> runtime_error "INT: unsupported type for int conversion"
        in
        Stack.push (VInt64 int_val) stack;
        next ()
    | STRING ->
        push_trace pc "STRING";
        let v = Stack.pop stack in
        let str_val =
          match v with
          | VHeapRef id -> (
              match Gc.get_string id with
              | Some s -> s
              | None ->
                  runtime_error "STRING: invalid heap reference for string")
          | VInt64 i -> Int64.to_string i
          | VFloat f -> string_of_float f
          | VBool b -> if b then "true" else "false"
          | VByte c -> String.make 1 c
          | VUnit -> "()"
          | _ -> runtime_error "STRING: unsupported type for string conversion"
        in
        let new_str_ref = Gc.alloc_string_with_gc stack env str_val in
        Stack.push new_str_ref stack;
        next ()
    | PLUSASSIGN ->
        push_trace pc "PLUSASSIGN";
        let value = pop1 stack in
        let var = pop1 stack in
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
        let result =
          match (old_value, value) with
          | VFloat oldf, VFloat newf -> VFloat (oldf +. newf)
          | VInt64 oldi, VInt64 newi -> VInt64 (Int64.add oldi newi)
          | _ -> runtime_error "PLUSASSIGN: type mismatch"
        in
        let env = update_variable name result env in
        Stack.push result stack;
        execute instructions env (pc + 1)
    | MINUSASSIGN ->
        push_trace pc "MINUSASSIGN";
        let value = pop1 stack in
        let var = pop1 stack in
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
        let result =
          match (old_value, value) with
          | VFloat oldf, VFloat newf -> VFloat (oldf -. newf)
          | VInt64 oldi, VInt64 newi -> VInt64 (Int64.sub oldi newi)
          | _ -> runtime_error "MINUSASSIGN: type mismatch"
        in
        let env = update_variable name result env in
        Stack.push result stack;
        execute instructions env (pc + 1)
    | STARASSIGN ->
        push_trace pc "STARASSIGN";
        let value = pop1 stack in
        let var = pop1 stack in
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
        let result =
          match (old_value, value) with
          | VFloat oldf, VFloat newf -> VFloat (oldf *. newf)
          | VInt64 oldi, VInt64 newi -> VInt64 (Int64.mul oldi newi)
          | _ -> runtime_error "STARASSIGN: type mismatch"
        in
        let env = update_variable name result env in
        Stack.push result stack;
        execute instructions env (pc + 1)
    | SLASHASSIGN ->
        push_trace pc "SLASHASSIGN";
        let value = pop1 stack in
        let var = pop1 stack in
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
        let result =
          match (old_value, value) with
          | VFloat oldf, VFloat newf ->
              if newf = 0.0 then runtime_error "SLASHASSIGN: division by zero";
              VFloat (oldf /. newf)
          | VInt64 oldi, VInt64 newi ->
              if newi = 0L then runtime_error "SLASHASSIGN: division by zero";
              VInt64 (Int64.div oldi newi)
          | _ -> runtime_error "SLASHASSIGN: type mismatch"
        in
        let env = update_variable name result env in
        Stack.push result stack;
        execute instructions env (pc + 1)
    | LOAD_VAR_REF name ->
        push_trace pc ("LOAD_VAR_REF " ^ name);
        let v = Gc.alloc_string_with_gc stack env name in
        Stack.push v stack;
        next ()

and update_variable name result env =
  let updated = ref false in
  let new_env =
    List.map
      (fun (k, (v, mutable_)) ->
        if k = name then (
          updated := true;
          (k, (result, mutable_)))
        else (k, (v, mutable_)))
      env
  in
  if !updated then new_env
  else
    let updated_globals =
      List.map
        (fun (k, (v, mutable_)) ->
          if k = name then (k, (result, mutable_)) else (k, (v, mutable_)))
        !global_env
    in
    if List.exists (fun (k, _) -> k = name) !global_env then
      global_env := updated_globals
    else global_env := (name, (result, true)) :: !global_env;
    env

let run instructions =
  try
    let result = execute (Array.of_list instructions) [] 0 in
    clear_trace ();
    result
  with RuntimeError msg ->
    prerr_endline msg;
    exit 1
