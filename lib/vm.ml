open Opcode
open Gc

exception RuntimeError of string

type trace_entry = { pc : int; instr : string }

type frame = {
  code : opcode array;
  mutable pc : int;
  mutable env : (string * (value * bool)) list;
}

let trace : trace_entry list ref = ref []
let push_trace pc instr = trace := { pc; instr } :: !trace
let clear_trace () = trace := []

let print_trace msg =
  match !trace with
  | { pc; instr } :: _ ->
      prerr_endline "Stack Trace (most recent call last):";
      prerr_endline
        ("  at instruction '" ^ instr ^ "' (pc=" ^ string_of_int pc ^ ")");
      prerr_endline ("Fatal error: " ^ msg)
  | [] -> prerr_endline ("Fatal error: " ^ msg)

let runtime_error msg = raise (RuntimeError msg)
let stack : Gc.value Stack.t = Stack.create ()
let global_env : (string * (Gc.value * bool)) list ref = ref []
let bool_to_float b = if b then 1.0 else 0.0
let bool_to_int64 b = if b then 1L else 0L
let bool_to_int32 b = if b then 1l else 0l
let bool_to_uint32 b = if b then Uint32.one else Uint32.zero
let bool_to_uint64 b = if b then Uint64.one else Uint64.zero
let output_buffer : Buffer.t = Buffer.create 1024

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
  else if exp = 0L then 1L
  else if Int64.rem exp 2L = 0L then
    let half = int64_pow base (Int64.div exp 2L) in
    let prod = Int64.mul half half in
    if half <> 0L && Int64.div prod half <> half then
      invalid_arg "int64_pow: overflow"
    else prod
  else
    let tail = int64_pow base (Int64.sub exp 1L) in
    let prod = Int64.mul base tail in
    if base <> 0L && Int64.div prod base <> tail then
      invalid_arg "int64_pow: overflow"
    else prod

let rec int32_pow base exp =
  if exp < 0l then invalid_arg "int32_pow: negative exponent"
  else if exp = 0l then 1l
  else if Int32.rem exp 2l = 0l then
    let half = int32_pow base (Int32.div exp 2l) in
    let prod = Int32.mul half half in
    if half <> 0l && Int32.div prod half <> half then
      invalid_arg "int32_pow: overflow"
    else prod
  else
    let tail = int32_pow base (Int32.sub exp 1l) in
    let prod = Int32.mul base tail in
    if base <> 0l && Int32.div prod base <> tail then
      invalid_arg "int32_pow: overflow"
    else prod

let rec uint64_pow base exp =
  if exp < 0L then invalid_arg "uint64_pow: negative exponent"
  else if exp = 0L then Uint64.one
  else if Uint64.rem exp 2L = 0L then
    let half = uint64_pow base (Uint64.div exp 2L) in
    let prod = Uint64.mul half half in
    if half <> 0L && Uint64.div prod half <> half then
      invalid_arg "uint64_pow: overflow"
    else prod
  else
    let tail = uint64_pow base (Uint64.sub exp 1L) in
    let prod = Uint64.mul base tail in
    if base <> 0L && Uint64.div prod base <> tail then
      invalid_arg "uint64_pow: overflow"
    else prod

let rec uint32_pow base exp =
  if exp < 0l then invalid_arg "uint32_pow: negative exponent"
  else if exp = 0l then Uint32.one
  else if Uint32.rem exp 2l = 0l then
    let half = uint32_pow base (Uint32.div exp 2l) in
    let prod = Uint32.mul half half in
    if half <> 0l && Uint32.div prod half <> half then
      invalid_arg "uint32_pow: overflow"
    else prod
  else
    let tail = uint32_pow base (Uint32.sub exp 1l) in
    let prod = Uint32.mul base tail in
    if base <> 0l && Uint32.div prod base <> tail then
      invalid_arg "uint32_pow: overflow"
    else prod

let update_variable name result env =
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

let run (instructions : opcode list) =
  clear_trace ();
  let frame_stack = Stack.create () in
  let push_frame code env = Stack.push { code; pc = 0; env } frame_stack in
  push_frame (Array.of_list instructions) [];
  try
    while not (Stack.is_empty frame_stack) do
      let frame = Stack.top frame_stack in
      if frame.pc >= Array.length frame.code then (
        ignore (Stack.pop frame_stack);
        ())
      else
        let instr = frame.code.(frame.pc) in
        let next () = frame.pc <- frame.pc + 1 in
        match instr with
        | LOAD_INT64 v ->
            push_trace frame.pc ("LOAD_INT64 " ^ Int64.to_string v);
            Stack.push (VInt64 v) stack;
            next ()
        | LOAD_INT32 v ->
            push_trace frame.pc ("LOAD_INT32 " ^ Int32.to_string v);
            Stack.push (VInt32 v) stack;
            next ()
        | LOAD_UINT64 u ->
            push_trace frame.pc ("LOAD_UINT64 " ^ Uint64.to_string u);
            Stack.push (VUint64 u) stack;
            next ()
        | LOAD_UINT32 u ->
            push_trace frame.pc ("LOAD_UINT32 " ^ Uint32.to_string u);
            Stack.push (VUint32 u) stack;
            next ()
        | LOAD_BINARY v ->
            push_trace frame.pc ("LOAD_BINARY " ^ string_of_int v);
            Stack.push (VInt v) stack;
            next ()
        | LOAD_FLOAT v ->
            push_trace frame.pc ("LOAD_FLOAT " ^ string_of_float v);
            Stack.push (VFloat v) stack;
            next ()
        | LOAD_STRING s ->
            push_trace frame.pc ("LOAD_STRING " ^ s);
            let v = Gc.alloc_string_with_gc stack frame.env s in
            Stack.push v stack;
            next ()
        | LOAD_BYTE c ->
            push_trace frame.pc ("LOAD_BYTE " ^ String.make 1 c);
            let v = Gc.alloc_string_with_gc stack frame.env (String.make 1 c) in
            Stack.push v stack;
            next ()
        | LOAD_BOOL b ->
            push_trace frame.pc ("LOAD_BOOL " ^ string_of_bool b);
            Stack.push (VBool b) stack;
            next ()
        | LOAD_UNIT _ ->
            push_trace frame.pc "LOAD_UNIT";
            Stack.push (VFloat nan) stack;
            next ()
        | LOAD_TUPLE n ->
            push_trace frame.pc ("LOAD_TUPLE " ^ string_of_int n);
            if Stack.length stack < n then
              runtime_error
                ("LOAD_TUPLE expects " ^ string_of_int n
               ^ " values on the stack")
            else
              let items = pop_n_rev [] n in
              Stack.push (VTuple items) stack;
              next ()
        | LOAD_VAR name ->
            push_trace frame.pc ("LOAD_VAR " ^ name);
            Stack.push (get_var frame.env name) stack;
            next ()
        | PLUS ->
            push_trace frame.pc "PLUS";
            let a, b = pop2 stack in
            (match (a, b) with
            | VFloat a, VFloat b -> Stack.push (VFloat (a +. b)) stack
            | VInt64 a, VInt64 b ->
                let sum = Int64.add a b in
                if
                  (a > 0L && b > 0L && sum < 0L)
                  || (a < 0L && b < 0L && sum > 0L)
                then runtime_error "int64 addition overflow"
                else Stack.push (VInt64 sum) stack
            | VInt32 a, VInt32 b ->
                let sum = Int32.add a b in
                if
                  (a > 0l && b > 0l && sum < 0l)
                  || (a < 0l && b < 0l && sum > 0l)
                then runtime_error "int32 addition overflow"
                else Stack.push (VInt32 sum) stack
            | VUint32 a, VUint32 b ->
                let sum = Uint32.add a b in
                if
                  (a > 0l && b > 0l && sum < 0l)
                  || (a < 0l && b < 0l && sum > 0l)
                then runtime_error "uint32 addition overflow"
                else Stack.push (VUint32 sum) stack
            | VUint64 a, VUint64 b ->
                let sum = Uint64.add a b in
                if
                  (a > 0L && b > 0L && sum < 0L)
                  || (a < 0L && b < 0L && sum > 0L)
                then runtime_error "uint64 addition overflow"
                else Stack.push (VUint64 sum) stack
            | _ -> runtime_error "PLUS expects numbers");
            next ()
        | MINUS ->
            push_trace frame.pc "MINUS";
            let a, b = pop2 stack in
            (match (a, b) with
            | VFloat a, VFloat b -> Stack.push (VFloat (a -. b)) stack
            | VInt64 a, VInt64 b ->
                let diff = Int64.sub a b in
                if
                  (b > 0L && a < Int64.add Int64.min_int b)
                  || (b < 0L && a > Int64.add Int64.max_int b)
                then runtime_error "int64 subtraction overflow"
                else Stack.push (VInt64 diff) stack
            | VInt32 a, VInt32 b ->
                let diff = Int32.sub a b in
                if
                  (b > 0l && a < Int32.add Int32.min_int b)
                  || (b < 0l && a > Int32.add Int32.max_int b)
                then runtime_error "int32 subtraction overflow"
                else Stack.push (VInt32 diff) stack
            | VUint32 a, VUint32 b ->
                let diff = Uint32.sub a b in
                if
                  (a > 0l && b > 0l && diff < 0l)
                  || (a < 0l && b < 0l && diff > 0l)
                then runtime_error "uint32 subtraction overflow"
                else Stack.push (VUint32 diff) stack
            | VUint64 a, VUint64 b ->
                let diff = Uint64.sub a b in
                if
                  (a > 0L && b > 0L && diff < 0L)
                  || (a < 0L && b < 0L && diff > 0L)
                then runtime_error "uint64 subtraction overflow"
                else Stack.push (VUint64 diff) stack
            | _ -> runtime_error "MINUS expects numbers");
            next ()
        | STAR ->
            push_trace frame.pc "STAR";
            let a, b = pop2 stack in
            (match (a, b) with
            | VFloat a, VFloat b -> Stack.push (VFloat (a *. b)) stack
            | VInt64 a, VInt64 b ->
                let prod = Int64.mul a b in
                if a <> 0L && Int64.div prod a <> b then
                  runtime_error "int64 multiplication overflow"
                else Stack.push (VInt64 prod) stack
            | VInt32 a, VInt32 b ->
                let prod = Int32.mul a b in
                if a <> 0l && Int32.div prod a <> b then
                  runtime_error "int32 multiplication overflow"
                else Stack.push (VInt32 prod) stack
            | VUint32 a, VUint32 b ->
                let prod = Uint32.mul a b in
                if b <> 0l && Uint32.div prod b <> a then
                  runtime_error "uint32 multiplication overflow"
                else Stack.push (VUint32 prod) stack
            | VUint64 a, VUint64 b ->
                let prod = Uint64.mul a b in
                if b <> 0L && Uint64.div prod b <> a then
                  runtime_error "uint64 multiplication overflow"
                else Stack.push (VUint64 prod) stack
            | _ -> runtime_error "STAR expects numbers");
            next ()
        | SLASH ->
            push_trace frame.pc "SLASH";
            let a, b = pop2 stack in
            (match (a, b) with
            | VFloat _, VFloat 0.0 -> runtime_error "Division by zero"
            | VInt64 _, VInt64 0L -> runtime_error "Division by zero"
            | VInt32 _, VInt32 0l -> runtime_error "Division by zero"
            | VFloat a, VFloat b -> Stack.push (VFloat (a /. b)) stack
            | VInt64 a, VInt64 b -> Stack.push (VInt64 (Int64.div a b)) stack
            | VInt32 a, VInt32 b -> Stack.push (VInt32 (Int32.div a b)) stack
            | VUint32 a, VUint32 b ->
                Stack.push (VUint32 (Uint32.div a b)) stack
            | VUint64 a, VUint64 b ->
                Stack.push (VUint64 (Uint64.div a b)) stack
            | _ -> runtime_error "SLASH expects numbers");
            next ()
        | MOD ->
            push_trace frame.pc "MOD";
            let a, b = pop2 stack in
            (match (a, b) with
            | VFloat _, VFloat 0.0 -> runtime_error "Modulo by zero"
            | VInt64 _, VInt64 0L -> runtime_error "Modulo by zero"
            | VInt32 _, VInt32 0l -> runtime_error "Modulo by zero"
            | VFloat a, VFloat b -> Stack.push (VFloat (mod_float a b)) stack
            | VInt64 a, VInt64 b -> Stack.push (VInt64 (Int64.rem a b)) stack
            | VInt32 a, VInt32 b -> Stack.push (VInt32 (Int32.rem a b)) stack
            | VUint32 a, VUint32 b ->
                Stack.push (VUint32 (Uint32.rem a b)) stack
            | VUint64 a, VUint64 b ->
                Stack.push (VUint64 (Uint64.rem a b)) stack
            | _ -> runtime_error "MOD expects numbers");
            next ()
        | POW ->
            push_trace frame.pc "POW";
            let a, b = pop2 stack in
            (match (a, b) with
            | VFloat a, VFloat b -> Stack.push (VFloat (a ** b)) stack
            | VInt64 a, VInt64 b -> (
                if b < 0L then runtime_error "POW expects non-negative exponent"
                else
                  try Stack.push (VInt64 (int64_pow a b)) stack
                  with _ -> runtime_error "POW overflow")
            | VInt32 a, VInt32 b -> (
                if b < 0l then runtime_error "POW expects non-negative exponent"
                else
                  try Stack.push (VInt32 (int32_pow a b)) stack
                  with _ -> runtime_error "POW overflow")
            | VUint32 a, VUint32 b -> (
                if b < 0l then runtime_error "POW expects non-negative exponent"
                else
                  try Stack.push (VUint32 (uint32_pow a b)) stack
                  with _ -> runtime_error "POW overflow")
            | VUint64 a, VUint64 b -> (
                if b < 0L then runtime_error "POW expects non-nigative exponent"
                else
                  try Stack.push (VUint64 (uint64_pow a b)) stack
                  with _ -> runtime_error "POW overflow")
            | _ -> runtime_error "POW expects numbers");
            next ()
        | CONCAT ->
            push_trace frame.pc "CONCAT";
            let b, a = pop2 stack in
            let result =
              get_string_from_stack_value b ^ get_string_from_stack_value a
            in
            let v = Gc.alloc_string_with_gc stack frame.env result in
            Stack.push v stack;
            next ()
        | JUMP_IF_FALSE offset -> (
            push_trace frame.pc ("JUMP_IF_FALSE " ^ string_of_int offset);
            match Stack.pop_opt stack with
            | Some (VFloat 0.0) | Some (VInt64 0L) | Some (VBool false) ->
                frame.pc <- frame.pc + offset
            | Some _ -> next ()
            | None -> runtime_error "JUMP_IF_FALSE with empty stack")
        | JUMP n ->
            push_trace frame.pc ("JUMP " ^ string_of_int n);
            frame.pc <- frame.pc + n
        | LESS ->
            push_trace frame.pc "LESS";
            let a, b = pop2 stack in
            let result =
              match (a, b) with
              | VFloat a, VFloat b -> VFloat (bool_to_float (a < b))
              | VInt64 a, VInt64 b ->
                  VInt64 (bool_to_int64 (Int64.compare a b < 0))
              | VInt32 a, VInt32 b ->
                  VInt32 (bool_to_int32 (Int32.compare a b < 0))
              | VUint32 a, VUint32 b ->
                  VUint32
                    (if Uint32.compare a b < 0 then Uint32.one else Uint32.zero)
              | VUint64 a, VUint64 b ->
                  VUint64
                    (if Uint64.compare a b < 0 then Uint64.one else Uint64.zero)
              | _ -> runtime_error "LESS expects two floats or int64"
            in
            Stack.push result stack;
            next ()
        | GREATER ->
            push_trace frame.pc "GREATER";
            let a, b = pop2 stack in
            let result =
              match (a, b) with
              | VFloat a, VFloat b -> VFloat (bool_to_float (a > b))
              | VInt64 a, VInt64 b ->
                  VInt64 (bool_to_int64 (Int64.compare a b > 0))
              | VInt32 a, VInt32 b ->
                  VInt32 (bool_to_int32 (Int32.compare a b > 0))
              | VUint32 a, VUint32 b ->
                  VUint32
                    (if Uint32.compare a b > 0 then Uint32.one else Uint32.zero)
              | VUint64 a, VUint64 b ->
                  VUint64
                    (if Uint64.compare a b > 0 then Uint64.one else Uint64.zero)
              | _ -> runtime_error "GREATER expects two floats or int64"
            in
            Stack.push result stack;
            next ()
        | LESS_EQUAL ->
            push_trace frame.pc "LESS_EQUAL";
            let a, b = pop2 stack in
            let result =
              match (a, b) with
              | VFloat a, VFloat b -> VFloat (bool_to_float (a <= b))
              | VInt64 a, VInt64 b ->
                  VInt64 (bool_to_int64 (Int64.compare a b <= 0))
              | VInt32 a, VInt32 b ->
                  VInt32 (bool_to_int32 (Int32.compare a b <= 0))
              | VUint32 a, VUint32 b ->
                  VUint32
                    (if Uint32.compare a b <= 0 then Uint32.one else Uint32.zero)
              | VUint64 a, VUint64 b ->
                  VUint64
                    (if Uint64.compare a b <= 0 then Uint64.one else Uint64.zero)
              | _ -> runtime_error "LESS_EQUAL expects two floats or int64"
            in
            Stack.push result stack;
            next ()
        | GREATER_EQUAL ->
            push_trace frame.pc "GREATER_EQUAL";
            let a, b = pop2 stack in
            let result =
              match (a, b) with
              | VFloat a, VFloat b -> VFloat (bool_to_float (a >= b))
              | VInt64 a, VInt64 b ->
                  VInt64 (bool_to_int64 (Int64.compare a b >= 0))
              | VInt32 a, VInt32 b ->
                  VInt32 (bool_to_int32 (Int32.compare a b >= 0))
              | VUint32 a, VUint32 b ->
                  VUint32
                    (if Uint32.compare a b >= 0 then Uint32.one else Uint32.zero)
              | VUint64 a, VUint64 b ->
                  VUint64
                    (if Uint64.compare a b >= 0 then Uint64.one else Uint64.zero)
              | _ -> runtime_error "GREATER_EQUAL expects two floats or int64"
            in
            Stack.push result stack;
            next ()
        | NOT_EQUAL ->
            push_trace frame.pc "NOT_EQUAL";
            let a, b = pop2 stack in
            let result =
              match (a, b) with
              | VFloat a, VFloat b -> VFloat (bool_to_float (a <> b))
              | VInt64 a, VInt64 b -> VInt64 (bool_to_int64 (a <> b))
              | VUint32 a, VUint32 b -> VUint32 (bool_to_uint32 (a <> b))
              | VUint64 a, VUint64 b -> VUint64 (bool_to_uint64 (a <> b))
              | VInt32 a, VInt32 b -> VInt32 (bool_to_int32 (a <> b))
              | _ -> runtime_error "NOT_EQUAL expects two floats or int64"
            in
            Stack.push result stack;
            next ()
        | EQUAL ->
            push_trace frame.pc "EQUAL";
            let b, a = pop2 stack in
            let result =
              match (a, b) with
              | VHeapRef id1, VHeapRef id2 -> (
                  match (Gc.get_string id1, Gc.get_string id2) with
                  | Some sa, Some sb -> sa = sb
                  | _ -> id1 = id2)
              | VFloat f1, VFloat f2 -> f1 = f2
              | VInt64 i1, VInt64 i2 -> i1 = i2
              | VUint32 u1, VUint32 u2 -> u1 = u2
              | VUint64 u1, VUint64 u2 -> u1 = u2
              | VInt32 i1, VInt32 i2 -> i1 = i2
              | _ -> false
            in
            Stack.push (VBool result) stack;
            next ()
        | AND ->
            push_trace frame.pc "AND";
            let a, b = pop2 stack in
            let bool_val = function
              | VBool b -> b
              | VFloat f -> f <> 0.0
              | VInt64 i -> i <> 0L
              | VInt32 i -> i <> 0l
              | VUint32 u -> u <> Uint32.zero
              | VUint64 u -> u <> Uint64.zero
              | _ -> runtime_error "AND expects bool, float, or int64"
            in
            Stack.push (VBool (bool_val a && bool_val b)) stack;
            next ()
        | OR ->
            push_trace frame.pc "OR";
            let a, b = pop2 stack in
            let bool_val = function
              | VBool b -> b
              | VFloat f -> f <> 0.0
              | VInt64 i -> i <> 0L
              | VInt32 i -> i <> 0l
              | VUint32 u -> u <> Uint32.zero
              | VUint64 u -> u <> Uint64.zero
              | _ -> runtime_error "OR expects bool, float, or int64"
            in
            Stack.push (VBool (bool_val a || bool_val b)) stack;
            next ()
        | NOT ->
            push_trace frame.pc "NOT";
            let v = pop1 stack in
            let bool_val =
              match v with
              | VFloat f -> f <> 0.0
              | VInt64 i -> i <> 0L
              | VInt32 i -> i <> 0l
              | VUint32 u -> u <> Uint32.zero
              | VUint64 u -> u <> Uint64.zero
              | _ -> runtime_error "NOT expects float or int64"
            in
            Stack.push (VFloat (if not bool_val then 1.0 else 0.0)) stack;
            next ()
        | INC ->
            push_trace frame.pc "INC";
            let v = pop1 stack in
            let result =
              match v with
              | VFloat f -> VFloat (f +. 1.0)
              | VInt64 i -> VInt64 Int64.(add i 1L)
              | VInt32 i -> VInt32 Int32.(add i 1l)
              | VUint32 u -> VUint32 Uint32.(add u Uint32.one)
              | VUint64 u -> VUint64 Uint64.(add u Uint64.one)
              | _ -> runtime_error "INC expects float or int64"
            in
            Stack.push result stack;
            next ()
        | DEC ->
            push_trace frame.pc "DEC";
            let v = pop1 stack in
            let result =
              match v with
              | VFloat f -> VFloat (f -. 1.0)
              | VInt64 i -> VInt64 Int64.(sub i 1L)
              | VInt32 i -> VInt32 Int32.(sub i 1l)
              | VUint32 u -> VUint32 Uint32.(sub u Uint32.one)
              | VUint64 u -> VUint64 Uint64.(sub u Uint64.one)
              | _ -> runtime_error "DEC expects float or int64"
            in
            Stack.push result stack;
            next ()
        | DUP ->
            push_trace frame.pc "DUP";
            Stack.top_opt stack |> Option.iter (fun v -> Stack.push v stack);
            if Stack.is_empty stack then
              runtime_error "Stack underflow during DUP";
            next ()
        | POP ->
            push_trace frame.pc "POP";
            if Stack.pop_opt stack = None then
              runtime_error "POP attempted on empty stack"
            else next ()
        | LOAD_ARRAY length ->
            push_trace frame.pc ("LOAD_ARRAY " ^ string_of_int length);
            if Stack.length stack < length then
              runtime_error
                ("LOAD_ARRAY expects " ^ string_of_int length
               ^ " elements on stack")
            else
              let items = pop_n_rev [] length in
              Stack.push (VArray items) stack;
              next ()
        | LOAD_INDEX ->
            push_trace frame.pc "LOAD_INDEX";
            if Stack.length stack < 2 then
              runtime_error
                "LOAD_INDEX requires two values on the stack (collection, \
                 index)";
            let index =
              match Stack.pop stack with
              | VInt64 i -> Int64.to_int i
              | _ -> runtime_error "Expected int for index in LOAD_INDEX"
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
              if pc >= Array.length frame.code then
                runtime_error "Unterminated FUNCTION block"
              else
                match frame.code.(pc) with
                | RETURN -> pc + 1
                | _ -> skip_function (pc + 1)
            in
            frame.pc <- skip_function (frame.pc + 1)
        | CALL function_name ->
            push_trace frame.pc ("CALL " ^ function_name);
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
            let code = Array.of_list body in

            frame.pc <- frame.pc + 1;
            push_frame code local_env
        | TAIL_CALL function_name ->
            push_trace frame.pc ("TAIL_CALL " ^ function_name);
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
            let code = Array.of_list body in
            ignore (Stack.pop frame_stack);
            Stack.push { code; pc = 0; env = local_env } frame_stack
        | RETURN ->
            push_trace frame.pc "RETURN";
            let return_value =
              match Stack.pop_opt stack with Some v -> v | None -> VFloat nan
            in
            ignore (Stack.pop frame_stack);
            Stack.push return_value stack
        | STORE_VAR name ->
            push_trace frame.pc ("STORE_VAR " ^ name);
            if Stack.is_empty stack then
              runtime_error ("STORE_VAR '" ^ name ^ "' failed: stack is empty")
            else
              let value = Stack.pop stack in
              if List.mem_assoc name frame.env then (
                frame.env <-
                  (name, (value, true)) :: List.remove_assoc name frame.env;
                next ())
              else (
                global_env :=
                  (name, (value, true)) :: List.remove_assoc name !global_env;
                next ())
        | PRINTLN ->
            push_trace frame.pc "PRINTLN";
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
                | VInt32 1l -> "true"
                | VInt32 0l -> "false"
                | VUint32 1l -> "true"
                | VUint32 0l -> "false"
                | VUint64 1L -> "true"
                | VUint64 0L -> "false"
                | VFloat f when Float.is_infinite f ->
                    if f > 0.0 then "inf" else "-inf"
                | VFloat f -> Printf.sprintf "%.12g" f
                | VInt64 i -> Printf.sprintf "%Ld" i
                | VInt32 i -> Printf.sprintf "%ld" i
                | VUint32 u -> Uint32.to_string u
                | VUint64 u -> Uint64.to_string u
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
              Buffer.add_string output_buffer (string_of_value value ^ "\n");
              next ()
        | INPUT -> (
            push_trace frame.pc "INPUT";
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
                      Gc.alloc_string_with_gc stack frame.env input_value
                  in
                  Stack.push processed_value stack;
                  next ()
              | None -> runtime_error "Invalid prompt ID for INPUT")
        | NEG ->
            push_trace frame.pc "NEG";
            let v = Stack.pop stack in
            (match v with
            | VFloat f -> Stack.push (VFloat (-.f)) stack
            | VInt64 i -> Stack.push (VInt64 (Int64.neg i)) stack
            | _ -> runtime_error "NEG expects a float or int");
            next ()
        | FLOAT ->
            push_trace frame.pc "FLOAT";
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
                  | None ->
                      runtime_error "FLOAT: invalid heap reference for string")
              | _ ->
                  runtime_error "FLOAT: unsupported type for float conversion"
            in
            Stack.push (VFloat float_val) stack;
            next ()
        | INT ->
            push_trace frame.pc "INT";
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
                  | None ->
                      runtime_error "INT: invalid heap reference for string")
              | _ -> runtime_error "INT: unsupported type for int conversion"
            in
            Stack.push (VInt64 int_val) stack;
            next ()
        | STRING ->
            push_trace frame.pc "STRING";
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
              | _ ->
                  runtime_error "STRING: unsupported type for string conversion"
            in
            let new_str_ref = Gc.alloc_string_with_gc stack frame.env str_val in
            Stack.push new_str_ref stack;
            next ()
        | PLUSASSIGN ->
            push_trace frame.pc "PLUSASSIGN";
            let value = pop1 stack in
            let var = pop1 stack in
            let name =
              match var with
              | VHeapRef id -> (
                  match Gc.get_string id with
                  | Some name -> name
                  | None ->
                      runtime_error
                        "PLUSASSIGN: invalid variable name reference")
              | _ -> runtime_error "PLUSASSIGN: expected variable name"
            in
            let old_value = get_var frame.env name in
            let result =
              match (old_value, value) with
              | VFloat oldf, VFloat newf -> VFloat (oldf +. newf)
              | VInt64 oldi, VInt64 newi -> VInt64 (Int64.add oldi newi)
              | VInt32 oldi, VInt32 newi -> VInt32 (Int32.add oldi newi)
              | VUint32 oldu, VUint32 newu -> VUint32 (Uint32.add oldu newu)
              | VUint64 oldu, VUint64 newu -> VUint64 (Uint64.add oldu newu)
              | _ -> runtime_error "PLUSASSIGN: type mismatch"
            in
            frame.env <- update_variable name result frame.env;
            Stack.push result stack;
            next ()
        | MINUSASSIGN ->
            push_trace frame.pc "MINUSASSIGN";
            let value = pop1 stack in
            let var = pop1 stack in
            let name =
              match var with
              | VHeapRef id -> (
                  match Gc.get_string id with
                  | Some name -> name
                  | None ->
                      runtime_error
                        "MINUSASSIGN: invalid variable name reference")
              | _ -> runtime_error "MINUSASSIGN: expected variable name"
            in
            let old_value = get_var frame.env name in
            let result =
              match (old_value, value) with
              | VFloat oldf, VFloat newf -> VFloat (oldf -. newf)
              | VInt64 oldi, VInt64 newi -> VInt64 (Int64.sub oldi newi)
              | VInt32 oldi, VInt32 newi -> VInt32 (Int32.sub oldi newi)
              | VUint32 oldu, VUint32 newu -> VUint32 (Uint32.sub oldu newu)
              | VUint64 oldu, VUint64 newu -> VUint64 (Uint64.sub oldu newu)
              | _ -> runtime_error "MINUSASSIGN: type mismatch"
            in
            frame.env <- update_variable name result frame.env;
            Stack.push result stack;
            next ()
        | STARASSIGN ->
            push_trace frame.pc "STARASSIGN";
            let value = pop1 stack in
            let var = pop1 stack in
            let name =
              match var with
              | VHeapRef id -> (
                  match Gc.get_string id with
                  | Some name -> name
                  | None ->
                      runtime_error
                        "STARASSIGN: invalid variable name reference")
              | _ -> runtime_error "STARASSIGN: expected variable name"
            in
            let old_value = get_var frame.env name in
            let result =
              match (old_value, value) with
              | VFloat oldf, VFloat newf -> VFloat (oldf *. newf)
              | VInt64 oldi, VInt64 newi -> VInt64 (Int64.mul oldi newi)
              | VInt32 oldi, VInt32 newi -> VInt32 (Int32.mul oldi newi)
              | VUint32 oldu, VUint32 newu -> VUint32 (Uint32.mul oldu newu)
              | VUint64 oldu, VUint64 newu -> VUint64 (Uint64.mul oldu newu)
              | _ -> runtime_error "STARASSIGN: type mismatch"
            in
            frame.env <- update_variable name result frame.env;
            Stack.push result stack;
            next ()
        | SLASHASSIGN ->
            push_trace frame.pc "SLASHASSIGN";
            let value = pop1 stack in
            let var = pop1 stack in
            let name =
              match var with
              | VHeapRef id -> (
                  match Gc.get_string id with
                  | Some name -> name
                  | None ->
                      runtime_error
                        "SLASHASSIGN: invalid variable name reference")
              | _ -> runtime_error "SLASHASSIGN: expected variable name"
            in
            let old_value = get_var frame.env name in
            let result =
              match (old_value, value) with
              | VFloat oldf, VFloat newf ->
                  if newf = 0.0 then
                    runtime_error "SLASHASSIGN: division by zero";
                  VFloat (oldf /. newf)
              | VInt64 oldi, VInt64 newi ->
                  if newi = 0L then
                    runtime_error "SLASHASSIGN: division by zero";
                  VInt64 (Int64.div oldi newi)
              | VInt32 oldi, VInt32 newi ->
                  if newi = 0l then
                    runtime_error "SLASHASSIGN: division by zero";
                  VInt32 (Int32.div oldi newi)
              | VUint32 oldu, VUint32 newu -> VUint32 (Uint32.div oldu newu)
              | VUint64 oldu, VUint64 newu -> VUint64 (Uint64.div oldu newu)
              | _ -> runtime_error "SLASHASSIGN: type mismatch"
            in
            frame.env <- update_variable name result frame.env;
            Stack.push result stack;
            next ()
        | LOAD_VAR_REF name ->
            push_trace frame.pc ("LOAD_VAR_REF " ^ name);
            let v = Gc.alloc_string_with_gc stack frame.env name in
            Stack.push v stack;
            next ()
        | LENGTH ->
            push_trace frame.pc "LENGTH";
            if Stack.is_empty stack then
              runtime_error "LENGTH expects a value on the stack"
            else
              let v = Stack.pop stack in
              let length =
                match v with
                | VHeapRef id -> (
                    match Gc.get_string id with
                    | Some s -> Int64.of_int (String.length s)
                    | None ->
                        runtime_error
                          "LENGTH: invalid heap reference for string")
                | VArray items -> Int64.of_int (List.length items)
                | VTuple items -> Int64.of_int (List.length items)
                | _ -> runtime_error "LENGTH expects a string, array, or tuple"
              in
              Stack.push (VInt64 length) stack;
              next ()
        | BITWISENOT ->
            push_trace frame.pc "BITWISE_NOT";
            let v = pop1 stack in
            (match v with
            | VInt i -> Stack.push (VInt (Int.lognot i)) stack
            | VInt64 i -> Stack.push (VInt64 (Int64.lognot i)) stack
            | VInt32 i -> Stack.push (VInt32 (Int32.lognot i)) stack
            | VUint32 u -> Stack.push (VUint32 (Uint32.not u)) stack
            | VUint64 u -> Stack.push (VUint64 (Uint64.not u)) stack
            | _ -> runtime_error "BITWISE_NOT expects an int64");
            next ()
        | BITWISEAND ->
            push_trace frame.pc "BITWISE_AND";
            let a, b = pop2 stack in
            (match (a, b) with
            | VInt a, VInt b -> Stack.push (VInt (Int.logand a b)) stack
            | VInt64 a, VInt64 b -> Stack.push (VInt64 (Int64.logand a b)) stack
            | VInt32 a, VInt32 b -> Stack.push (VInt32 (Int32.logand a b)) stack
            | VUint32 a, VUint32 b ->
                Stack.push (VUint32 (Uint32.and_ a b)) stack
            | VUint64 a, VUint64 b ->
                Stack.push (VUint64 (Uint64.and_ a b)) stack
            | _ -> runtime_error "BITWISE_AND expects int64 operands");
            next ()
        | BITWISEOR ->
            push_trace frame.pc "BITWISE_OR";
            let a, b = pop2 stack in
            (match (a, b) with
            | VInt a, VInt b -> Stack.push (VInt (Int.logor a b)) stack
            | VInt64 a, VInt64 b -> Stack.push (VInt64 (Int64.logor a b)) stack
            | VInt32 a, VInt32 b -> Stack.push (VInt32 (Int32.logor a b)) stack
            | VUint32 a, VUint32 b ->
                Stack.push (VUint32 (Uint32.or_ a b)) stack
            | VUint64 a, VUint64 b ->
                Stack.push (VUint64 (Uint64.or_ a b)) stack
            | _ -> runtime_error "BITWISE_OR expects int64 operands");
            next ()
        | BITWISEXOR ->
            push_trace frame.pc "BITWISE_XOR";
            let a, b = pop2 stack in
            (match (a, b) with
            | VInt a, VInt b -> Stack.push (VInt (Int.logxor a b)) stack
            | VInt64 a, VInt64 b -> Stack.push (VInt64 (Int64.logxor a b)) stack
            | VInt32 a, VInt32 b -> Stack.push (VInt32 (Int32.logxor a b)) stack
            | VUint32 a, VUint32 b ->
                Stack.push (VUint32 (Uint32.xor a b)) stack
            | VUint64 a, VUint64 b ->
                Stack.push (VUint64 (Uint64.xor a b)) stack
            | _ -> runtime_error "BITWISE_XOR expects int64 operands");
            next ()
        | LEFTSHIFT ->
            push_trace frame.pc "LEFT_SHIFT";
            let a, b = pop2 stack in
            (match (a, b) with
            | VInt a, VInt b -> Stack.push (VInt (Int.shift_left a b)) stack
            | VInt64 a, VInt64 b ->
                Stack.push (VInt64 (Int64.shift_left a (Int64.to_int b))) stack
            | VInt32 a, VInt32 b ->
                Stack.push (VInt32 (Int32.shift_left a (Int32.to_int b))) stack
            | VUint32 a, VUint32 b ->
                Stack.push (VUint32 (Uint32.shl a b)) stack
            | VUint64 a, VUint64 b ->
                Stack.push (VUint64 (Uint64.shl a b)) stack
            | _ -> runtime_error "LEFT_SHIFT expects int64 operands");
            next ()
        | RIGHTSHIFT ->
            push_trace frame.pc "RIGHT_SHIFT";
            let a, b = pop2 stack in
            (match (a, b) with
            | VInt a, VInt b -> Stack.push (VInt (Int.shift_right a b)) stack
            | VInt64 a, VInt64 b ->
                Stack.push (VInt64 (Int64.shift_right a (Int64.to_int b))) stack
            | VInt32 a, VInt32 b ->
                Stack.push (VInt32 (Int32.shift_right a (Int32.to_int b))) stack
            | VUint32 a, VUint32 b ->
                Stack.push (VUint32 (Uint32.shr a b)) stack
            | VUint64 a, VUint64 b ->
                Stack.push (VUint64 (Uint64.shr a b)) stack
            | _ -> runtime_error "RIGHT_SHIFT expects int64 operands");
            next ()
        | RIGHTSHIFTLOGICAL ->
            push_trace frame.pc "RIGHT_SHIFT_LOGICAL";
            let a, b = pop2 stack in
            (match (a, b) with
            | VInt a, VInt b ->
                Stack.push (VInt (Int.shift_right_logical a b)) stack
            | VInt64 a, VInt64 b ->
                let shifted = Int64.shift_right_logical a (Int64.to_int b) in
                Stack.push (VInt64 shifted) stack
            | VInt32 a, VInt32 b ->
                Stack.push
                  (VInt32 (Int32.shift_right_logical a (Int32.to_int b)))
                  stack
            | _ -> runtime_error "RIGHT_SHIFT_LOGICAL expects int64 operands");
            next ()
        | BITWISEANDASSIGN ->
            push_trace frame.pc "BITWISE_ANDASSIGN";
            let value = pop1 stack in
            let var = pop1 stack in
            let name =
              match var with
              | VHeapRef id -> (
                  match Gc.get_string id with
                  | Some name -> name
                  | None -> runtime_error "BITWISE_ANDASSIGN: invalid var ref")
              | _ -> runtime_error "BITWISE_ANDASSIGN: expected var name"
            in
            let old_value = get_var frame.env name in
            let result =
              match (old_value, value) with
              | VInt oldi, VInt newi -> VInt (Int.logand oldi newi)
              | VInt64 oldi, VInt64 newi -> VInt64 (Int64.logand oldi newi)
              | VInt32 oldi, VInt32 newi -> VInt32 (Int32.logand oldi newi)
              | VUint32 oldu, VUint32 newu -> VUint32 (Uint32.and_ oldu newu)
              | VUint64 oldu, VUint64 newu -> VUint64 (Uint64.and_ oldu newu)
              | _ -> runtime_error "BITWISE_ANDASSIGN: type mismatch"
            in
            frame.env <- update_variable name result frame.env;
            Stack.push result stack;
            next ()
        | BITWISEORASSIGN ->
            push_trace frame.pc "BITWISE_ORASSIGN";
            let value = pop1 stack in
            let var = pop1 stack in
            let name =
              match var with
              | VHeapRef id -> (
                  match Gc.get_string id with
                  | Some name -> name
                  | None -> runtime_error "BITWISE_ORASSIGN: invalid var ref")
              | _ -> runtime_error "BITWISE_ORASSIGN: expected var name"
            in
            let old_value = get_var frame.env name in
            let result =
              match (old_value, value) with
              | VInt oldi, VInt newi -> VInt (Int.logor oldi newi)
              | VInt64 oldi, VInt64 newi -> VInt64 (Int64.logor oldi newi)
              | VInt32 oldi, VInt32 newi -> VInt32 (Int32.logor oldi newi)
              | VUint32 oldu, VUint32 newu -> VUint32 (Uint32.or_ oldu newu)
              | VUint64 oldu, VUint64 newu -> VUint64 (Uint64.or_ oldu newu)
              | _ -> runtime_error "BITWISE_ORASSIGN: type mismatch"
            in
            frame.env <- update_variable name result frame.env;
            Stack.push result stack;
            next ()
        | BITWISEXORASSIGN ->
            push_trace frame.pc "BITWISE_XORASSIGN";
            let value = pop1 stack in
            let var = pop1 stack in
            let name =
              match var with
              | VHeapRef id -> (
                  match Gc.get_string id with
                  | Some name -> name
                  | None -> runtime_error "BITWISE_XORASSIGN: invalid var ref")
              | _ -> runtime_error "BITWISE_XORASSIGN: expected var name"
            in
            let old_value = get_var frame.env name in
            let result =
              match (old_value, value) with
              | VInt oldi, VInt newi -> VInt (Int.logxor oldi newi)
              | VInt64 oldi, VInt64 newi -> VInt64 (Int64.logxor oldi newi)
              | VInt32 oldi, VInt32 newi -> VInt32 (Int32.logxor oldi newi)
              | VUint32 oldu, VUint32 newu -> VUint32 (Uint32.xor oldu newu)
              | VUint64 oldu, VUint64 newu -> VUint64 (Uint64.xor oldu newu)
              | _ -> runtime_error "BITWISE_XORASSIGN: type mismatch"
            in
            frame.env <- update_variable name result frame.env;
            Stack.push result stack;
            next ()
        | LEFTSHIFTASSIGN ->
            push_trace frame.pc "LEFT_SHIFTASSIGN";
            let value = pop1 stack in
            let var = pop1 stack in
            let name =
              match var with
              | VHeapRef id -> (
                  match Gc.get_string id with
                  | Some name -> name
                  | None -> runtime_error "LEFT_SHIFTASSIGN: invalid var ref")
              | _ -> runtime_error "LEFT_SHIFTASSIGN: expected var name"
            in
            let old_value = get_var frame.env name in
            let result =
              match (old_value, value) with
              | VInt oldi, VInt newi -> VInt (Int.shift_left oldi newi)
              | VInt64 oldi, VInt64 newi ->
                  VInt64 (Int64.shift_left oldi (Int64.to_int newi))
              | VInt32 oldi, VInt32 newi ->
                  VInt32 (Int32.shift_left oldi (Int32.to_int newi))
              | VUint32 oldu, VUint32 newu -> VUint32 (Uint32.shl oldu newu)
              | VUint64 oldu, VUint64 newu -> VUint64 (Uint64.shl oldu newu)
              | _ -> runtime_error "LEFT_SHIFTASSIGN: type mismatch"
            in
            frame.env <- update_variable name result frame.env;
            Stack.push result stack;
            next ()
        | RIGHTSHIFTASSIGN ->
            push_trace frame.pc "RIGHT_SHIFTASSIGN";
            let value = pop1 stack in
            let var = pop1 stack in
            let name =
              match var with
              | VHeapRef id -> (
                  match Gc.get_string id with
                  | Some name -> name
                  | None -> runtime_error "RIGHT_SHIFTASSIGN: invalid var ref")
              | _ -> runtime_error "RIGHT_SHIFTASSIGN: expected var name"
            in
            let old_value = get_var frame.env name in
            let result =
              match (old_value, value) with
              | VInt oldi, VInt newi -> VInt (Int.shift_right oldi newi)
              | VInt64 oldi, VInt64 newi ->
                  VInt64 (Int64.shift_right oldi (Int64.to_int newi))
              | VInt32 oldi, VInt32 newi ->
                  VInt32 (Int32.shift_right oldi (Int32.to_int newi))
              | VUint32 oldu, VUint32 newu -> VUint32 (Uint32.shr oldu newu)
              | VUint64 oldu, VUint64 newu -> VUint64 (Uint64.shr oldu newu)
              | _ -> runtime_error "RIGHT_SHIFTASSIGN: type mismatch"
            in
            frame.env <- update_variable name result frame.env;
            Stack.push result stack;
            next ()
        | ASSERT -> (
            push_trace frame.pc "ASSERT";
            let cond = pop1 stack in
            match cond with
            | VBool true -> next ()
            | VBool false -> runtime_error "Assertion failed"
            | _ -> runtime_error "ASSERT expects a boolean value")
    done;
    print_string (Buffer.contents output_buffer);
    flush stdout
  with RuntimeError msg ->
    print_trace msg;
    exit 1
