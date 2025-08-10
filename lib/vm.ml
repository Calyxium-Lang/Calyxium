open Init
open Opcode
open Gc

exception RuntimeError of string

type trace_entry = { instr : string }

type frame = {
  mutable code : opcode array;
  mutable pc : int;
  mutable env : (string * (value * bool)) list;
}

let output_buffer : string list ref = ref []
let stdlib_modules : (string, value) Hashtbl.t = Hashtbl.create 16
let trace : trace_entry list ref = ref []
let push_trace instr = trace := { instr } :: !trace
let clear_trace () = trace := []

let flush_buffer () =
  List.iter print_string (List.rev !output_buffer);
  output_buffer := [];
  flush stdout

let max_buffer_size = 1000

let safe_push_output s =
  output_buffer := s :: !output_buffer;
  if List.length !output_buffer >= 10 then flush_buffer ();
  if List.length !output_buffer >= max_buffer_size then (
    flush_buffer ();
    Printf.eprintf
      "Warning: output buffer flushed early to prevent overflow.\n%!")

let init_stdlib () =
  List.iter
    (fun (name, entries) ->
      let tbl = Hashtbl.create (List.length entries) in
      List.iter (fun (k, v) -> Hashtbl.add tbl k v) entries;
      Hashtbl.add stdlib_modules name (VModule tbl))
    stdlib_definitions

let print_trace msg =
  match !trace with
  | { instr; _ } :: _ -> prerr_endline ("Error at '" ^ instr ^ "': " ^ msg)
  | [] -> prerr_endline ("Error: " ^ msg)

let runtime_error msg = raise (RuntimeError msg)
let stack = Stack.create ()
let global_env = ref []

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
  if n = 0 then acc
  else
    match Stack.pop_opt stack with
    | Some v -> pop_n_rev (v :: acc) (n - 1)
    | None -> runtime_error "Stack underflow in pop_n_rev"

let pop2_safe stack =
  match (Stack.pop_opt stack, Stack.pop_opt stack) with
  | Some b, Some a -> (a, b)
  | _ -> runtime_error "Stack underflow"

let pop1 stack =
  match Stack.pop_opt stack with
  | Some v -> v
  | None -> runtime_error "Stack underflow"

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

let rec equal_value a b =
  match (a, b) with
  | VHeapRef id1, VHeapRef id2 -> (
      match (Gc.get_string id1, Gc.get_string id2) with
      | Some sa, Some sb -> sa = sb
      | _ -> id1 = id2)
  | VFloat f1, VFloat f2 -> f1 = f2
  | VInt i1, VInt i2 -> i1 = i2
  | VBool b1, VBool b2 -> b1 = b2
  | VByte c1, VByte c2 -> c1 = c2
  | VUnit, VUnit -> true
  | VTuple l1, VTuple l2 ->
      List.length l1 = List.length l2 && List.for_all2 equal_value l1 l2
  | VArray l1, VArray l2 ->
      List.length l1 = List.length l2 && List.for_all2 equal_value l1 l2
  | _ -> false

let rec not_equal_value a b =
  match (a, b) with
  | VHeapRef id1, VHeapRef id2 -> (
      match (Gc.get_string id1, Gc.get_string id2) with
      | Some sa, Some sb -> sa <> sb
      | _ -> id1 = id2)
  | VFloat f1, VFloat f2 -> f1 <> f2
  | VInt i1, VInt i2 -> i1 <> i2
  | VBool b1, VBool b2 -> b1 <> b2
  | VByte c1, VByte c2 -> c1 <> c2
  | VUnit, VUnit -> true
  | VTuple l1, VTuple l2 ->
      List.length l1 = List.length l2 && List.for_all2 not_equal_value l1 l2
  | _ -> false

let string_to_bytes s = Array.init (String.length s) (String.get s)

let safe_shift_left a b =
  if Bigint.sign b < 0 || Bigint.gt b (Bigint.of_int 63) then
    runtime_error "Invalid shift amount"
  else Bigint.shift_left a (Bigint.to_int b)

let rec force v = match v with VThunk f -> force (f ()) | v -> v

let reset_vm_state () =
  Stack.clear stack;
  global_env := [];
  clear_trace ();
  output_buffer := []

let run instructions =
  clear_trace ();
  let frame_stack = Stack.create () in
  let push_frame code env = Stack.push { code; pc = 0; env } frame_stack in
  push_frame (Array.of_list instructions) [];
  try
    while not (Stack.is_empty frame_stack) do
      let frame = Stack.top frame_stack in
      if frame.pc >= Array.length frame.code then (
        ignore (pop1 frame_stack);
        ())
      else
        let instr = frame.code.(frame.pc) in
        let next () = frame.pc <- frame.pc + 1 in
        match instr with
        | LOAD_INT v ->
            push_trace ("LOAD_INT " ^ Bigint.to_string v);
            Stack.push (VInt v) stack;
            next ()
        | LOAD_FLOAT v ->
            push_trace ("LOAD_FLOAT " ^ string_of_float v);
            Stack.push (VFloat v) stack;
            next ()
        | LOAD_STRING s ->
            push_trace ("LOAD_STRING " ^ s);
            let v = Gc.alloc_string_with_gc stack frame.env s in
            Stack.push v stack;
            next ()
        | LOAD_BYTE c ->
            push_trace ("LOAD_BYTE " ^ String.make 1 c);
            Stack.push (VByte c) stack;
            next ()
        | LOAD_BOOL b ->
            push_trace ("LOAD_BOOL " ^ string_of_bool b);
            Stack.push (VBool b) stack;
            next ()
        | LOAD_UNIT _ ->
            push_trace "LOAD_UNIT";
            Stack.push VUnit stack;
            next ()
        | LOAD_TUPLE n ->
            push_trace ("LOAD_TUPLE " ^ string_of_int n);
            if Stack.length stack < n then
              runtime_error
                ("LOAD_TUPLE expects " ^ string_of_int n
               ^ " values on the stack")
            else
              let items = pop_n_rev [] n in
              Stack.push
                (VThunk (fun () -> VTuple (List.map force items)))
                stack;
              next ()
        | LOAD_VAR name ->
            push_trace ("LOAD_VAR " ^ name);
            let v = get_var frame.env name in
            Stack.push (force v) stack;
            next ()
        | PLUS ->
            push_trace "PLUS";
            let a, b = pop2_safe stack in
            Stack.push
              (VThunk
                 (fun () ->
                   match (force a, force b) with
                   | VFloat a, VFloat b ->
                       let res = a +. b in
                       if
                         classify_float res = FP_nan
                         || classify_float res = FP_infinite
                       then runtime_error "Float overflow in addition"
                       else VFloat res
                   | VInt a, VInt b -> VInt (Bigint.add a b)
                   | _ -> runtime_error "PLUS expects numbers"))
              stack;
            next ()
        | MINUS ->
            push_trace "MINUS";
            let a, b = pop2_safe stack in
            Stack.push
              (VThunk
                 (fun () ->
                   match (force a, force b) with
                   | VFloat a, VFloat b ->
                       let res = a -. b in
                       if
                         classify_float res = FP_nan
                         || classify_float res = FP_infinite
                       then runtime_error "Float overflow in subtraction"
                       else VFloat res
                   | VInt a, VInt b -> VInt (Bigint.sub a b)
                   | _ -> runtime_error "MINUS expects numbers"))
              stack;
            next ()
        | STAR ->
            push_trace "STAR";
            let a, b = pop2_safe stack in
            Stack.push
              (VThunk
                 (fun () ->
                   match (force a, force b) with
                   | VFloat a, VFloat b ->
                       let res = a *. b in
                       if
                         classify_float res = FP_nan
                         || classify_float res = FP_infinite
                       then runtime_error "Float overflow in multiplication"
                       else VFloat res
                   | VInt a, VInt b -> VInt (Bigint.mul a b)
                   | _ -> runtime_error "STAR expects numbers"))
              stack;
            next ()
        | SLASH ->
            push_trace "SLASH";
            let a, b = pop2_safe stack in
            Stack.push
              (VThunk
                 (fun () ->
                   match (force a, force b) with
                   | VFloat _, VFloat 0.0 -> runtime_error "Division by zero"
                   | VInt _, VInt z when Bigint.equal z Bigint.zero ->
                       runtime_error "Division by zero"
                   | VFloat a, VFloat b ->
                       let res = a /. b in
                       if
                         classify_float res = FP_nan
                         || classify_float res = FP_infinite
                       then runtime_error "Float overflow in division"
                       else VFloat res
                   | VInt a, VInt b -> VInt (Bigint.div a b)
                   | _ -> runtime_error "SLASH expects numbers"))
              stack;
            next ()
        | MOD ->
            push_trace "MOD";
            let a, b = pop2_safe stack in
            Stack.push
              (VThunk
                 (fun () ->
                   match (force a, force b) with
                   | VFloat _, VFloat 0.0 -> runtime_error "Modulo by zero"
                   | VInt _, VInt z when Bigint.equal z Bigint.zero ->
                       runtime_error "Modulo by zero"
                   | VFloat a, VFloat b -> VFloat (mod_float a b)
                   | VInt a, VInt b -> VInt (Bigint.rem a b)
                   | _ -> runtime_error "MOD expects numbers"))
              stack;
            next ()
        | POW ->
            push_trace "POW";
            let a, b = pop2_safe stack in
            Stack.push
              (VThunk
                 (fun () ->
                   match (force a, force b) with
                   | VFloat a, VFloat b -> VFloat (a ** b)
                   | VInt a, VInt b ->
                       if Bigint.sign b < 0 then
                         runtime_error "POW expects non-negative exponent"
                       else VInt (Bigint.pow a (Bigint.to_int b))
                   | _ -> runtime_error "POW expects numbers"))
              stack;
            next ()
        | CONCAT ->
            push_trace "CONCAT";
            let a, b = pop2_safe stack in
            Stack.push
              (VThunk
                 (fun () ->
                   let result =
                     get_string_from_stack_value (force a)
                     ^ get_string_from_stack_value (force b)
                   in
                   Gc.alloc_string_with_gc stack frame.env result))
              stack;
            next ()
        | JUMP_IF_FALSE offset -> (
            push_trace ("JUMP_IF_FALSE " ^ string_of_int offset);
            match Stack.pop_opt stack with
            | Some (VFloat 0.0) -> frame.pc <- frame.pc + offset
            | Some (VInt z) when Bigint.equal z Bigint.zero ->
                frame.pc <- frame.pc + offset
            | Some (VBool false) -> frame.pc <- frame.pc + offset
            | Some _ -> next ()
            | None -> runtime_error "JUMP_IF_FALSE with empty stack")
        | JUMP n ->
            push_trace ("JUMP " ^ string_of_int n);
            frame.pc <- frame.pc + n
        | LESS ->
            push_trace "LESS";
            let a, b = pop2_safe stack in
            let a, b = (force a, force b) in
            let result =
              match (a, b) with
              | VFloat a, VFloat b -> VBool (a < b)
              | VInt a, VInt b -> VBool (Bigint.compare a b < 0)
              | _ -> runtime_error "LESS expects two floats or int"
            in
            Stack.push result stack;
            next ()
        | GREATER ->
            push_trace "GREATER";
            let a, b = pop2_safe stack in
            let a, b = (force a, force b) in
            let result =
              match (a, b) with
              | VFloat a, VFloat b -> VBool (a > b)
              | VInt a, VInt b -> VBool (Bigint.compare a b > 0)
              | _ -> runtime_error "GREATER expects two floats or int"
            in
            Stack.push result stack;
            next ()
        | LESS_EQUAL ->
            push_trace "LESS_EQUAL";
            let a, b = pop2_safe stack in
            let a, b = (force a, force b) in
            let result =
              match (a, b) with
              | VFloat a, VFloat b -> VBool (a <= b)
              | VInt a, VInt b -> VBool (Bigint.compare a b <= 0)
              | _ -> runtime_error "LESS_EQUAL expects two floats or int"
            in
            Stack.push result stack;
            next ()
        | GREATER_EQUAL ->
            push_trace "GREATER_EQUAL";
            let a, b = pop2_safe stack in
            let a, b = (force a, force b) in
            let result =
              match (a, b) with
              | VFloat a, VFloat b -> VBool (a >= b)
              | VInt a, VInt b -> VBool (Bigint.compare a b >= 0)
              | _ -> runtime_error "GREATER_EQUAL expects two floats or int"
            in
            Stack.push result stack;
            next ()
        | NOT_EQUAL ->
            push_trace "NOT_EQUAL";
            let a, b = pop2_safe stack in
            let a, b = (force a, force b) in
            let result = not_equal_value a b in
            Stack.push (VBool result) stack;
            next ()
        | EQUAL ->
            push_trace "EQUAL";
            let b, a = pop2_safe stack in
            let a, b = (force a, force b) in
            let result = equal_value a b in
            Stack.push (VBool result) stack;
            next ()
        | AND ->
            push_trace "AND";
            let a, b = pop2_safe stack in
            let a, b = (force a, force b) in
            let bool_val = function
              | VBool b -> b
              | VFloat f -> f <> 0.0
              | VInt i -> i <> Bigint.zero
              | _ -> runtime_error "AND expects bool, float, or int"
            in
            Stack.push (VBool (bool_val a && bool_val b)) stack;
            next ()
        | OR ->
            push_trace "OR";
            let a, b = pop2_safe stack in
            let a, b = (force a, force b) in
            let bool_val = function
              | VBool b -> b
              | VFloat f -> f <> 0.0
              | VInt i -> i <> Bigint.zero
              | _ -> runtime_error "OR expects bool, float, or int"
            in
            Stack.push (VBool (bool_val a || bool_val b)) stack;
            next ()
        | NOT ->
            push_trace "NOT";
            let v = force (pop1 stack) in
            let bool_val =
              match v with
              | VBool b -> b
              | _ -> runtime_error "NOT expects float or int"
            in
            Stack.push (VBool (not bool_val)) stack;
            next ()
        | INC ->
            push_trace "INC";
            let v = force (pop1 stack) in
            let result =
              match v with
              | VFloat f ->
                  let res = f +. 1.0 in
                  if
                    classify_float res = FP_nan
                    || classify_float res = FP_infinite
                  then runtime_error "Float overflow in increment"
                  else VFloat res
              | VInt i -> VInt (Bigint.add i Bigint.one)
              | _ -> runtime_error "INC expects float or int"
            in
            Stack.push result stack;
            next ()
        | DEC ->
            push_trace "DEC";
            let v = force (pop1 stack) in
            let result =
              match v with
              | VFloat f ->
                  let res = f -. 1.0 in
                  if
                    classify_float res = FP_nan
                    || classify_float res = FP_infinite
                  then runtime_error "Float overflow in decrement"
                  else VFloat res
              | VInt i -> VInt (Bigint.sub i Bigint.one)
              | _ -> runtime_error "DEC expects float or int"
            in
            Stack.push result stack;
            next ()
        | DUP ->
            push_trace "DUP";
            Stack.top_opt stack |> Option.iter (fun v -> Stack.push v stack);
            if Stack.is_empty stack then
              runtime_error "Stack underflow during DUP";
            next ()
        | POP ->
            push_trace "POP";
            if Stack.pop_opt stack = None then
              runtime_error "POP attempted on empty stack"
            else next ()
        | LOAD_ARRAY length ->
            push_trace ("LOAD_ARRAY " ^ string_of_int length);
            if Stack.length stack < length then
              runtime_error
                ("LOAD_ARRAY expects " ^ string_of_int length
               ^ " elements on stack")
            else
              let items = pop_n_rev [] length in
              Stack.push
                (VThunk (fun () -> VArray (List.map force items)))
                stack;
              next ()
        | LOAD_INDEX ->
            push_trace "LOAD_INDEX";
            if Stack.length stack < 2 then
              runtime_error
                "LOAD_INDEX requires two values on the stack (collection, \
                 index)";
            let index_val = pop1 stack in
            let collection_val = pop1 stack in
            let index =
              match force index_val with
              | VInt i -> Bigint.to_int i
              | _ -> runtime_error "Expected int for index in LOAD_INDEX"
            in
            let collection = force collection_val in
            let item =
              match collection with
              | VArray items ->
                  if index < 0 || index >= List.length items then
                    runtime_error
                      ("Index out of bounds in LOAD_INDEX: "
                     ^ string_of_int index)
                  else List.nth items index
              | VTuple items ->
                  if index < 0 || index >= List.length items then
                    runtime_error
                      ("Index out of bounds in LOAD_INDEX: "
                     ^ string_of_int index)
                  else List.nth items index
              | VHeapRef id -> (
                  match Gc.find_heap_obj id with
                  | Gc.HString s ->
                      if index < 0 || index >= String.length s then
                        runtime_error
                          ("Index out of bounds in LOAD_INDEX (string): "
                         ^ string_of_int index)
                      else VByte s.[index]
                  | Gc.HBytes arr ->
                      if index < 0 || index >= Array.length arr then
                        runtime_error
                          ("Index out of bounds in LOAD_INDEX (bytes): "
                         ^ string_of_int index)
                      else VByte arr.(index)
                  | _ ->
                      runtime_error
                        "Expected array, tuple, string, bytes, or range for \
                         LOAD_INDEX")
              | VRange id -> (
                  match Gc.find_heap_obj id with
                  | Gc.HRange { current; step; end_ } -> (
                      let elem =
                        Bigint.add current
                          (Bigint.mul step (Bigint.of_int index))
                      in
                      match end_ with
                      | Some e
                        when (step >= Bigint.zero && elem > e)
                             || (step < Bigint.zero && elem < e) ->
                          runtime_error
                            "Index out of bounds in LOAD_INDEX for range"
                      | _ -> VInt elem)
                  | _ -> runtime_error "Expected range for VRange")
              | _ ->
                  runtime_error
                    "Expected array, tuple, string, bytes, or range for \
                     LOAD_INDEX"
            in
            Stack.push (VThunk (fun () -> item)) stack;
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
            push_trace ("CALL " ^ function_name);
            let function_body = resolve_function_body function_name in
            let param_names =
              List.map
                (fun (p : Ast.Stmt.parameter) -> p.name)
                function_body.params
            in
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
            let body = List.drop skip_header function_body.bytecode in
            let code = Array.of_list body in

            frame.pc <- frame.pc + 1;
            push_frame code local_env
        | TAIL_CALL function_name ->
            push_trace ("TAIL_CALL " ^ function_name);
            let function_body = resolve_function_body function_name in
            let param_names =
              List.map
                (fun (p : Ast.Stmt.parameter) -> p.name)
                function_body.params
            in
            let arg_count = List.length param_names in

            if Stack.length stack < arg_count then
              runtime_error
                ("TAIL_CALL to '" ^ function_name ^ "' requires "
               ^ string_of_int arg_count ^ " arguments, but stack has only "
                ^ string_of_int (Stack.length stack));

            let raw_args = pop_n [] arg_count in
            let local_env =
              List.combine param_names (List.map (fun v -> (v, true)) raw_args)
            in

            let skip_header = 1 + arg_count in
            let body = List.drop skip_header function_body.bytecode in
            let code = Array.of_list body in
            let frame = Stack.top frame_stack in
            frame.code <- code;
            frame.pc <- 0;
            frame.env <- local_env
        | RETURN ->
            push_trace "RETURN";
            let return_value =
              match Stack.pop_opt stack with
              | Some v -> force v
              | None -> VFloat nan
            in
            ignore (pop1 frame_stack);
            Stack.push return_value stack
        | STORE_VAR name ->
            push_trace ("STORE_VAR " ^ name);
            if Stack.is_empty stack then
              runtime_error ("STORE_VAR '" ^ name ^ "' failed: stack is empty")
            else
              let value = pop1 stack in
              let lazy_value = VThunk (fun () -> force value) in
              if List.mem_assoc name frame.env then (
                frame.env <-
                  (name, (lazy_value, true)) :: List.remove_assoc name frame.env;
                next ())
              else (
                global_env :=
                  (name, (lazy_value, true))
                  :: List.remove_assoc name !global_env;
                next ())
        | PRINT ->
            push_trace "PRINT";
            if Stack.is_empty stack then
              runtime_error "PRINTLN attempted with empty stack"
            else
              let rec string_of_value = function
                | VUnit -> "unit"
                | VByte c -> Printf.sprintf "'%c'" c
                | VBool true -> "true"
                | VBool false -> "false"
                | VFloat f ->
                    if Float.is_nan f then "NaN"
                    else if Float.is_infinite f then
                      if f > 0.0 then "inf" else "-inf"
                    else Printf.sprintf "%g" f
                | VInt i -> Bigint.to_string i
                | VHeapRef id -> (
                    match Gc.get_string id with
                    | Some s -> s
                    | None -> (
                        match Gc.get_bytes id with
                        | Some arr ->
                            String.init (Array.length arr) (Array.get arr)
                        | None -> (
                            match Gc.get_value id with
                            | Some v -> string_of_value v
                            | None -> "<invalid ref>")))
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
                | VModule _ -> "<module>"
                | VNative _ -> "<native function>"
                | VClosure name -> "<function " ^ name ^ ">"
                | VThunk _ -> "<thunk>"
                | VRange _ -> "<range>"
              in
              let value = force (pop1 stack) in
              safe_push_output (string_of_value value);
              next ()
        | INPUT -> (
            push_trace "INPUT";
            if Stack.is_empty stack then
              runtime_error "Stack underflow during INPUT"
            else
              let value = pop1 stack in
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
                      if String.length input_value = 1 then
                        VByte input_value.[0]
                      else Gc.alloc_string_with_gc stack frame.env input_value
                  in
                  Stack.push processed_value stack;
                  next ()
              | None -> runtime_error "Invalid prompt ID for INPUT")
        | NEG ->
            push_trace "NEG";
            let v = force (pop1 stack) in
            (match v with
            | VFloat f ->
                let res = -.f in
                if
                  classify_float res = FP_nan
                  || classify_float res = FP_infinite
                then runtime_error "Float overflow in negation"
                else Stack.push (VFloat res) stack
            | VInt i -> Stack.push (VInt (Bigint.neg i)) stack
            | _ -> runtime_error "NEG expects a float or int");
            next ()
        | FLOAT ->
            push_trace "FLOAT";
            let v = force (pop1 stack) in
            let float_val =
              match v with
              | VFloat f -> f
              | VInt i -> Bigint.to_float i
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
            push_trace "TO_INT";
            let v = force (pop1 stack) in
            let int_val =
              match v with
              | VInt i -> i
              | VFloat f -> Bigint.of_float f
              | VByte c -> Bigint.of_int (Char.code c)
              | VHeapRef id -> (
                  match Gc.get_string id with
                  | Some s -> (
                      try Bigint.of_string s
                      with Failure _ ->
                        runtime_error ("INT: invalid int string: " ^ s))
                  | None ->
                      runtime_error "INT: invalid heap reference for string")
              | _ -> runtime_error "INT: unsupported type for int conversion"
            in
            Stack.push (VInt int_val) stack;
            next ()
        | STRING ->
            push_trace "TO_STRING";
            let v = force (pop1 stack) in
            let str_val =
              match v with
              | VHeapRef id -> (
                  match Gc.get_string id with
                  | Some s -> s
                  | None -> (
                      match Gc.get_bytes id with
                      | Some arr ->
                          String.init (Array.length arr) (Array.get arr)
                      | None ->
                          runtime_error
                            "STRING: invalid heap reference for string or byte \
                             array"))
              | VArray vs ->
                  if List.for_all (function VByte _ -> true | _ -> false) vs
                  then
                    String.init (List.length vs) (fun i ->
                        match List.nth vs i with VByte c -> c | _ -> '\000')
                  else if
                    List.for_all (function VFloat _ -> true | _ -> false) vs
                  then
                    String.concat ""
                      (List.map
                         (function VFloat f -> string_of_float f | _ -> "")
                         vs)
                  else if
                    List.for_all (function VInt _ -> true | _ -> false) vs
                  then
                    String.concat ""
                      (List.map
                         (function VInt i -> Bigint.to_string i | _ -> "")
                         vs)
                  else
                    runtime_error
                      "STRING: array is not []byte, []float or []int"
              | VInt i -> Bigint.to_string i
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
        | BYTES ->
            push_trace "TO_BYTES";
            let v = pop1 stack in
            let str_val =
              match v with
              | VHeapRef id -> (
                  match Gc.get_string id with
                  | Some s -> s
                  | None ->
                      runtime_error
                        "TO_BYTES: invalid heap reference for string")
              | VInt i -> Bigint.to_string i
              | VFloat f -> string_of_float f
              | VBool b -> if b then "true" else "false"
              | VByte c -> String.make 1 c
              | VUnit -> "()"
              | _ -> runtime_error "TO_BYTES: unsupported type for conversion"
            in
            let byte_array = string_to_bytes str_val in
            let new_bytes_ref =
              Gc.alloc_bytes_with_gc stack frame.env byte_array
            in
            Stack.push new_bytes_ref stack;
            next ()
        | BYTE -> (
            push_trace "TO_BYTE";
            let v = force (pop1 stack) in
            match v with
            | VInt i ->
                Stack.push
                  (VByte
                     (Char.chr
                        (Bigint.to_int
                           (Bigint.logand i (Bigint.of_int64 0xFFL)))))
                  stack;
                next ()
            | VByte c ->
                Stack.push (VByte c) stack;
                next ()
            | VHeapRef id -> (
                match Gc.get_string id with
                | Some s ->
                    if String.length s = 1 then Stack.push (VByte s.[0]) stack
                    else runtime_error "BYTE: heap string length must be 1"
                | None ->
                    runtime_error "BYTE: invalid heap reference for string")
            | VFloat f ->
                Stack.push (VByte (Char.chr (int_of_float f land 0xFF))) stack;
                next ()
            | VArray vs ->
                if List.for_all (function VInt _ -> true | _ -> false) vs then (
                  let byte_arr =
                    Array.of_list
                      (List.map
                         (function
                           | VInt i ->
                               Char.chr
                                 (Bigint.to_int
                                    (Bigint.logand i (Bigint.of_int64 0xFFL)))
                           | _ -> assert false)
                         vs)
                  in
                  let vbyte_arr = Array.map (fun c -> VByte c) byte_arr in
                  Stack.push (VArray (Array.to_list vbyte_arr)) stack;
                  next ())
                else runtime_error "BYTE: array contains non-integer elements"
            | _ -> runtime_error "BYTE: unsupported type for byte conversion")
        | PLUSASSIGN ->
            push_trace "PLUSASSIGN";
            let value = force (pop1 stack) in
            let var = force (pop1 stack) in
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
            let old_value = force (get_var frame.env name) in
            let result =
              match (old_value, value) with
              | VFloat oldf, VFloat newf -> VFloat (oldf +. newf)
              | VInt oldi, VInt newi -> VInt (Bigint.add oldi newi)
              | _ -> runtime_error "PLUSASSIGN: type mismatch"
            in
            frame.env <- update_variable name result frame.env;
            Stack.push result stack;
            next ()
        | MINUSASSIGN ->
            push_trace "MINUSASSIGN";
            let value = force (pop1 stack) in
            let var = force (pop1 stack) in
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
            let old_value = force (get_var frame.env name) in
            let result =
              match (old_value, value) with
              | VFloat oldf, VFloat newf -> VFloat (oldf -. newf)
              | VInt oldi, VInt newi -> VInt (Bigint.sub oldi newi)
              | _ -> runtime_error "MINUSASSIGN: type mismatch"
            in
            frame.env <- update_variable name result frame.env;
            next ()
        | STARASSIGN ->
            push_trace "STARASSIGN";
            let value = force (pop1 stack) in
            let var = force (pop1 stack) in
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
            let old_value = force (get_var frame.env name) in
            let result =
              match (old_value, value) with
              | VFloat oldf, VFloat newf -> VFloat (oldf *. newf)
              | VInt oldi, VInt newi -> VInt (Bigint.mul oldi newi)
              | _ -> runtime_error "STARASSIGN: type mismatch"
            in
            frame.env <- update_variable name result frame.env;
            next ()
        | SLASHASSIGN ->
            push_trace "SLASHASSIGN";
            let value = force (pop1 stack) in
            let var = force (pop1 stack) in
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
            let old_value = force (get_var frame.env name) in
            let result =
              match (old_value, value) with
              | VFloat oldf, VFloat newf ->
                  if newf = 0.0 then
                    runtime_error "SLASHASSIGN: division by zero";
                  VFloat (oldf /. newf)
              | VInt oldi, VInt newi ->
                  if newi = Bigint.zero then
                    runtime_error "SLASHASSIGN: division by zero";
                  VInt (Bigint.div oldi newi)
              | _ -> runtime_error "SLASHASSIGN: type mismatch"
            in
            frame.env <- update_variable name result frame.env;
            next ()
        | LOAD_VAR_REF name ->
            push_trace ("LOAD_VAR_REF " ^ name);
            let v = Gc.alloc_string_with_gc stack frame.env name in
            Stack.push v stack;
            next ()
        | LENGTH ->
            push_trace "LENGTH";
            if Stack.is_empty stack then
              runtime_error "LENGTH expects a value on the stack"
            else
              let v = pop1 stack in
              let length =
                match v with
                | VHeapRef id -> (
                    match Gc.find_heap_obj id with
                    | Gc.HString s -> Bigint.of_int (String.length s)
                    | Gc.HBytes bytes_arr ->
                        Bigint.of_int (Array.length bytes_arr)
                    | _ ->
                        runtime_error
                          "LENGTH: heap reference is not a string or bytes")
                | VArray items -> Bigint.of_int (List.length items)
                | VTuple items -> Bigint.of_int (List.length items)
                | _ ->
                    runtime_error
                      "LENGTH expects a string, bytes, array, or tuple"
              in
              Stack.push (VInt length) stack;
              next ()
        | BITWISENOT ->
            push_trace "BITWISE_NOT";
            let v = force (pop1 stack) in
            (match v with
            | VInt i -> Stack.push (VInt (Bigint.lognot i)) stack
            | _ -> runtime_error "BITWISE_NOT expects an int");
            next ()
        | BITWISEAND ->
            push_trace "BITWISE_AND";
            let a, b = pop2_safe stack in
            let a, b = (force a, force b) in
            (match (a, b) with
            | VInt a, VInt b -> Stack.push (VInt (Bigint.logand a b)) stack
            | _ -> runtime_error "BITWISE_AND expects int operands");
            next ()
        | BITWISEOR ->
            push_trace "BITWISE_OR";
            let a, b = pop2_safe stack in
            let a, b = (force a, force b) in
            (match (a, b) with
            | VInt a, VInt b -> Stack.push (VInt (Bigint.logor a b)) stack
            | _ -> runtime_error "BITWISE_OR expects int operands");
            next ()
        | BITWISEXOR ->
            push_trace "BITWISE_XOR";
            let a, b = pop2_safe stack in
            let a, b = (force a, force b) in
            (match (a, b) with
            | VInt a, VInt b -> Stack.push (VInt (Bigint.logxor a b)) stack
            | _ -> runtime_error "BITWISE_XOR expects int operands");
            next ()
        | LEFTSHIFT ->
            push_trace "LEFTSHIFT";
            let a, b = pop2_safe stack in
            let a, b = (force a, force b) in
            (match (a, b) with
            | VInt a, VInt b -> Stack.push (VInt (safe_shift_left a b)) stack
            | _ -> runtime_error "LEFTSHIFT expects int operands");
            next ()
        | RIGHTSHIFT ->
            push_trace "RIGHT_SHIFT";
            let a, b = pop2_safe stack in
            let a, b = (force a, force b) in
            (match (a, b) with
            | VInt a, VInt b ->
                Stack.push (VInt (Bigint.shift_right a (Bigint.to_int b))) stack
            | _ -> runtime_error "RIGHT_SHIFT expects int operands");
            next ()
        | BITWISEANDASSIGN ->
            push_trace "BITWISE_ANDASSIGN";
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
              | VInt oldi, VInt newi -> VInt (Bigint.logand oldi newi)
              | _ -> runtime_error "BITWISE_ANDASSIGN: type mismatch"
            in
            frame.env <- update_variable name result frame.env;
            next ()
        | BITWISEORASSIGN ->
            push_trace "BITWISE_ORASSIGN";
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
              | VInt oldi, VInt newi -> VInt (Bigint.logor oldi newi)
              | _ -> runtime_error "BITWISE_ORASSIGN: type mismatch"
            in
            frame.env <- update_variable name result frame.env;
            next ()
        | BITWISEXORASSIGN ->
            push_trace "BITWISE_XORASSIGN";
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
              | VInt oldi, VInt newi -> VInt (Bigint.logxor oldi newi)
              | _ -> runtime_error "BITWISE_XORASSIGN: type mismatch"
            in
            frame.env <- update_variable name result frame.env;
            next ()
        | LEFTSHIFTASSIGN ->
            push_trace "LEFT_SHIFTASSIGN";
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
              | VInt oldi, VInt newi ->
                  VInt (Bigint.shift_left oldi (Bigint.to_int newi))
              | _ -> runtime_error "LEFT_SHIFTASSIGN: type mismatch"
            in
            frame.env <- update_variable name result frame.env;
            next ()
        | RIGHTSHIFTASSIGN ->
            push_trace "RIGHT_SHIFTASSIGN";
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
              | VInt oldi, VInt newi ->
                  VInt (Bigint.shift_right oldi (Bigint.to_int newi))
              | _ -> runtime_error "RIGHT_SHIFTASSIGN: type mismatch"
            in
            frame.env <- update_variable name result frame.env;
            next ()
        | ASSERT -> (
            push_trace "ASSERT";
            let cond = force (pop1 stack) in
            match cond with
            | VBool true -> next ()
            | VBool false -> runtime_error "Assertion failed"
            | _ -> runtime_error "ASSERT expects a boolean value")
        | PANIC ->
            push_trace "PANIC";
            if Stack.is_empty stack then
              runtime_error "PANIC attempted with empty stack"
            else
              let string_of_value = function
                | VHeapRef id -> (
                    match Gc.get_string id with
                    | Some s -> s
                    | None -> (
                        match Gc.get_bytes id with
                        | Some arr ->
                            String.init (Array.length arr) (Array.get arr)
                        | None ->
                            runtime_error
                              "PANIC: invalid heap reference for string"))
                | _ -> runtime_error "PANIC expects a string message"
              in
              let value = pop1 stack in
              Printf.eprintf "%s" (string_of_value value);
              exit 1
        | LOAD_MODULE name ->
            let m =
              match Hashtbl.find_opt stdlib_modules name with
              | Some modval -> modval
              | None -> failwith ("Unknown module: " ^ name)
            in
            Stack.push m stack;
            next ()
        | LOAD_FIELD name -> (
            let module_or_obj = pop1 stack in
            match module_or_obj with
            | VModule table -> (
                match Hashtbl.find_opt table name with
                | Some v ->
                    Stack.push v stack;
                    next ()
                | None -> failwith ("Unknown field " ^ name))
            | _ -> failwith "LOAD_FIELD expects a module or object")
        | CLOSURE func_name ->
            Stack.push (VClosure func_name) stack;
            next ()
        | MAKE_RANGE ->
            let v = Stack.pop stack in
            let start =
              match v with
              | VInt x -> x
              | _ -> runtime_error "MAKE_RANGE expects int"
            in
            let range_val = Gc.alloc_range start Bigint.one None in
            let id =
              match range_val with
              | VHeapRef id -> id
              | _ -> runtime_error "alloc_range must return VHeapRef"
            in
            Stack.push (VRange id) stack;
            next ()
        | ARRAYCONCAT -> (
            push_trace "ARRAYCONCAT";
            if Stack.length stack < 2 then
              runtime_error "ARRAYCONCAT requires two arrays on the stack"
            else
              let right = force (pop1 stack) in
              let left = force (pop1 stack) in
              match (left, right) with
              | VArray l1, VArray l2 ->
                  Stack.push (VArray (l1 @ l2)) stack;
                  next ()
              | _ -> runtime_error "ARRAYCONCAT expects two arrays")
        | SLICE -> (
            let v_end = force (pop1 stack) in
            let v_start = force (pop1 stack) in
            let v_arr = force (pop1 stack) in
            match v_arr with
            | VArray elements ->
                let len = List.length elements in
                let s =
                  match v_start with
                  | VInt s -> max 0 (Bigint.to_int s)
                  | VUnit -> 0
                  | _ -> runtime_error "SLICE: start index must be int or unit"
                in
                let e =
                  match v_end with
                  | VInt e -> min len (Bigint.to_int e)
                  | VUnit -> len
                  | _ -> runtime_error "SLICE: end index must be int or unit"
                in
                let slice =
                  if s > e then []
                  else
                    elements |> List.to_seq |> Seq.drop s
                    |> Seq.take (e - s)
                    |> List.of_seq
                in
                Stack.push (VArray slice) stack;
                next ()
            | _ -> runtime_error "SLICE expects array as first argument")
    done;
    flush_buffer ()
  with RuntimeError msg ->
    print_trace msg;
    exit 1
