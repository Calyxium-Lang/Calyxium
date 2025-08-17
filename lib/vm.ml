exception RuntimeError of string

type trace_entry = { instr : string }

type frame = {
  mutable code : Opcode.opcode array;
  mutable pc : int;
  mutable env : (string * (Gc.value * bool)) list;
}

let output_buffer = ref []
let stdlib_modules = Hashtbl.create 16
let trace = ref []
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
      Hashtbl.add stdlib_modules name (Gc.VModule tbl))
    Init.stdlib_definitions

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
  | Gc.VHeapRef id -> (
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
  | Opcode.FUNCTION _ :: rest ->
      let rec collect acc = function
        | Opcode.STORE_VAR name :: tl -> collect (name :: acc) tl
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
  | Gc.VHeapRef id1, Gc.VHeapRef id2 -> (
      match (Gc.get_string id1, Gc.get_string id2) with
      | Some sa, Some sb -> sa = sb
      | _ -> id1 = id2)
  | Gc.VFloat f1, Gc.VFloat f2 -> f1 = f2
  | Gc.VInt i1, Gc.VInt i2 -> i1 = i2
  | Gc.VBool b1, Gc.VBool b2 -> b1 = b2
  | Gc.VByte c1, Gc.VByte c2 -> c1 = c2
  | Gc.VUnit, Gc.VUnit -> true
  | Gc.VTuple l1, Gc.VTuple l2 ->
      List.length l1 = List.length l2 && List.for_all2 equal_value l1 l2
  | Gc.VArray l1, Gc.VArray l2 ->
      List.length l1 = List.length l2 && List.for_all2 equal_value l1 l2
  | _ -> false

let rec not_equal_value a b =
  match (a, b) with
  | Gc.VHeapRef id1, Gc.VHeapRef id2 -> (
      match (Gc.get_string id1, Gc.get_string id2) with
      | Some sa, Some sb -> sa <> sb
      | _ -> id1 = id2)
  | Gc.VFloat f1, Gc.VFloat f2 -> f1 <> f2
  | Gc.VInt i1, Gc.VInt i2 -> i1 <> i2
  | Gc.VBool b1, Gc.VBool b2 -> b1 <> b2
  | Gc.VByte c1, Gc.VByte c2 -> c1 <> c2
  | Gc.VUnit, Gc.VUnit -> true
  | Gc.VTuple l1, Gc.VTuple l2 ->
      List.length l1 = List.length l2 && List.for_all2 not_equal_value l1 l2
  | _ -> false

let string_to_bytes s = Array.init (String.length s) (String.get s)

let safe_shift_left a b =
  if Bigint.sign b < 0 || Bigint.gt b (Bigint.of_int 63) then
    runtime_error "Invalid shift amount"
  else Bigint.shift_left a (Bigint.to_int b)

let rec force v = match v with Gc.VThunk f -> force (f ()) | v -> v

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
        | Opcode.LOAD_INT v ->
            push_trace ("LOAD_INT " ^ Bigint.to_string v);
            Stack.push (Gc.VInt v) stack;
            next ()
        | Opcode.LOAD_FLOAT v ->
            push_trace ("LOAD_FLOAT " ^ string_of_float v);
            Stack.push (Gc.VFloat v) stack;
            next ()
        | Opcode.LOAD_STRING s ->
            push_trace ("LOAD_STRING " ^ s);
            let v = Gc.alloc_string_with_gc stack frame.env s in
            Stack.push v stack;
            next ()
        | Opcode.LOAD_BYTE c ->
            push_trace ("LOAD_BYTE " ^ String.make 1 c);
            Stack.push (Gc.VByte c) stack;
            next ()
        | Opcode.LOAD_BOOL b ->
            push_trace ("LOAD_BOOL " ^ string_of_bool b);
            Stack.push (Gc.VBool b) stack;
            next ()
        | Opcode.LOAD_UNIT _ ->
            push_trace "LOAD_UNIT";
            Stack.push Gc.VUnit stack;
            next ()
        | Opcode.LOAD_TUPLE n ->
            push_trace ("LOAD_TUPLE " ^ string_of_int n);
            if Stack.length stack < n then
              runtime_error
                ("LOAD_TUPLE expects " ^ string_of_int n
               ^ " values on the stack")
            else
              let items = pop_n_rev [] n in
              Stack.push
                (Gc.VThunk (fun () -> Gc.VTuple (List.map force items)))
                stack;
              next ()
        | Opcode.LOAD_VAR name ->
            push_trace ("LOAD_VAR " ^ name);
            let v = get_var frame.env name in
            Stack.push (force v) stack;
            next ()
        | Opcode.PLUS ->
            push_trace "PLUS";
            let a, b = pop2_safe stack in
            Stack.push
              (Gc.VThunk
                 (fun () ->
                   match (force a, force b) with
                   | Gc.VFloat a, Gc.VFloat b ->
                       let res = a +. b in
                       if
                         classify_float res = FP_nan
                         || classify_float res = FP_infinite
                       then runtime_error "Float overflow in addition"
                       else Gc.VFloat res
                   | Gc.VInt a, Gc.VInt b -> Gc.VInt (Bigint.add a b)
                   | _ -> runtime_error "PLUS expects numbers"))
              stack;
            next ()
        | Opcode.MINUS ->
            push_trace "MINUS";
            let a, b = pop2_safe stack in
            Stack.push
              (Gc.VThunk
                 (fun () ->
                   match (force a, force b) with
                   | Gc.VFloat a, Gc.VFloat b ->
                       let res = a -. b in
                       if
                         classify_float res = FP_nan
                         || classify_float res = FP_infinite
                       then runtime_error "Float overflow in subtraction"
                       else Gc.VFloat res
                   | Gc.VInt a, Gc.VInt b -> Gc.VInt (Bigint.sub a b)
                   | _ -> runtime_error "MINUS expects numbers"))
              stack;
            next ()
        | Opcode.STAR ->
            push_trace "STAR";
            let a, b = pop2_safe stack in
            Stack.push
              (Gc.VThunk
                 (fun () ->
                   match (force a, force b) with
                   | Gc.VFloat a, Gc.VFloat b ->
                       let res = a *. b in
                       if
                         classify_float res = FP_nan
                         || classify_float res = FP_infinite
                       then runtime_error "Float overflow in multiplication"
                       else Gc.VFloat res
                   | Gc.VInt a, Gc.VInt b -> Gc.VInt (Bigint.mul a b)
                   | _ -> runtime_error "STAR expects numbers"))
              stack;
            next ()
        | Opcode.SLASH ->
            push_trace "SLASH";
            let a, b = pop2_safe stack in
            Stack.push
              (Gc.VThunk
                 (fun () ->
                   match (force a, force b) with
                   | Gc.VFloat _, Gc.VFloat 0.0 ->
                       runtime_error "Division by zero"
                   | Gc.VInt _, Gc.VInt z when Bigint.equal z Bigint.zero ->
                       runtime_error "Division by zero"
                   | Gc.VFloat a, Gc.VFloat b ->
                       let res = a /. b in
                       if
                         classify_float res = FP_nan
                         || classify_float res = FP_infinite
                       then runtime_error "Float overflow in division"
                       else Gc.VFloat res
                   | Gc.VInt a, Gc.VInt b -> Gc.VInt (Bigint.div a b)
                   | _ -> runtime_error "SLASH expects numbers"))
              stack;
            next ()
        | Opcode.MOD ->
            push_trace "MOD";
            let a, b = pop2_safe stack in
            Stack.push
              (Gc.VThunk
                 (fun () ->
                   match (force a, force b) with
                   | Gc.VFloat _, Gc.VFloat 0.0 ->
                       runtime_error "Modulo by zero"
                   | Gc.VInt _, Gc.VInt z when Bigint.equal z Bigint.zero ->
                       runtime_error "Modulo by zero"
                   | Gc.VFloat a, Gc.VFloat b -> Gc.VFloat (mod_float a b)
                   | Gc.VInt a, Gc.VInt b -> Gc.VInt (Bigint.rem a b)
                   | _ -> runtime_error "MOD expects numbers"))
              stack;
            next ()
        | Opcode.POW ->
            push_trace "POW";
            let a, b = pop2_safe stack in
            Stack.push
              (Gc.VThunk
                 (fun () ->
                   match (force a, force b) with
                   | Gc.VFloat a, Gc.VFloat b -> Gc.VFloat (a ** b)
                   | Gc.VInt a, Gc.VInt b ->
                       if Bigint.sign b < 0 then
                         runtime_error "POW expects non-negative exponent"
                       else Gc.VInt (Bigint.pow a (Bigint.to_int b))
                   | _ -> runtime_error "POW expects numbers"))
              stack;
            next ()
        | Opcode.CONCAT ->
            push_trace "CONCAT";
            let a, b = pop2_safe stack in
            Stack.push
              (Gc.VThunk
                 (fun () ->
                   let result =
                     get_string_from_stack_value (force a)
                     ^ get_string_from_stack_value (force b)
                   in
                   Gc.alloc_string_with_gc stack frame.env result))
              stack;
            next ()
        | Opcode.JUMP_IF_FALSE offset -> (
            push_trace ("JUMP_IF_FALSE " ^ string_of_int offset);
            match Stack.pop_opt stack with
            | Some (Gc.VFloat 0.0) -> frame.pc <- frame.pc + offset
            | Some (Gc.VInt z) when Bigint.equal z Bigint.zero ->
                frame.pc <- frame.pc + offset
            | Some (Gc.VBool false) -> frame.pc <- frame.pc + offset
            | Some _ -> next ()
            | None -> runtime_error "JUMP_IF_FALSE with empty stack")
        | Opcode.JUMP n ->
            push_trace ("JUMP " ^ string_of_int n);
            frame.pc <- frame.pc + n
        | Opcode.LESS ->
            push_trace "LESS";
            let a, b = pop2_safe stack in
            let a, b = (force a, force b) in
            let result =
              match (a, b) with
              | Gc.VFloat a, Gc.VFloat b -> Gc.VBool (a < b)
              | Gc.VInt a, Gc.VInt b -> Gc.VBool (Bigint.compare a b < 0)
              | _ -> runtime_error "LESS expects two floats or int"
            in
            Stack.push result stack;
            next ()
        | Opcode.GREATER ->
            push_trace "GREATER";
            let a, b = pop2_safe stack in
            let a, b = (force a, force b) in
            let result =
              match (a, b) with
              | Gc.VFloat a, Gc.VFloat b -> Gc.VBool (a > b)
              | Gc.VInt a, Gc.VInt b -> Gc.VBool (Bigint.compare a b > 0)
              | _ -> runtime_error "GREATER expects two floats or int"
            in
            Stack.push result stack;
            next ()
        | Opcode.LESS_EQUAL ->
            push_trace "LESS_EQUAL";
            let a, b = pop2_safe stack in
            let a, b = (force a, force b) in
            let result =
              match (a, b) with
              | Gc.VFloat a, Gc.VFloat b -> Gc.VBool (a <= b)
              | Gc.VInt a, Gc.VInt b -> Gc.VBool (Bigint.compare a b <= 0)
              | _ -> runtime_error "LESS_EQUAL expects two floats or int"
            in
            Stack.push result stack;
            next ()
        | Opcode.GREATER_EQUAL ->
            push_trace "GREATER_EQUAL";
            let a, b = pop2_safe stack in
            let a, b = (force a, force b) in
            let result =
              match (a, b) with
              | Gc.VFloat a, Gc.VFloat b -> Gc.VBool (a >= b)
              | Gc.VInt a, Gc.VInt b -> Gc.VBool (Bigint.compare a b >= 0)
              | _ -> runtime_error "GREATER_EQUAL expects two floats or int"
            in
            Stack.push result stack;
            next ()
        | Opcode.NOT_EQUAL ->
            push_trace "NOT_EQUAL";
            let a, b = pop2_safe stack in
            let a, b = (force a, force b) in
            let result = not_equal_value a b in
            Stack.push (Gc.VBool result) stack;
            next ()
        | Opcode.EQUAL ->
            push_trace "EQUAL";
            let b, a = pop2_safe stack in
            let a, b = (force a, force b) in
            let result = equal_value a b in
            Stack.push (Gc.VBool result) stack;
            next ()
        | Opcode.AND ->
            push_trace "AND";
            let a, b = pop2_safe stack in
            let a, b = (force a, force b) in
            let bool_val = function
              | Gc.VBool b -> b
              | Gc.VFloat f -> f <> 0.0
              | Gc.VInt i -> i <> Bigint.zero
              | _ -> runtime_error "AND expects bool, float, or int"
            in
            Stack.push (Gc.VBool (bool_val a && bool_val b)) stack;
            next ()
        | Opcode.OR ->
            push_trace "OR";
            let a, b = pop2_safe stack in
            let a, b = (force a, force b) in
            let bool_val = function
              | Gc.VBool b -> b
              | Gc.VFloat f -> f <> 0.0
              | Gc.VInt i -> i <> Bigint.zero
              | _ -> runtime_error "OR expects bool, float, or int"
            in
            Stack.push (Gc.VBool (bool_val a || bool_val b)) stack;
            next ()
        | Opcode.NOT ->
            push_trace "NOT";
            let v = force (pop1 stack) in
            let bool_val =
              match v with
              | Gc.VBool b -> b
              | _ -> runtime_error "NOT expects float or int"
            in
            Stack.push (Gc.VBool (not bool_val)) stack;
            next ()
        | Opcode.INC ->
            push_trace "INC";
            let v = force (pop1 stack) in
            let result =
              match v with
              | Gc.VFloat f ->
                  let res = f +. 1.0 in
                  if
                    classify_float res = FP_nan
                    || classify_float res = FP_infinite
                  then runtime_error "Float overflow in increment"
                  else Gc.VFloat res
              | Gc.VInt i -> Gc.VInt (Bigint.add i Bigint.one)
              | _ -> runtime_error "INC expects float or int"
            in
            Stack.push result stack;
            next ()
        | Opcode.DEC ->
            push_trace "DEC";
            let v = force (pop1 stack) in
            let result =
              match v with
              | Gc.VFloat f ->
                  let res = f -. 1.0 in
                  if
                    classify_float res = FP_nan
                    || classify_float res = FP_infinite
                  then runtime_error "Float overflow in decrement"
                  else Gc.VFloat res
              | Gc.VInt i -> Gc.VInt (Bigint.sub i Bigint.one)
              | _ -> runtime_error "DEC expects float or int"
            in
            Stack.push result stack;
            next ()
        | Opcode.DUP ->
            push_trace "DUP";
            Stack.top_opt stack |> Option.iter (fun v -> Stack.push v stack);
            if Stack.is_empty stack then
              runtime_error "Stack underflow during DUP";
            next ()
        | Opcode.POP ->
            push_trace "POP";
            if Stack.pop_opt stack = None then
              runtime_error "POP attempted on empty stack"
            else next ()
        | Opcode.LOAD_ARRAY length ->
            push_trace ("LOAD_ARRAY " ^ string_of_int length);
            if Stack.length stack < length then
              runtime_error
                ("LOAD_ARRAY expects " ^ string_of_int length
               ^ " elements on stack")
            else
              let items = pop_n_rev [] length in
              Stack.push
                (Gc.VThunk (fun () -> Gc.VArray (List.map force items)))
                stack;
              next ()
        | Opcode.LOAD_INDEX ->
            push_trace "LOAD_INDEX";
            if Stack.length stack < 2 then
              runtime_error
                "LOAD_INDEX requires two values on the stack (collection, \
                 index)";
            let index_val = pop1 stack in
            let collection_val = pop1 stack in
            let index =
              match force index_val with
              | Gc.VInt i -> Bigint.to_int i
              | _ -> runtime_error "Expected int for index in LOAD_INDEX"
            in
            let collection = force collection_val in
            let item =
              match collection with
              | Gc.VArray items ->
                  if index < 0 || index >= List.length items then
                    runtime_error
                      ("Index out of bounds in LOAD_INDEX: "
                     ^ string_of_int index)
                  else List.nth items index
              | Gc.VTuple items ->
                  if index < 0 || index >= List.length items then
                    runtime_error
                      ("Index out of bounds in LOAD_INDEX: "
                     ^ string_of_int index)
                  else List.nth items index
              | Gc.VHeapRef id -> (
                  match Gc.find_heap_obj id with
                  | Gc.HString s ->
                      if index < 0 || index >= String.length s then
                        runtime_error
                          ("Index out of bounds in LOAD_INDEX (string): "
                         ^ string_of_int index)
                      else Gc.VByte s.[index]
                  | Gc.HBytes arr ->
                      if index < 0 || index >= Array.length arr then
                        runtime_error
                          ("Index out of bounds in LOAD_INDEX (bytes): "
                         ^ string_of_int index)
                      else Gc.VByte arr.(index)
                  | _ ->
                      runtime_error
                        "Expected array, tuple, string, bytes, or range for \
                         LOAD_INDEX")
              | Gc.VRange id -> (
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
                      | _ -> Gc.VInt elem)
                  | _ -> runtime_error "Expected range for Gc.VRange")
              | _ ->
                  runtime_error
                    "Expected array, tuple, string, bytes, or range for \
                     LOAD_INDEX"
            in
            Stack.push (Gc.VThunk (fun () -> item)) stack;
            next ()
        | Opcode.FUNCTION _ ->
            let rec skip_function pc =
              if pc >= Array.length frame.code then
                runtime_error "Unterminated FUNCTION block"
              else
                match frame.code.(pc) with
                | Opcode.RETURN -> pc + 1
                | _ -> skip_function (pc + 1)
            in
            frame.pc <- skip_function (frame.pc + 1)
        | Opcode.CALL function_name ->
            push_trace ("CALL " ^ function_name);
            let function_body = resolve_function_body function_name in
            let param_names =
              List.map
                (fun (p : Ast.Stmt.parameter) -> p.Ast.Stmt.name)
                function_body.Bytecode.params
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
            let body = List.drop skip_header function_body.Bytecode.bytecode in
            let code = Array.of_list body in

            frame.pc <- frame.pc + 1;
            push_frame code local_env
        | Opcode.TAIL_CALL function_name ->
            push_trace ("TAIL_CALL " ^ function_name);

            let fn = resolve_function_body function_name in
            let arity = List.length fn.Bytecode.params in

            if Stack.length stack < arity then
              runtime_error
                ("TAIL_CALL to '" ^ function_name ^ "' requires "
               ^ string_of_int arity ^ " arguments, but stack has only "
                ^ string_of_int (Stack.length stack));

            let args_rev = pop_n [] arity in
            let args = List.rev args_rev in

            let frame = Stack.top frame_stack in

            let new_env =
              List.map2
                (fun (p : Ast.Stmt.parameter) v -> (p.Ast.Stmt.name, (v, true)))
                fn.Bytecode.params args
            in

            let roots = Gc.get_stack_roots stack in
            Gc.maybe_collect_gc roots new_env;

            frame.env <- new_env;
            frame.code <- fn.Bytecode.body_code;
            frame.pc <- 0
        | Opcode.RETURN ->
            push_trace "RETURN";
            let return_value =
              match Stack.pop_opt stack with
              | Some v -> force v
              | None -> Gc.VFloat nan
            in
            ignore (pop1 frame_stack);
            Stack.push return_value stack
        | Opcode.STORE_VAR name ->
            push_trace ("STORE_VAR " ^ name);
            if Stack.is_empty stack then
              runtime_error ("STORE_VAR '" ^ name ^ "' failed: stack is empty")
            else
              let value = pop1 stack in
              let lazy_value = Gc.VThunk (fun () -> force value) in
              if List.mem_assoc name frame.env then (
                frame.env <-
                  (name, (lazy_value, true)) :: List.remove_assoc name frame.env;
                next ())
              else (
                global_env :=
                  (name, (lazy_value, true))
                  :: List.remove_assoc name !global_env;
                next ())
        | Opcode.PRINT ->
            push_trace "PRINT";
            if Stack.is_empty stack then
              runtime_error "PRINTLN attempted with empty stack"
            else
              let rec string_of_value = function
                | Gc.VUnit -> "unit"
                | Gc.VByte c -> Printf.sprintf "'%c'" c
                | Gc.VBool true -> "true"
                | Gc.VBool false -> "false"
                | Gc.VFloat f ->
                    if Float.is_nan f then "NaN"
                    else if Float.is_infinite f then
                      if f > 0.0 then "inf" else "-inf"
                    else if Float.floor f = f then Printf.sprintf "%.1f" f
                    else Printf.sprintf "%g" f
                | Gc.VInt i -> Bigint.to_string i
                | Gc.VHeapRef id -> (
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
                | Gc.VArray items ->
                    let contents =
                      items |> List.map string_of_value |> String.concat ", "
                    in
                    "[" ^ contents ^ "]"
                | Gc.VTuple items ->
                    let contents =
                      items |> List.map string_of_value |> String.concat ", "
                    in
                    "(" ^ contents ^ ")"
                | Gc.VModule _ -> "<module>"
                | Gc.VNative _ -> "<native function>"
                | Gc.VClosure name -> "<function " ^ name ^ ">"
                | Gc.VThunk _ -> "<thunk>"
                | Gc.VRange _ -> "<range>"
                | _ -> "<unknown>"
              in
              let value = force (pop1 stack) in
              safe_push_output (string_of_value value);
              next ()
        | Opcode.INPUT -> (
            push_trace "INPUT";
            if Stack.is_empty stack then
              runtime_error "Stack underflow during INPUT"
            else
              let value = pop1 stack in
              let id =
                match value with
                | Gc.VHeapRef id -> id
                | _ -> runtime_error "INPUT expected a heap reference ID"
              in
              match Gc.get_string id with
              | Some prompt ->
                  Printf.printf "%s" prompt;
                  let input_value = read_line () in
                  let processed_value =
                    try Gc.VFloat (float_of_string input_value)
                    with Failure _ ->
                      if String.length input_value = 1 then
                        Gc.VByte input_value.[0]
                      else Gc.alloc_string_with_gc stack frame.env input_value
                  in
                  Stack.push processed_value stack;
                  next ()
              | None -> runtime_error "Invalid prompt ID for INPUT")
        | Opcode.NEG ->
            push_trace "NEG";
            let v = force (pop1 stack) in
            (match v with
            | Gc.VFloat f ->
                let res = -.f in
                if
                  classify_float res = FP_nan
                  || classify_float res = FP_infinite
                then runtime_error "Float overflow in negation"
                else Stack.push (Gc.VFloat res) stack
            | Gc.VInt i -> Stack.push (Gc.VInt (Bigint.neg i)) stack
            | _ -> runtime_error "NEG expects a float or int");
            next ()
        | Opcode.FLOAT ->
            push_trace "FLOAT";
            let v = force (pop1 stack) in
            let float_val =
              match v with
              | Gc.VFloat f -> f
              | Gc.VInt i -> Bigint.to_float i
              | Gc.VHeapRef id -> (
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
            Stack.push (Gc.VFloat float_val) stack;
            next ()
        | Opcode.INT ->
            push_trace "TO_INT";
            let v = force (pop1 stack) in
            let int_val =
              match v with
              | Gc.VInt i -> i
              | Gc.VFloat f -> Bigint.of_float f
              | Gc.VByte c -> Bigint.of_int (Char.code c)
              | Gc.VHeapRef id -> (
                  match Gc.get_string id with
                  | Some s -> (
                      try Bigint.of_string s
                      with Failure _ ->
                        runtime_error ("INT: invalid int string: " ^ s))
                  | None ->
                      runtime_error "INT: invalid heap reference for string")
              | _ -> runtime_error "INT: unsupported type for int conversion"
            in
            Stack.push (Gc.VInt int_val) stack;
            next ()
        | Opcode.STRING ->
            push_trace "TO_STRING";
            let v = force (pop1 stack) in
            let str_val =
              match v with
              | Gc.VHeapRef id -> (
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
              | Gc.VArray vs ->
                  if
                    List.for_all (function Gc.VByte _ -> true | _ -> false) vs
                  then
                    String.init (List.length vs) (fun i ->
                        match List.nth vs i with Gc.VByte c -> c | _ -> '\000')
                  else if
                    List.for_all
                      (function Gc.VFloat _ -> true | _ -> false)
                      vs
                  then
                    String.concat ""
                      (List.map
                         (function Gc.VFloat f -> string_of_float f | _ -> "")
                         vs)
                  else if
                    List.for_all (function Gc.VInt _ -> true | _ -> false) vs
                  then
                    String.concat ""
                      (List.map
                         (function Gc.VInt i -> Bigint.to_string i | _ -> "")
                         vs)
                  else
                    runtime_error
                      "STRING: array is not []byte, []float or []int"
              | Gc.VInt i -> Bigint.to_string i
              | Gc.VFloat f -> string_of_float f
              | Gc.VBool b -> if b then "true" else "false"
              | Gc.VByte c -> String.make 1 c
              | Gc.VUnit -> "()"
              | _ ->
                  runtime_error "STRING: unsupported type for string conversion"
            in
            let new_str_ref = Gc.alloc_string_with_gc stack frame.env str_val in
            Stack.push new_str_ref stack;
            next ()
        | Opcode.BYTES ->
            push_trace "TO_BYTES";
            let v = pop1 stack in
            let str_val =
              match v with
              | Gc.VHeapRef id -> (
                  match Gc.get_string id with
                  | Some s -> s
                  | None ->
                      runtime_error
                        "TO_BYTES: invalid heap reference for string")
              | Gc.VInt i -> Bigint.to_string i
              | Gc.VFloat f -> string_of_float f
              | Gc.VBool b -> if b then "true" else "false"
              | Gc.VByte c -> String.make 1 c
              | Gc.VUnit -> "()"
              | _ -> runtime_error "TO_BYTES: unsupported type for conversion"
            in
            let byte_array = string_to_bytes str_val in
            let new_bytes_ref =
              Gc.alloc_bytes_with_gc stack frame.env byte_array
            in
            Stack.push new_bytes_ref stack;
            next ()
        | Opcode.BYTE -> (
            push_trace "TO_BYTE";
            let v = force (pop1 stack) in
            match v with
            | Gc.VInt i ->
                Stack.push
                  (Gc.VByte
                     (Char.chr
                        (Bigint.to_int
                           (Bigint.logand i (Bigint.of_int64 0xFFL)))))
                  stack;
                next ()
            | Gc.VByte c ->
                Stack.push (Gc.VByte c) stack;
                next ()
            | Gc.VHeapRef id -> (
                match Gc.get_string id with
                | Some s ->
                    if String.length s = 1 then
                      Stack.push (Gc.VByte s.[0]) stack
                    else runtime_error "BYTE: heap string length must be 1"
                | None ->
                    runtime_error "BYTE: invalid heap reference for string")
            | Gc.VFloat f ->
                Stack.push
                  (Gc.VByte (Char.chr (int_of_float f land 0xFF)))
                  stack;
                next ()
            | Gc.VArray vs ->
                if List.for_all (function Gc.VInt _ -> true | _ -> false) vs
                then (
                  let byte_arr =
                    Array.of_list
                      (List.map
                         (function
                           | Gc.VInt i ->
                               Char.chr
                                 (Bigint.to_int
                                    (Bigint.logand i (Bigint.of_int64 0xFFL)))
                           | _ -> assert false)
                         vs)
                  in
                  let vbyte_arr = Array.map (fun c -> Gc.VByte c) byte_arr in
                  Stack.push (Gc.VArray (Array.to_list vbyte_arr)) stack;
                  next ())
                else runtime_error "BYTE: array contains non-integer elements"
            | _ -> runtime_error "BYTE: unsupported type for byte conversion")
        | Opcode.PLUSASSIGN ->
            push_trace "PLUSASSIGN";
            let value = force (pop1 stack) in
            let var = force (pop1 stack) in
            let name =
              match var with
              | Gc.VHeapRef id -> (
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
              | Gc.VFloat oldf, Gc.VFloat newf -> Gc.VFloat (oldf +. newf)
              | Gc.VInt oldi, Gc.VInt newi -> Gc.VInt (Bigint.add oldi newi)
              | _ -> runtime_error "PLUSASSIGN: type mismatch"
            in
            frame.env <- update_variable name result frame.env;
            Stack.push result stack;
            next ()
        | Opcode.MINUSASSIGN ->
            push_trace "MINUSASSIGN";
            let value = force (pop1 stack) in
            let var = force (pop1 stack) in
            let name =
              match var with
              | Gc.VHeapRef id -> (
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
              | Gc.VFloat oldf, Gc.VFloat newf -> Gc.VFloat (oldf -. newf)
              | Gc.VInt oldi, Gc.VInt newi -> Gc.VInt (Bigint.sub oldi newi)
              | _ -> runtime_error "MINUSASSIGN: type mismatch"
            in
            frame.env <- update_variable name result frame.env;
            next ()
        | Opcode.STARASSIGN ->
            push_trace "STARASSIGN";
            let value = force (pop1 stack) in
            let var = force (pop1 stack) in
            let name =
              match var with
              | Gc.VHeapRef id -> (
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
              | Gc.VFloat oldf, Gc.VFloat newf -> Gc.VFloat (oldf *. newf)
              | Gc.VInt oldi, Gc.VInt newi -> Gc.VInt (Bigint.mul oldi newi)
              | _ -> runtime_error "STARASSIGN: type mismatch"
            in
            frame.env <- update_variable name result frame.env;
            next ()
        | Opcode.SLASHASSIGN ->
            push_trace "SLASHASSIGN";
            let value = force (pop1 stack) in
            let var = force (pop1 stack) in
            let name =
              match var with
              | Gc.VHeapRef id -> (
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
              | Gc.VFloat oldf, Gc.VFloat newf ->
                  if newf = 0.0 then
                    runtime_error "SLASHASSIGN: division by zero";
                  Gc.VFloat (oldf /. newf)
              | Gc.VInt oldi, Gc.VInt newi ->
                  if newi = Bigint.zero then
                    runtime_error "SLASHASSIGN: division by zero";
                  Gc.VInt (Bigint.div oldi newi)
              | _ -> runtime_error "SLASHASSIGN: type mismatch"
            in
            frame.env <- update_variable name result frame.env;
            next ()
        | Opcode.LOAD_VAR_REF name ->
            push_trace ("LOAD_VAR_REF " ^ name);
            let v = Gc.alloc_string_with_gc stack frame.env name in
            Stack.push v stack;
            next ()
        | Opcode.LENGTH ->
            push_trace "LENGTH";
            if Stack.is_empty stack then
              runtime_error "LENGTH expects a value on the stack"
            else
              let v = pop1 stack in
              let length =
                match v with
                | Gc.VHeapRef id -> (
                    match Gc.find_heap_obj id with
                    | Gc.HString s -> Bigint.of_int (String.length s)
                    | Gc.HBytes bytes_arr ->
                        Bigint.of_int (Array.length bytes_arr)
                    | _ ->
                        runtime_error
                          "LENGTH: heap reference is not a string or bytes")
                | Gc.VArray items -> Bigint.of_int (List.length items)
                | Gc.VTuple items -> Bigint.of_int (List.length items)
                | _ ->
                    runtime_error
                      "LENGTH expects a string, bytes, array, or tuple"
              in
              Stack.push (Gc.VInt length) stack;
              next ()
        | Opcode.BITWISENOT ->
            push_trace "BITWISE_NOT";
            let v = force (pop1 stack) in
            (match v with
            | Gc.VInt i -> Stack.push (Gc.VInt (Bigint.lognot i)) stack
            | _ -> runtime_error "BITWISE_NOT expects an int");
            next ()
        | Opcode.BITWISEAND ->
            push_trace "BITWISE_AND";
            let a, b = pop2_safe stack in
            let a, b = (force a, force b) in
            (match (a, b) with
            | Gc.VInt a, Gc.VInt b ->
                Stack.push (Gc.VInt (Bigint.logand a b)) stack
            | _ -> runtime_error "BITWISE_AND expects int operands");
            next ()
        | Opcode.BITWISEOR ->
            push_trace "BITWISE_OR";
            let a, b = pop2_safe stack in
            let a, b = (force a, force b) in
            (match (a, b) with
            | Gc.VInt a, Gc.VInt b ->
                Stack.push (Gc.VInt (Bigint.logor a b)) stack
            | _ -> runtime_error "BITWISE_OR expects int operands");
            next ()
        | Opcode.BITWISEXOR ->
            push_trace "BITWISE_XOR";
            let a, b = pop2_safe stack in
            let a, b = (force a, force b) in
            (match (a, b) with
            | Gc.VInt a, Gc.VInt b ->
                Stack.push (Gc.VInt (Bigint.logxor a b)) stack
            | _ -> runtime_error "BITWISE_XOR expects int operands");
            next ()
        | Opcode.LEFTSHIFT ->
            push_trace "LEFTSHIFT";
            let a, b = pop2_safe stack in
            let a, b = (force a, force b) in
            (match (a, b) with
            | Gc.VInt a, Gc.VInt b ->
                Stack.push (Gc.VInt (safe_shift_left a b)) stack
            | _ -> runtime_error "LEFTSHIFT expects int operands");
            next ()
        | Opcode.RIGHTSHIFT ->
            push_trace "RIGHT_SHIFT";
            let a, b = pop2_safe stack in
            let a, b = (force a, force b) in
            (match (a, b) with
            | Gc.VInt a, Gc.VInt b ->
                Stack.push
                  (Gc.VInt (Bigint.shift_right a (Bigint.to_int b)))
                  stack
            | _ -> runtime_error "RIGHT_SHIFT expects int operands");
            next ()
        | Opcode.BITWISEANDASSIGN ->
            push_trace "BITWISE_ANDASSIGN";
            let value = pop1 stack in
            let var = pop1 stack in
            let name =
              match var with
              | Gc.VHeapRef id -> (
                  match Gc.get_string id with
                  | Some name -> name
                  | None -> runtime_error "BITWISE_ANDASSIGN: invalid var ref")
              | _ -> runtime_error "BITWISE_ANDASSIGN: expected var name"
            in
            let old_value = get_var frame.env name in
            let result =
              match (old_value, value) with
              | Gc.VInt oldi, Gc.VInt newi -> Gc.VInt (Bigint.logand oldi newi)
              | _ -> runtime_error "BITWISE_ANDASSIGN: type mismatch"
            in
            frame.env <- update_variable name result frame.env;
            next ()
        | Opcode.BITWISEORASSIGN ->
            push_trace "BITWISE_ORASSIGN";
            let value = pop1 stack in
            let var = pop1 stack in
            let name =
              match var with
              | Gc.VHeapRef id -> (
                  match Gc.get_string id with
                  | Some name -> name
                  | None -> runtime_error "BITWISE_ORASSIGN: invalid var ref")
              | _ -> runtime_error "BITWISE_ORASSIGN: expected var name"
            in
            let old_value = get_var frame.env name in
            let result =
              match (old_value, value) with
              | Gc.VInt oldi, Gc.VInt newi -> Gc.VInt (Bigint.logor oldi newi)
              | _ -> runtime_error "BITWISE_ORASSIGN: type mismatch"
            in
            frame.env <- update_variable name result frame.env;
            next ()
        | Opcode.BITWISEXORASSIGN ->
            push_trace "BITWISE_XORASSIGN";
            let value = pop1 stack in
            let var = pop1 stack in
            let name =
              match var with
              | Gc.VHeapRef id -> (
                  match Gc.get_string id with
                  | Some name -> name
                  | None -> runtime_error "BITWISE_XORASSIGN: invalid var ref")
              | _ -> runtime_error "BITWISE_XORASSIGN: expected var name"
            in
            let old_value = get_var frame.env name in
            let result =
              match (old_value, value) with
              | Gc.VInt oldi, Gc.VInt newi -> Gc.VInt (Bigint.logxor oldi newi)
              | _ -> runtime_error "BITWISE_XORASSIGN: type mismatch"
            in
            frame.env <- update_variable name result frame.env;
            next ()
        | Opcode.LEFTSHIFTASSIGN ->
            push_trace "LEFT_SHIFTASSIGN";
            let value = pop1 stack in
            let var = pop1 stack in
            let name =
              match var with
              | Gc.VHeapRef id -> (
                  match Gc.get_string id with
                  | Some name -> name
                  | None -> runtime_error "LEFT_SHIFTASSIGN: invalid var ref")
              | _ -> runtime_error "LEFT_SHIFTASSIGN: expected var name"
            in
            let old_value = get_var frame.env name in
            let result =
              match (old_value, value) with
              | Gc.VInt oldi, Gc.VInt newi ->
                  Gc.VInt (Bigint.shift_left oldi (Bigint.to_int newi))
              | _ -> runtime_error "LEFT_SHIFTASSIGN: type mismatch"
            in
            frame.env <- update_variable name result frame.env;
            next ()
        | Opcode.RIGHTSHIFTASSIGN ->
            push_trace "RIGHT_SHIFTASSIGN";
            let value = pop1 stack in
            let var = pop1 stack in
            let name =
              match var with
              | Gc.VHeapRef id -> (
                  match Gc.get_string id with
                  | Some name -> name
                  | None -> runtime_error "RIGHT_SHIFTASSIGN: invalid var ref")
              | _ -> runtime_error "RIGHT_SHIFTASSIGN: expected var name"
            in
            let old_value = get_var frame.env name in
            let result =
              match (old_value, value) with
              | Gc.VInt oldi, Gc.VInt newi ->
                  Gc.VInt (Bigint.shift_right oldi (Bigint.to_int newi))
              | _ -> runtime_error "RIGHT_SHIFTASSIGN: type mismatch"
            in
            frame.env <- update_variable name result frame.env;
            next ()
        | Opcode.ASSERT -> (
            push_trace "ASSERT";
            let cond = force (pop1 stack) in
            match cond with
            | Gc.VBool true -> next ()
            | Gc.VBool false -> runtime_error "Assertion failed"
            | _ -> runtime_error "ASSERT expects a boolean value")
        | Opcode.PANIC ->
            push_trace "PANIC";
            if Stack.is_empty stack then
              runtime_error "PANIC attempted with empty stack"
            else
              let string_of_value = function
                | Gc.VHeapRef id -> (
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
        | Opcode.LOAD_MODULE name ->
            let m =
              match Hashtbl.find_opt stdlib_modules name with
              | Some modval -> modval
              | None -> failwith ("Unknown module: " ^ name)
            in
            Stack.push m stack;
            next ()
        | Opcode.LOAD_FIELD name -> (
            let module_or_obj = pop1 stack in
            match module_or_obj with
            | Gc.VModule table -> (
                match Hashtbl.find_opt table name with
                | Some v ->
                    Stack.push v stack;
                    next ()
                | None -> failwith ("Unknown field " ^ name))
            | _ -> failwith "LOAD_FIELD expects a module or object")
        | Opcode.CLOSURE func_name ->
            Stack.push (Gc.VClosure func_name) stack;
            next ()
        | Opcode.MAKE_RANGE ->
            let v = Stack.pop stack in
            let start =
              match v with
              | Gc.VInt x -> x
              | _ -> runtime_error "MAKE_RANGE expects int"
            in
            let range_val = Gc.alloc_range start Bigint.one None in
            let id =
              match range_val with
              | Gc.VHeapRef id -> id
              | _ -> runtime_error "alloc_range must return Gc.VHeapRef"
            in
            Stack.push (Gc.VRange id) stack;
            next ()
        | Opcode.ARRAYCONCAT -> (
            push_trace "ARRAYCONCAT";
            if Stack.length stack < 2 then
              runtime_error "ARRAYCONCAT requires two arrays on the stack"
            else
              let right = force (pop1 stack) in
              let left = force (pop1 stack) in
              match (left, right) with
              | Gc.VArray l1, Gc.VArray l2 ->
                  Stack.push (Gc.VArray (l1 @ l2)) stack;
                  next ()
              | _ -> runtime_error "ARRAYCONCAT expects two arrays")
        | Opcode.SLICE -> (
            let v_end = force (pop1 stack) in
            let v_start = force (pop1 stack) in
            let v_arr = force (pop1 stack) in
            match v_arr with
            | Gc.VArray elements ->
                let len = List.length elements in
                let s =
                  match v_start with
                  | Gc.VInt s -> max 0 (Bigint.to_int s)
                  | Gc.VUnit -> 0
                  | _ -> runtime_error "SLICE: start index must be int or unit"
                in
                let e =
                  match v_end with
                  | Gc.VInt e -> min len (Bigint.to_int e)
                  | Gc.VUnit -> len
                  | _ -> runtime_error "SLICE: end index must be int or unit"
                in
                let slice =
                  if s > e then []
                  else
                    elements |> List.to_seq |> Seq.drop s
                    |> Seq.take (e - s)
                    |> List.of_seq
                in
                Stack.push (Gc.VArray slice) stack;
                next ()
            | _ -> runtime_error "SLICE expects array as first argument")
        | Opcode.TYPE ->
            push_trace "TYPE";
            if Stack.is_empty stack then
              runtime_error "TYPE expects a value on the stack"
            else
              let v = force (Stack.pop stack) in
              let type_str =
                match v with
                | Gc.VInt _ -> "int"
                | Gc.VFloat _ -> "float"
                | Gc.VHeapRef id -> (
                    match Gc.get_string id with
                    | Some _ -> "string"
                    | None -> (
                        match Gc.get_bytes id with
                        | Some _ -> "bytes"
                        | None -> (
                            match Gc.get_value id with
                            | Some _ -> "ref"
                            | None -> "unknown")))
                | Gc.VByte _ -> "byte"
                | Gc.VBool _ -> "bool"
                | Gc.VUnit -> "unit"
                | Gc.VArray _ -> "array"
                | Gc.VTuple _ -> "tuple"
                | Gc.VRange _ -> "range"
                | Gc.VNative _ -> "native"
                | Gc.VModule _ -> "module"
                | Gc.VClosure _ -> "function"
                | Gc.VThunk _ -> "thunk"
                | _ -> "unknown"
              in
              let v = Gc.alloc_string_with_gc stack frame.env type_str in
              Stack.push v stack;
              next ()
        | Opcode.CALL_CLOSURE arg_count -> (
            if Stack.length stack < arg_count + 1 then
              runtime_error
                ("CALL_CLOSURE requires " ^ string_of_int arg_count
               ^ " arguments plus closure, but stack has only "
                ^ string_of_int (Stack.length stack));

            let closure_val = Stack.pop stack in
            let args = pop_n [] arg_count in
            match closure_val with
            | Gc.VClosure func_name -> (
                match Hashtbl.find_opt Bytecode.function_table func_name with
                | None ->
                    runtime_error
                      ("CALL_CLOSURE: unknown closure function: " ^ func_name)
                | Some function_body ->
                    let param_names =
                      List.map
                        (fun (p : Ast.Stmt.parameter) -> p.Ast.Stmt.name)
                        function_body.Bytecode.params
                    in
                    if List.length param_names <> arg_count then
                      runtime_error
                        ("CALL_CLOSURE: argument count mismatch, expected "
                        ^ string_of_int (List.length param_names)
                        ^ " but got " ^ string_of_int arg_count);
                    let local_env =
                      List.combine param_names
                        (List.map (fun v -> (v, true)) args)
                    in
                    let roots = Gc.get_stack_roots stack in
                    Gc.maybe_collect_gc roots local_env;
                    let skip_header = 1 + arg_count in
                    let body =
                      List.drop skip_header function_body.Bytecode.bytecode
                    in
                    let code = Array.of_list body in
                    frame.pc <- frame.pc + 1;
                    push_frame code local_env)
            | _ -> runtime_error "CALL_CLOSURE: top of stack is not a closure")
        | Opcode.HEAD -> (
            let v_arr = force (pop1 stack) in
            match v_arr with
            | Gc.VArray (h :: _) ->
                Stack.push h stack;
                next ()
            | Gc.VArray [] -> runtime_error "HEAD: empty array"
            | _ -> runtime_error "HEAD expects an array")
        | Opcode.TAIL -> (
            let v_arr = force (pop1 stack) in
            match v_arr with
            | Gc.VArray (_ :: t) ->
                Stack.push (Gc.VArray t) stack;
                next ()
            | Gc.VArray [] -> Stack.push (Gc.VArray []) stack
            | _ -> runtime_error "TAIL expects an array")
        | Opcode.REVERSE -> (
            let v_arr = force (pop1 stack) in
            match v_arr with
            | Gc.VArray elements ->
                let reversed = List.rev elements in
                Stack.push (Gc.VArray reversed) stack;
                next ()
            | _ -> runtime_error "REVERSE expects an array")
        | _ -> failwith "Not Supported"
    done;
    flush_buffer ()
  with RuntimeError msg ->
    print_trace msg;
    exit 1
