open Opcode

let interned_strings : (string, int) Hashtbl.t = Hashtbl.create 100

type heap_obj =
  | HString of string
  | HArray of float array
  | HClosure of string * opcode list * (string * (value * bool)) list

and value =
  | VFloat of float
  | VInt of int
  | VInt64 of int64
  | VUint32 of Uint32.t
  | VBool of bool
  | VByte of char
  | VHeapRef of int
  | VArray of value list
  | VTuple of value list
  | VUnit

type generation = {
  objs : (int, heap_obj) Hashtbl.t;
  marked : (int, bool) Hashtbl.t;
}

let allocation_count = ref 0
let allocation_threshold = ref 1000
let young_gen = { objs = Hashtbl.create 128; marked = Hashtbl.create 128 }
let old_gen = { objs = Hashtbl.create 512; marked = Hashtbl.create 512 }
let next_id = ref 0

let alloc_id () =
  let id = !next_id in
  incr next_id;
  id

let alloc_in_young obj =
  let id = alloc_id () in
  Hashtbl.add young_gen.objs id obj;
  VHeapRef id

let find_heap_obj id =
  try Hashtbl.find young_gen.objs id
  with Not_found -> Hashtbl.find old_gen.objs id

let get_string id =
  match try Some (find_heap_obj id) with Not_found -> None with
  | Some (HString s) -> Some s
  | _ -> None

let get_array id =
  match try Some (find_heap_obj id) with Not_found -> None with
  | Some (HArray arr) -> Some arr
  | _ -> None

let mark id gen =
  if not (Hashtbl.mem gen.marked id) then Hashtbl.replace gen.marked id true

let mark_value v =
  let stack = Stack.create () in
  Stack.push v stack;
  while not (Stack.is_empty stack) do
    match Stack.pop stack with
    | VHeapRef id -> (
        try
          ignore (Hashtbl.find young_gen.objs id);
          mark id young_gen
        with Not_found -> (
          try
            ignore (Hashtbl.find old_gen.objs id);
            mark id old_gen
          with Not_found -> ()))
    | VArray values | VTuple values ->
        List.iter (fun v -> Stack.push v stack) values
    | _ -> ()
  done

let mark_env env = List.iter (fun (_, (v, _)) -> mark_value v) env

let sweep gen =
  Hashtbl.filter_map_inplace
    (fun id obj ->
      if Hashtbl.mem gen.marked id then (
        Hashtbl.remove gen.marked id;
        Some obj)
      else None)
    gen.objs

let mark_and_promote roots env =
  Hashtbl.clear young_gen.marked;
  List.iter mark_value roots;
  mark_env env;

  let to_promote = ref [] in
  Hashtbl.iter
    (fun id obj ->
      if Hashtbl.mem young_gen.marked id then
        to_promote := (id, obj) :: !to_promote)
    young_gen.objs;

  List.iter
    (fun (id, obj) ->
      Hashtbl.replace old_gen.objs id obj;
      Hashtbl.remove young_gen.objs id)
    !to_promote;

  sweep young_gen;
  sweep old_gen

let maybe_collect_gc roots env =
  incr allocation_count;
  if !allocation_count >= !allocation_threshold then (
    mark_and_promote roots env;
    allocation_count := 0)

let get_stack_roots stack =
  List.rev (Stack.fold (fun acc v -> v :: acc) [] stack)

let alloc_string_with_gc stack env s =
  match Hashtbl.find_opt interned_strings s with
  | Some id -> VHeapRef id
  | None ->
      let roots = get_stack_roots stack in
      maybe_collect_gc roots env;
      let id = alloc_id () in
      Hashtbl.add young_gen.objs id (HString s);
      Hashtbl.add interned_strings s id;
      VHeapRef id

let alloc_array_with_gc stack env arr =
  let roots = get_stack_roots stack in
  maybe_collect_gc roots env;
  alloc_in_young (HArray arr)

let reset_heap () =
  Hashtbl.clear young_gen.objs;
  Hashtbl.clear young_gen.marked;
  Hashtbl.clear old_gen.objs;
  Hashtbl.clear old_gen.marked;
  next_id := 0
