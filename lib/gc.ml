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
let allocation_threshold = 1000
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

let get_string id =
  match Hashtbl.find_opt young_gen.objs id with
  | Some (HString s) -> Some s
  | _ -> (
      match Hashtbl.find_opt old_gen.objs id with
      | Some (HString s) -> Some s
      | _ -> None)

let get_array id =
  match Hashtbl.find_opt young_gen.objs id with
  | Some (HArray arr) -> Some arr
  | _ -> (
      match Hashtbl.find_opt old_gen.objs id with
      | Some (HArray arr) -> Some arr
      | _ -> None)

let mark id gen =
  if not (Hashtbl.mem gen.marked id) then Hashtbl.replace gen.marked id true

let rec mark_value = function
  | VFloat _ -> ()
  | VInt _ -> ()
  | VInt64 _ -> ()
  | VBool _ -> ()
  | VByte _ -> ()
  | VHeapRef id ->
      if Hashtbl.mem young_gen.objs id then mark id young_gen
      else if Hashtbl.mem old_gen.objs id then mark id old_gen
  | VArray values -> List.iter mark_value values
  | VTuple values -> List.iter mark_value values
  | VUnit -> ()

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

  Hashtbl.iter
    (fun id obj ->
      if Hashtbl.mem young_gen.marked id then
        Hashtbl.replace old_gen.objs id obj)
    young_gen.objs;

  sweep young_gen;
  sweep old_gen;
  ()

let maybe_collect_gc (roots : value list) (env : (string * (value * bool)) list)
    =
  incr allocation_count;
  if !allocation_count >= allocation_threshold then (
    mark_and_promote roots env;
    allocation_count := 0)

let get_stack_roots (stack : value Stack.t) : value list =
  Stack.fold (fun acc v -> v :: acc) [] stack

let alloc_string_with_gc stack env s =
  match Hashtbl.find_opt interned_strings s with
  | Some id -> VHeapRef id
  | None ->
      let roots = get_stack_roots stack in
      maybe_collect_gc roots env;
      let id = !next_id in
      incr next_id;
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
