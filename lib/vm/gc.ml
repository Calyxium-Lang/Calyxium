open Calyxium_parser

let interned_strings = Hashtbl.create 1_000_000
let new_interned_strings = Hashtbl.create 1_000_000

type heap_obj = ..
type value = ..

type value +=
  | VFloat of float
  | VInt of Bigint.t
  | VBool of bool
  | VByte of char
  | VHeapRef of int
  | VArray of value list
  | VTuple of value list
  | VUnit
  | VModule of (string, value) Hashtbl.t
  | VNative of (value list -> value)
  | VClosure of string
  | VThunk of (unit -> value)
  | VRange of int

type heap_obj +=
  | HString of string
  | HArray of float array
  | HBytes of char array
  | HClosure of string * Opcode.opcode list * (string * (value * bool)) list
  | HRange of { current : Bigint.t; step : Bigint.t; end_ : Bigint.t option }

type generation = {
  objs : (int, heap_obj) Hashtbl.t;
  marked : (int, bool) Hashtbl.t;
}

let allocation_count = ref 0
let allocation_threshold = ref 10_000
let young_gen = { objs = Hashtbl.create 128; marked = Hashtbl.create 128 }
let old_gen = { objs = Hashtbl.create 512; marked = Hashtbl.create 512 }
let next_id = ref 0
let obj_size v = Obj.reachable_words (Obj.repr v) * Sys.word_size / 8

let heap_memory () =
  let sum_tbl tbl = Hashtbl.fold (fun _ obj acc -> acc + obj_size obj) tbl 0 in
  let young_mem = sum_tbl young_gen.objs in
  let old_mem = sum_tbl old_gen.objs in
  (young_mem, old_mem)

let print_heap_memory () =
  let y, o = heap_memory () in
  Printf.printf "Heap memory: young=%dB, old=%dB\n%!" y o

let heap_size () =
  let young_count = Hashtbl.length young_gen.objs in
  let old_count = Hashtbl.length old_gen.objs in
  (young_count, old_count)

let print_heap_stats () =
  let y, o = heap_size () in
  Printf.printf "Heap: young=%d, old=%d\n%!" y o

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
  let opt_obj = try Some (find_heap_obj id) with Not_found -> None in
  match opt_obj with
  | Some (HString s) -> Some s
  | Some _ -> None
  | None -> None

let get_bytes id =
  match try Some (find_heap_obj id) with Not_found -> None with
  | Some (HBytes arr) -> Some arr
  | Some _ -> None
  | None -> None

let get_array id =
  match try Some (find_heap_obj id) with Not_found -> None with
  | Some (HArray arr) -> Some arr
  | Some _ -> None
  | None -> None

let get_value id =
  match try Some (find_heap_obj id) with Not_found -> None with
  | Some (HString s) ->
      Some (VArray (List.init (String.length s) (fun i -> VByte s.[i])))
  | Some (HBytes b) ->
      Some (VArray (Array.to_list (Array.map (fun c -> VByte c) b)))
  | Some (HArray floats) ->
      Some (VArray (Array.to_list (Array.map (fun f -> VFloat f) floats)))
  | Some (HClosure (name, _, _)) -> Some (VClosure name)
  | Some (HRange _) -> Some (VRange id)
  | Some _ -> None
  | None -> None

let mark id gen =
  if not (Hashtbl.mem gen.marked id) then Hashtbl.replace gen.marked id true

let mark_value v =
  let stack = Stack.create () in
  Stack.push v stack;
  while not (Stack.is_empty stack) do
    match Stack.pop stack with
    | VHeapRef id | VRange id -> (
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
    | (_ : value) -> ()
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
  Hashtbl.clear old_gen.marked;

  List.iter mark_value roots;
  mark_env env;

  Hashtbl.iter
    (fun _ id ->
      mark id young_gen;
      mark id old_gen)
    new_interned_strings;

  let to_promote =
    Hashtbl.fold
      (fun id obj acc ->
        if Hashtbl.mem young_gen.marked id then (id, obj) :: acc else acc)
      young_gen.objs []
  in
  List.iter
    (fun (id, obj) ->
      Hashtbl.replace old_gen.objs id obj;
      Hashtbl.remove young_gen.objs id)
    to_promote;

  Hashtbl.clear new_interned_strings;
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
      Hashtbl.add new_interned_strings s id;
      VHeapRef id

let alloc_bytes_with_gc stack env bytes_arr =
  let roots = get_stack_roots stack in
  maybe_collect_gc roots env;
  let id = alloc_id () in
  Hashtbl.add young_gen.objs id (HBytes bytes_arr);
  VHeapRef id

let alloc_array_with_gc stack env arr =
  let roots = get_stack_roots stack in
  maybe_collect_gc roots env;
  alloc_in_young (HArray arr)

let alloc_range current step end_ =
  let id = alloc_id () in
  Hashtbl.add young_gen.objs id (HRange { current; step; end_ });
  VHeapRef id

let reset_heap () =
  Hashtbl.clear young_gen.objs;
  Hashtbl.clear young_gen.marked;
  Hashtbl.clear old_gen.objs;
  Hashtbl.clear old_gen.marked;
  Hashtbl.clear new_interned_strings;
  next_id := 0
