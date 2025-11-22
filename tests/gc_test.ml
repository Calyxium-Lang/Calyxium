[@@@ocaml.warning "-4-40-42"]

open Printf
open Calyxium_vm.Gc
open Calyxium_parser

let test_case name f =
  try
    f ();
    printf "Passed: %s\n" name
  with
  | Failure msg ->
      printf "[FAIL] %s: %s\n" name msg;
      exit 1
  | e ->
      printf "[ERROR] %s: unexpected exception: %s\n" name
        (Printexc.to_string e)

let () =
  print_endline "GC Testing";

  test_case "interned string allocation" (fun () ->
      reset_heap ();
      let stack = Stack.create () in
      let env = [] in
      let v1 = alloc_string_with_gc stack env "hello" in
      let v2 = alloc_string_with_gc stack env "hello" in
      match (v1, v2) with
      | VHeapRef id1, VHeapRef id2 ->
          if id1 <> id2 then failwith "Interned string allocated multiple times"
      | _ -> failwith "Expected VHeapRef for string");

  test_case "array allocation and retrieval" (fun () ->
      reset_heap ();
      let stack = Stack.create () in
      let env = [] in
      let arr = [| 1.0; 2.0; 3.0 |] in
      let v = alloc_array_with_gc stack env arr in
      match v with
      | VHeapRef id -> (
          match get_array id with
          | Some a ->
              if Array.length a <> 3 then failwith "Array length mismatch"
          | None -> failwith "Failed to retrieve array from heap")
      | _ -> failwith "Expected VHeapRef for array");

  test_case "range allocation" (fun () ->
      reset_heap ();
      let r = alloc_range Bigint.zero Bigint.one (Some (Bigint.of_int 10)) in
      match r with
      | VHeapRef id -> (
          match get_value id with
          | Some (VRange _) -> ()
          | _ -> failwith "Range not correctly allocated")
      | _ -> failwith "Expected VHeapRef for range");

  test_case "young to old promotion" (fun () ->
      reset_heap ();
      let stack = Stack.create () in
      let env = [ ("x", (VInt Bigint.one, false)) ] in
      let v = alloc_string_with_gc stack env "gc_test" in
      for _ = 1 to !allocation_threshold do
        ignore (alloc_string_with_gc stack env "dummy")
      done;
      mark_and_promote [ v ] env;
      match v with
      | VHeapRef id ->
          if not (Hashtbl.mem old_gen.objs id) then
            failwith "Object not promoted to old generation after GC"
      | _ -> failwith "Expected VHeapRef for string");

  test_case "get_string returns correct value" (fun () ->
      reset_heap ();
      let stack = Stack.create () in
      let env = [] in
      let v = alloc_string_with_gc stack env "teststr" in
      match v with
      | VHeapRef id -> (
          match get_string id with
          | Some s when s = "teststr" -> ()
          | _ -> failwith "get_string failed")
      | _ -> failwith "Expected VHeapRef");

  test_case "bytes allocation and retrieval" (fun () ->
      reset_heap ();
      let stack = Stack.create () in
      let env = [] in
      let b = [| 'a'; 'b'; 'c' |] in
      let v = alloc_bytes_with_gc stack env b in
      match v with
      | VHeapRef id -> (
          match get_bytes id with
          | Some arr when Array.to_list arr = [ 'a'; 'b'; 'c' ] -> ()
          | _ -> failwith "get_bytes failed")
      | _ -> failwith "Expected VHeapRef");

  test_case "array to VArray conversion" (fun () ->
      reset_heap ();
      let stack = Stack.create () in
      let env = [] in
      let arr = [| 1.1; 2.2 |] in
      let v = alloc_array_with_gc stack env arr in
      match v with
      | VHeapRef id -> (
          match get_value id with
          | Some (VArray [ VFloat 1.1; VFloat 2.2 ]) -> ()
          | _ -> failwith "Array to VArray conversion failed")
      | _ -> failwith "Expected VHeapRef");

  test_case "marking nested VArray" (fun () ->
      reset_heap ();
      let stack = Stack.create () in
      let env = [] in
      let inner = alloc_string_with_gc stack env "inner" in
      let v = alloc_array_with_gc stack env [| 1.0; 2.0 |] in
      mark_value (VArray [ inner; v ]);
      ());

  test_case "range allocation with no end" (fun () ->
      reset_heap ();
      let v = alloc_range Bigint.zero Bigint.one None in
      match v with
      | VHeapRef id -> (
          match get_value id with
          | Some (VRange _) -> ()
          | _ -> failwith "Range with None end failed")
      | _ -> failwith "Expected VHeapRef");

  test_case "large allocation stress test" (fun () ->
      reset_heap ();
      let stack = Stack.create () in
      let env = [] in

      let num = 500_000 in
      let refs = ref [] in
      for i = 1 to num do
        let s = "str_" ^ string_of_int i in
        let v = alloc_string_with_gc stack env s in
        refs := v :: !refs
      done;

      List.iter mark_value !refs;

      mark_and_promote !refs env);

  print_endline "All GC tests passed.\n"
