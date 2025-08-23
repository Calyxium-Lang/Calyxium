[@@@ocaml.warning "-4-40-42"]

open Printf

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
      Calyxiumlib.Gc.reset_heap ();
      let stack = Stack.create () in
      let env = [] in
      let v1 = Calyxiumlib.Gc.alloc_string_with_gc stack env "hello" in
      let v2 = Calyxiumlib.Gc.alloc_string_with_gc stack env "hello" in
      match (v1, v2) with
      | Calyxiumlib.Gc.VHeapRef id1, Calyxiumlib.Gc.VHeapRef id2 ->
          if id1 <> id2 then failwith "Interned string allocated multiple times"
      | _ -> failwith "Expected VHeapRef for string");

  test_case "array allocation and retrieval" (fun () ->
      Calyxiumlib.Gc.reset_heap ();
      let stack = Stack.create () in
      let env = [] in
      let arr = [| 1.0; 2.0; 3.0 |] in
      let v = Calyxiumlib.Gc.alloc_array_with_gc stack env arr in
      match v with
      | Calyxiumlib.Gc.VHeapRef id -> (
          match Calyxiumlib.Gc.get_array id with
          | Some a ->
              if Array.length a <> 3 then failwith "Array length mismatch"
          | None -> failwith "Failed to retrieve array from heap")
      | _ -> failwith "Expected VHeapRef for array");

  test_case "range allocation" (fun () ->
      Calyxiumlib.Gc.reset_heap ();
      let r =
        Calyxiumlib.Gc.alloc_range Calyxiumlib.Bigint.zero
          Calyxiumlib.Bigint.one
          (Some (Calyxiumlib.Bigint.of_int 10))
      in
      match r with
      | Calyxiumlib.Gc.VHeapRef id -> (
          match Calyxiumlib.Gc.get_value id with
          | Some (Calyxiumlib.Gc.VRange _) -> ()
          | _ -> failwith "Range not correctly allocated")
      | _ -> failwith "Expected VHeapRef for range");

  test_case "young to old promotion" (fun () ->
      Calyxiumlib.Gc.reset_heap ();
      let stack = Stack.create () in
      let env =
        [ ("x", (Calyxiumlib.Gc.VInt Calyxiumlib.Bigint.one, false)) ]
      in
      let v = Calyxiumlib.Gc.alloc_string_with_gc stack env "gc_test" in
      for _ = 1 to !Calyxiumlib.Gc.allocation_threshold do
        ignore (Calyxiumlib.Gc.alloc_string_with_gc stack env "dummy")
      done;
      Calyxiumlib.Gc.mark_and_promote [ v ] env;
      match v with
      | Calyxiumlib.Gc.VHeapRef id ->
          if not (Hashtbl.mem Calyxiumlib.Gc.old_gen.objs id) then
            failwith "Object not promoted to old generation after GC"
      | _ -> failwith "Expected VHeapRef for string");

  test_case "get_string returns correct value" (fun () ->
      Calyxiumlib.Gc.reset_heap ();
      let stack = Stack.create () in
      let env = [] in
      let v = Calyxiumlib.Gc.alloc_string_with_gc stack env "teststr" in
      match v with
      | Calyxiumlib.Gc.VHeapRef id -> (
          match Calyxiumlib.Gc.get_string id with
          | Some s when s = "teststr" -> ()
          | _ -> failwith "get_string failed")
      | _ -> failwith "Expected VHeapRef");

  test_case "bytes allocation and retrieval" (fun () ->
      Calyxiumlib.Gc.reset_heap ();
      let stack = Stack.create () in
      let env = [] in
      let b = [| 'a'; 'b'; 'c' |] in
      let v = Calyxiumlib.Gc.alloc_bytes_with_gc stack env b in
      match v with
      | Calyxiumlib.Gc.VHeapRef id -> (
          match Calyxiumlib.Gc.get_bytes id with
          | Some arr when Array.to_list arr = [ 'a'; 'b'; 'c' ] -> ()
          | _ -> failwith "get_bytes failed")
      | _ -> failwith "Expected VHeapRef");

  test_case "array to VArray conversion" (fun () ->
      Calyxiumlib.Gc.reset_heap ();
      let stack = Stack.create () in
      let env = [] in
      let arr = [| 1.1; 2.2 |] in
      let v = Calyxiumlib.Gc.alloc_array_with_gc stack env arr in
      match v with
      | Calyxiumlib.Gc.VHeapRef id -> (
          match Calyxiumlib.Gc.get_value id with
          | Some
              (Calyxiumlib.Gc.VArray
                 [ Calyxiumlib.Gc.VFloat 1.1; Calyxiumlib.Gc.VFloat 2.2 ]) ->
              ()
          | _ -> failwith "Array to VArray conversion failed")
      | _ -> failwith "Expected VHeapRef");

  test_case "marking nested VArray" (fun () ->
      Calyxiumlib.Gc.reset_heap ();
      let stack = Stack.create () in
      let env = [] in
      let inner = Calyxiumlib.Gc.alloc_string_with_gc stack env "inner" in
      let v = Calyxiumlib.Gc.alloc_array_with_gc stack env [| 1.0; 2.0 |] in
      Calyxiumlib.Gc.mark_value (Calyxiumlib.Gc.VArray [ inner; v ]);
      ());

  test_case "range allocation with no end" (fun () ->
      Calyxiumlib.Gc.reset_heap ();
      let v =
        Calyxiumlib.Gc.alloc_range Calyxiumlib.Bigint.zero
          Calyxiumlib.Bigint.one None
      in
      match v with
      | Calyxiumlib.Gc.VHeapRef id -> (
          match Calyxiumlib.Gc.get_value id with
          | Some (Calyxiumlib.Gc.VRange _) -> ()
          | _ -> failwith "Range with None end failed")
      | _ -> failwith "Expected VHeapRef");

  test_case "large allocation stress test" (fun () ->
      Calyxiumlib.Gc.reset_heap ();
      let stack = Stack.create () in
      let env = [] in

      let num = 500_000 in
      let refs = ref [] in
      for i = 1 to num do
        let s = "str_" ^ string_of_int i in
        let v = Calyxiumlib.Gc.alloc_string_with_gc stack env s in
        refs := v :: !refs
      done;

      List.iter Calyxiumlib.Gc.mark_value !refs;

      Calyxiumlib.Gc.mark_and_promote !refs env);

  print_endline "All GC tests passed.\n"
