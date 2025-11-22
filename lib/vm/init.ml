open Calyxium_stdlib.Mathlib

type stdlib_module = string * (string * Gc.value) list

let define_module name entries = (name, entries)

let native1 f =
  Gc.VNative
    (fun args -> match args with [ a ] -> f a | _ -> failwith "artiy")

let native2 f =
  Gc.VNative
    (fun args -> match args with [ a; b ] -> f a b | _ -> failwith "arity")

let stdlib_definitions : stdlib_module list =
  [
    define_module "Math"
      [
        ("pi", Gc.VFloat Float.pi);
        ("tau", Gc.VFloat Math.tau);
        ("nan", Gc.VFloat Math.nan);
        ("inf", Gc.VFloat Math.infinity);
        ("neg_inf", Gc.VFloat Math.neg_infinity);
        ( "sin",
          native1 (function
            | Gc.VFloat x -> Gc.VFloat (Math.sin x)
            | _ -> failwith "sin expects a float") );
      ];
  ]

let stdlib_modules =
  let tbl = Hashtbl.create 10 in
  List.iter
    (fun (name, entries) ->
      let mod_tbl = Hashtbl.create 10 in
      List.iter (fun (k, v) -> Hashtbl.add mod_tbl k v) entries;
      Hashtbl.add tbl name mod_tbl)
    stdlib_definitions;
  tbl
