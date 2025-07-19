open Calyxium_stdlib.Cmathlib
open Gc

type stdlib_module = string * (string * value) list

let define_module (name : string) (entries : (string * value) list) :
    stdlib_module =
  (name, entries)

let native1 f =
  VNative (fun args -> match args with [ a ] -> f a | _ -> failwith "artiy")

let native2 f =
  VNative
    (fun args -> match args with [ a; b ] -> f a b | _ -> failwith "arity")

let stdlib_definitions : stdlib_module list =
  [
    define_module "Math"
      [
        ("pi", VFloat Float.pi);
        ("tau", VFloat Math.tau);
        ("nan", VFloat Math.nan);
        ("inf", VFloat Math.infinity);
        ("neg_inf", VFloat Math.neg_infinity);
        ( "sin",
          native1 (function
            | VFloat x -> VFloat (Math.sin x)
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
