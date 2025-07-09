open Opcode
open Bytecode

type program_image = {
  entry : opcode list;
  functions : (string * opcode list) list;
}

let save_bytecode_to_file (filename : string) (entry : opcode list) : unit =
  let functions =
    Hashtbl.fold (fun name code acc -> (name, code) :: acc) function_table []
  in
  let prog = { entry; functions } in
  let oc = open_out_bin filename in
  Marshal.to_channel oc prog [];
  close_out oc

let load_bytecode_from_file (filename : string) : opcode list =
  let ic = open_in_bin filename in
  let { entry; functions } : program_image = Marshal.from_channel ic in
  close_in ic;
  List.iter
    (fun (name, code) -> Hashtbl.replace function_table name code)
    functions;
  entry
