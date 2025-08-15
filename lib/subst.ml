open Ast.Type

type t = (int, Ast.Type.t) Hashtbl.t

let empty () = Hashtbl.create 16
let find_opt s id = try Some (Hashtbl.find s id) with Not_found -> None
let add s id ty = Hashtbl.replace s id ty

let rec apply s ty =
  match ty with
  | VarType id -> (
      match find_opt s id with
      | None -> VarType id
      | Some ty' ->
          let ty'' = apply s ty' in
          Hashtbl.replace s id ty'';
          ty'')
  | ArrayType { element_type } ->
      ArrayType { element_type = apply s element_type }
  | TupleType lst -> TupleType (List.map (apply s) lst)
  | FunctionType (params, ret) ->
      FunctionType (List.map (apply s) params, apply s ret)
  | RecordType fields ->
      RecordType (List.map (fun (n, t) -> (n, apply s t)) fields)
  | (Any | Infer | SymbolType _) as other -> other
  | _ -> ty
