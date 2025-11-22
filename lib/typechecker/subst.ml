open Calyxium_parser

type t = (int, Ast.Type.t) Hashtbl.t

let empty () = Hashtbl.create 16
let find_opt s id = try Some (Hashtbl.find s id) with Not_found -> None
let add s id ty = Hashtbl.replace s id ty

let rec apply s ty =
  match ty with
  | Ast.Type.VarType id -> (
      match find_opt s id with
      | None -> Ast.Type.VarType id
      | Some ty' ->
          let ty'' = apply s ty' in
          Hashtbl.replace s id ty'';
          ty'')
  | Ast.Type.ArrayType { element_type } ->
      Ast.Type.ArrayType { element_type = apply s element_type }
  | Ast.Type.TupleType (lst, rest) ->
      let lst' = List.map (apply s) lst in
      let rest' = Option.map (apply s) rest in
      Ast.Type.TupleType (lst', rest')
  | Ast.Type.FunctionType (params, ret) ->
      Ast.Type.FunctionType (List.map (apply s) params, apply s ret)
  | Ast.Type.RecordType fields ->
      Ast.Type.RecordType (List.map (fun (n, t) -> (n, apply s t)) fields)
  | (Ast.Type.Any | Ast.Type.Infer | Ast.Type.SymbolType _) as other -> other
  | _ -> ty
