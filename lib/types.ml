exception TypeError of string
exception UnifyError of string

let fresh_var_counter = ref 0

let fresh_tyvar () =
  let id = !fresh_var_counter in
  incr fresh_var_counter;
  Ast.Type.VarType id

let option_exists f = function Some x -> f x | None -> false

let rec occurs_check s id ty =
  let ty = Subst.apply s ty in
  match ty with
  | Ast.Type.VarType v -> v = id
  | Ast.Type.ArrayType { element_type } -> occurs_check s id element_type
  | Ast.Type.TupleType (ts, rest) ->
      List.exists (occurs_check s id) ts
      || option_exists (occurs_check s id) rest
  | Ast.Type.FunctionType (params, ret) ->
      List.exists (occurs_check s id) params || occurs_check s id ret
  | Ast.Type.RecordType fields ->
      List.exists (fun (_, t) -> occurs_check s id t) fields
  | _ -> false

let rec string_of_type = function
  | Ast.Type.Any -> "any"
  | Ast.Type.Infer -> "infer"
  | Ast.Type.VarType id -> "'" ^ string_of_int id
  | Ast.Type.SymbolType { value } -> value
  | Ast.Type.ArrayType { element_type } ->
      "[" ^ string_of_type element_type ^ "]"
  | Ast.Type.TupleType (types, rest) -> (
      let base = String.concat ", " (List.map string_of_type types) in
      match rest with
      | None -> "(" ^ base ^ ")"
      | Some t -> "(" ^ base ^ ", " ^ string_of_type t ^ " ...)")
  | Ast.Type.FunctionType (params, ret) ->
      let params_str = String.concat " * " (List.map string_of_type params) in
      Printf.sprintf "(%s -> %s)" params_str (string_of_type ret)
  | Ast.Type.RecordType fields ->
      let field_strs =
        List.map (fun (name, ty) -> name ^ ": " ^ string_of_type ty) fields
      in
      "record { " ^ String.concat "; " field_strs ^ " }"
  | _ -> failwith "Unknown"

let rec type_eq expected actual =
  match (expected, actual) with
  | Ast.Type.SymbolType { value = "*" }, Ast.Type.SymbolType _ -> true
  | Ast.Type.SymbolType { value = "unit" }, _ -> true
  | Ast.Type.Any, _ | _, Ast.Type.Any -> true
  | Ast.Type.SymbolType { value = v1 }, Ast.Type.SymbolType { value = v2 } ->
      v1 = v2
  | ( Ast.Type.ArrayType { element_type = e1 },
      Ast.Type.ArrayType { element_type = e2 } ) ->
      type_eq e1 e2
  | Ast.Type.TupleType (xs, rest1), Ast.Type.TupleType (ys, rest2) -> (
      List.length xs = List.length ys
      && List.for_all2 type_eq xs ys
      &&
      match (rest1, rest2) with
      | None, None -> true
      | Some r1, Some r2 -> type_eq r1 r2
      | _ -> false)
  | Ast.Type.VarType _, _ | _, Ast.Type.VarType _ -> true
  | Ast.Type.SymbolType { value = "*" }, _ -> true
  | _, Ast.Type.SymbolType { value = "*" } -> true
  | _ -> false
