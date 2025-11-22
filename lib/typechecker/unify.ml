open Calyxium_parser

let bind_var subst var_id ty =
  let ty = Subst.apply subst ty in
  if ty = Ast.Type.VarType var_id then ()
  else if Types.occurs_check subst var_id ty then
    raise
      (Types.UnifyError
         ("Occurs check failed: cannot construct infinite type for "
         ^ Types.string_of_type (Ast.Type.VarType var_id)))
  else Subst.add subst var_id ty

let rec unify_with_subst subst t1 t2 =
  let t1 = Subst.apply subst t1 in
  let t2 = Subst.apply subst t2 in
  match (t1, t2) with
  | Ast.Type.Any, _ | _, Ast.Type.Any -> ()
  | Ast.Type.VarType id, t | t, Ast.Type.VarType id -> bind_var subst id t
  | Ast.Type.SymbolType { value = v1 }, Ast.Type.SymbolType { value = v2 }
    when v1 = v2 ->
      ()
  | ( Ast.Type.ArrayType { element_type = e1 },
      Ast.Type.ArrayType { element_type = e2 } ) ->
      unify_with_subst subst e1 e2
  | Ast.Type.TupleType (xs, rest1), Ast.Type.TupleType (ys, rest2) -> (
      match (xs, ys) with
      | xh :: xt, yh :: yt ->
          unify_with_subst subst xh yh;
          unify_with_subst subst
            (Ast.Type.TupleType (xt, rest1))
            (Ast.Type.TupleType (yt, rest2))
      | [], _ -> (
          match rest1 with
          | Some r1 ->
              unify_with_subst subst r1 (Ast.Type.TupleType (ys, rest2))
          | None ->
              if ys = [] then match rest2 with Some _ -> () | None -> ()
              else raise (Types.UnifyError "Tuple arity mismatch"))
      | _, [] -> (
          match rest2 with
          | Some r2 ->
              unify_with_subst subst (Ast.Type.TupleType (xs, rest1)) r2
          | None ->
              if xs = [] then match rest1 with Some _ -> () | None -> ()
              else raise (Types.UnifyError "Tuple arity mismatch")))
  | Ast.Type.FunctionType (ps1, r1), Ast.Type.FunctionType (ps2, r2)
    when List.length ps1 = List.length ps2 ->
      List.iter2 (unify_with_subst subst) ps1 ps2;
      unify_with_subst subst r1 r2
  | Ast.Type.RecordType fs1, Ast.Type.RecordType fs2 ->
      let names1 = List.map fst fs1 in
      let names2 = List.map fst fs2 in
      if List.sort_uniq compare names1 <> List.sort_uniq compare names2 then
        raise
          (Types.UnifyError "Cannot unify record types with different fields")
      else
        List.iter
          (fun name ->
            let t1 = List.assoc name fs1 in
            let t2 = List.assoc name fs2 in
            unify_with_subst subst t1 t2)
          names1
  | _, Ast.Type.Infer -> ()
  | Ast.Type.SymbolType { value = "*" }, _ -> ()
  | _, Ast.Type.SymbolType { value = "*" } -> ()
  | _ ->
      raise
        (Types.UnifyError
           ("Cannot unify " ^ Types.string_of_type t1 ^ " with "
          ^ Types.string_of_type t2))

let unify t1 t2 =
  let s = Subst.empty () in
  try unify_with_subst s t1 t2
  with Types.UnifyError msg ->
    raise (Types.TypeError ("Unification error: " ^ msg))

let rec ftv_type = function
  | Ast.Type.VarType id -> [ id ]
  | Ast.Type.ArrayType { element_type } -> ftv_type element_type
  | Ast.Type.TupleType (ts, rest) -> (
      let ftvs = List.concat_map ftv_type ts in
      match rest with None -> ftvs | Some t -> ftvs @ ftv_type t)
  | Ast.Type.FunctionType (ps, r) -> List.concat_map ftv_type (r :: ps)
  | Ast.Type.RecordType fs -> List.concat_map (fun (_, t) -> ftv_type t) fs
  | _ -> []

let ftv_env env =
  let vars = List.concat_map (fun (_, t) -> ftv_type t) env in
  List.sort_uniq compare vars

let generalize env t =
  let ftv_t = ftv_type t in
  let ftv_e = ftv_env env in
  let quant = List.filter (fun v -> not (List.mem v ftv_e)) ftv_t in
  (quant, t)

let instantiate_scheme (quantified_vars, tp) =
  let map_tbl = Hashtbl.create (List.length quantified_vars) in
  let fresh_for id =
    try Hashtbl.find map_tbl id
    with Not_found ->
      let v = Types.fresh_tyvar () in
      Hashtbl.add map_tbl id v;
      v
  in
  let rec inst t =
    match t with
    | Ast.Type.VarType id when List.mem id quantified_vars -> fresh_for id
    | Ast.Type.VarType _ -> t
    | Ast.Type.ArrayType { element_type } ->
        Ast.Type.ArrayType { element_type = inst element_type }
    | Ast.Type.TupleType (ts, rest) ->
        let ts' = List.map inst ts in
        let rest' = Option.map inst rest in
        Ast.Type.TupleType (ts', rest')
    | Ast.Type.FunctionType (ps, r) ->
        Ast.Type.FunctionType (List.map inst ps, inst r)
    | Ast.Type.RecordType fs ->
        Ast.Type.RecordType (List.map (fun (n, t) -> (n, inst t)) fs)
    | other -> other
  in
  inst tp

let rec can_compare t1 t2 =
  match (t1, t2) with
  | Ast.Type.Any, _ -> true
  | _, Ast.Type.Any -> true
  | Ast.Type.SymbolType { value = v1 }, Ast.Type.SymbolType { value = v2 } ->
      v1 = v2
  | Ast.Type.TupleType (ts1, rest1), Ast.Type.TupleType (ts2, rest2) -> (
      List.length ts1 = List.length ts2
      && List.for_all2 can_compare ts1 ts2
      &&
      match (rest1, rest2) with
      | None, None -> true
      | Some r1, Some r2 -> can_compare r1 r2
      | _ -> false)
  | ( Ast.Type.ArrayType { element_type = et1 },
      Ast.Type.ArrayType { element_type = et2 } ) ->
      can_compare et1 et2
  | Ast.Type.VarType _, _ | _, Ast.Type.VarType _ -> true
  | Ast.Type.SymbolType { value = "*" }, _ -> true
  | _, Ast.Type.SymbolType { value = "*" } -> true
  | _ -> false

let rec fold_left_map2 f env idents exprs =
  match (idents, exprs) with
  | [], [] -> (env, [])
  | ident :: id_rest, expr :: expr_rest ->
      let env', inferred = f env ident expr in
      let env'', rest = fold_left_map2 f env' id_rest expr_rest in
      (env'', inferred :: rest)
  | _ -> failwith "fold_left_map2: lists have different lengths"
