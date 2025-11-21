module Env = Map.Make (String)
module FuncEnv = Map.Make (String)

let option_exists f = function Some x -> f x | None -> false

let fold_binary l op r =
  match (l, op, r) with
  | Ir.IR_Int a, Token.Plus, Ir.IR_Int b -> Ir.IR_Int (Bigint.add a b)
  | Ir.IR_Int a, Token.Minus, Ir.IR_Int b -> Ir.IR_Int (Bigint.sub a b)
  | Ir.IR_Int a, Token.Star, Ir.IR_Int b -> Ir.IR_Int (Bigint.mul a b)
  | Ir.IR_Int a, Token.Slash, Ir.IR_Int b when not (Bigint.equal b Bigint.zero)
    ->
      Ir.IR_Int (Bigint.div a b)
  | Ir.IR_Float a, Token.Plus, Ir.IR_Float b -> Ir.IR_Float (a +. b)
  | Ir.IR_Float a, Token.Minus, Ir.IR_Float b -> Ir.IR_Float (a -. b)
  | Ir.IR_Float a, Token.Star, Ir.IR_Float b -> Ir.IR_Float (a *. b)
  | Ir.IR_Float a, Token.Slash, Ir.IR_Float b when b <> 0.0 ->
      Ir.IR_Float (a /. b)
  | Ir.IR_Bool a, Token.LogicalAnd, Ir.IR_Bool b -> Ir.IR_Bool (a && b)
  | Ir.IR_Bool a, Token.LogicalOr, Ir.IR_Bool b -> Ir.IR_Bool (a || b)
  | Ir.IR_String a, Token.Carot, Ir.IR_String b -> Ir.IR_String (a ^ b)
  | Ir.IR_Int a, Token.Greater, Ir.IR_Int b ->
      Ir.IR_Bool (Bigint.compare a b > 0)
  | Ir.IR_Int a, Token.Less, Ir.IR_Int b -> Ir.IR_Bool (Bigint.compare a b < 0)
  | Ir.IR_Int a, Token.Eq, Ir.IR_Int b -> Ir.IR_Bool (Bigint.equal a b)
  | Ir.IR_Int a, Token.Neq, Ir.IR_Int b -> Ir.IR_Bool (not (Bigint.equal a b))
  | Ir.IR_Float a, Token.Greater, Ir.IR_Float b -> Ir.IR_Bool (a > b)
  | Ir.IR_Float a, Token.Less, Ir.IR_Float b -> Ir.IR_Bool (a < b)
  | Ir.IR_Float a, Token.Eq, Ir.IR_Float b -> Ir.IR_Bool (a = b)
  | Ir.IR_Float a, Token.Neq, Ir.IR_Float b -> Ir.IR_Bool (a <> b)
  | _ -> Ir.IR_Binary (l, op, r)

let rec fold_expr env = function
  | Ir.IR_Int _ as i -> (i, env)
  | Ir.IR_Float _ as f -> (f, env)
  | Ir.IR_String _ as s -> (s, env)
  | Ir.IR_Byte _ as c -> (c, env)
  | Ir.IR_Bool _ as b -> (b, env)
  | Ir.IR_Unit as u -> (u, env)
  | Ir.IR_Var name -> (
      match Env.find_opt name env with
      | Some v -> (v, env)
      | None -> (Ir.IR_Var name, env))
  | Ir.IR_Binary (l, op, r) ->
      let l', _ = fold_expr env l in
      let r', _ = fold_expr env r in
      (fold_binary l' op r', env)
  | Ir.IR_Unary (op, e) -> (
      let e', _ = fold_expr env e in
      match (op, e') with
      | Token.Not, Ir.IR_Bool b -> (Ir.IR_Bool (not b), env)
      | _ -> (Ir.IR_Unary (op, e'), env))
  | Ir.IR_VarDecl var -> (
      match var.Ir.value with
      | Some v ->
          let v', _ = fold_expr env v in
          let env' =
            match v' with
            | Ir.IR_Int _ | Ir.IR_Float _ | Ir.IR_Bool _ | Ir.IR_String _
            | Ir.IR_Byte _ ->
                Env.add var.Ir.var_name v' env
            | _ -> env
          in
          (Ir.IR_VarDecl { var with Ir.value = Some v' }, env')
      | None -> (Ir.IR_VarDecl var, env))
  | Ir.IR_MultiVarDecl { names; value; typ } ->
      let value', env' = fold_expr env value in
      (Ir.IR_MultiVarDecl { names; value = value'; typ }, env')
  | Ir.IR_If (c, t, e) -> (
      let c', _ = fold_expr env c in
      match c' with
      | Ir.IR_Bool true -> fold_expr env t
      | Ir.IR_Bool false -> (
          match e with
          | Some e_branch -> fold_expr env e_branch
          | None -> (Ir.IR_Unit, env))
      | _ ->
          let t', _ = fold_expr env t in
          let e' = Option.map (fun e -> fst (fold_expr env e)) e in
          (Ir.IR_If (c', t', e'), env))
  | Ir.IR_Tuple xs ->
      let xs', _ = List.split (List.map (fun e -> fold_expr env e) xs) in
      (Ir.IR_Tuple xs', env)
  | Ir.IR_Array xs ->
      let xs', _ = List.split (List.map (fun e -> fold_expr env e) xs) in
      (Ir.IR_Array xs', env)
  | Ir.IR_Call (callee, args) ->
      let callee', _ = fold_expr env callee in
      let args', _ = List.split (List.map (fold_expr env) args) in
      (Ir.IR_Call (callee', args'), env)
  | Ir.IR_Index (arr, idx) ->
      let arr', _ = fold_expr env arr in
      let idx', _ = fold_expr env idx in
      (Ir.IR_Index (arr', idx'), env)
  | Ir.IR_Slice (arr, s, e) ->
      let arr', _ = fold_expr env arr in
      let s' = Option.map (fun x -> fst (fold_expr env x)) s in
      let e' = Option.map (fun x -> fst (fold_expr env x)) e in
      (Ir.IR_Slice (arr', s', e'), env)
  | Ir.IR_BlockExpr instrs ->
      let instrs', _ = fold_block env instrs in
      (Ir.IR_BlockExpr instrs', env)
  | Ir.IR_Lambda { parameters; body } ->
      let body', _ = fold_block Env.empty body in
      (Ir.IR_Lambda { parameters; body = body' }, env)
  | Ir.IR_Match (scrutinee, cases) ->
      let scr', _ = fold_expr env scrutinee in
      let cases' =
        List.map
          (fun (pat_opt, body) ->
            let pat' = Option.map (fun x -> fst (fold_expr env x)) pat_opt in
            let body', _ = fold_block env body in
            (pat', body'))
          cases
      in
      (Ir.IR_Match (scr', cases'), env)
  | Ir.IR_Dot (lhs, field) ->
      let lhs', _ = fold_expr env lhs in
      (Ir.IR_Dot (lhs', field), env)
  | Ir.IR_Ternary (c, t, e) -> fold_expr env (Ir.IR_If (c, t, Some e))
  | other -> (other, env)

and fold_instr env = function
  | Ir.IR_Expr e ->
      let e', env' = fold_expr env e in
      (Ir.IR_Expr e', env')
  | Ir.IR_Block instrs ->
      let instrs', env' = fold_block env instrs in
      (Ir.IR_Block instrs', env')
  | Ir.IR_FuncDecl f ->
      let body', _ = fold_block Env.empty f.Ir.body in
      (Ir.IR_FuncDecl { f with Ir.body = body' }, env)
  | other -> (other, env)

and fold_block env instrs =
  let rec aux env acc = function
    | [] -> (List.rev acc, env)
    | instr :: rest -> (
        match fold_instr env instr with
        | ( Ir.IR_Expr (Ir.IR_VarDecl { Ir.var_name; Ir.value = Some v; Ir.typ }),
            env' ) -> (
            match v with
            | Ir.IR_Int _ | Ir.IR_Float _ | Ir.IR_Bool _ | Ir.IR_String _
            | Ir.IR_Byte _ ->
                let used = List.exists (fun i -> uses_var var_name i) rest in
                if not used then aux env' acc rest
                else if count_var_uses var_name rest = 1 then
                  let rest' = List.map (subst_var var_name v) rest in
                  aux env' acc rest'
                else
                  aux env'
                    (Ir.IR_Expr
                       (Ir.IR_VarDecl { Ir.var_name; Ir.value = Some v; Ir.typ })
                    :: acc)
                    rest
            | _ ->
                aux env'
                  (Ir.IR_Expr
                     (Ir.IR_VarDecl { Ir.var_name; Ir.value = Some v; Ir.typ })
                  :: acc)
                  rest)
        | folded_instr, env' -> aux env' (folded_instr :: acc) rest)
  in
  aux env [] instrs

and uses_var name instr =
  let rec go_expr = function
    | Ir.IR_Var v -> v = name
    | Ir.IR_Binary (l, _, r) -> go_expr l || go_expr r
    | Ir.IR_Unary (_, e) -> go_expr e
    | Ir.IR_Call (f, args) -> go_expr f || List.exists go_expr args
    | Ir.IR_Array xs | Ir.IR_Tuple xs -> List.exists go_expr xs
    | Ir.IR_Index (a, i) -> go_expr a || go_expr i
    | Ir.IR_Slice (a, s, e) ->
        go_expr a || option_exists go_expr s || option_exists go_expr e
    | Ir.IR_If (c, t, e) -> go_expr c || go_expr t || option_exists go_expr e
    | Ir.IR_Ternary (c, t, e) -> go_expr c || go_expr t || go_expr e
    | Ir.IR_BlockExpr instrs -> List.exists (uses_var name) instrs
    | Ir.IR_Dot (e, _) -> go_expr e
    | _ -> false
  in
  match instr with
  | Ir.IR_Expr e -> go_expr e
  | Ir.IR_Block instrs -> List.exists (uses_var name) instrs
  | Ir.IR_FuncDecl f -> List.exists (uses_var name) f.Ir.body
  | _ -> false

and count_var_uses name instrs =
  List.fold_left (fun acc i -> acc + if uses_var name i then 1 else 0) 0 instrs

and subst_var name value instr =
  let rec go_expr = function
    | Ir.IR_Var v when v = name -> value
    | Ir.IR_Binary (l, op, r) -> Ir.IR_Binary (go_expr l, op, go_expr r)
    | Ir.IR_Unary (op, e) -> Ir.IR_Unary (op, go_expr e)
    | Ir.IR_Call (f, args) -> Ir.IR_Call (go_expr f, List.map go_expr args)
    | Ir.IR_Array xs -> Ir.IR_Array (List.map go_expr xs)
    | Ir.IR_Tuple xs -> Ir.IR_Tuple (List.map go_expr xs)
    | Ir.IR_Index (a, i) -> Ir.IR_Index (go_expr a, go_expr i)
    | Ir.IR_Slice (a, s, e) ->
        Ir.IR_Slice (go_expr a, Option.map go_expr s, Option.map go_expr e)
    | Ir.IR_If (c, t, e) -> Ir.IR_If (go_expr c, go_expr t, Option.map go_expr e)
    | Ir.IR_Ternary (c, t, e) -> Ir.IR_Ternary (go_expr c, go_expr t, go_expr e)
    | Ir.IR_BlockExpr instrs ->
        Ir.IR_BlockExpr (List.map (subst_var name value) instrs)
    | Ir.IR_Dot (e, f) -> Ir.IR_Dot (go_expr e, f)
    | e -> e
  in
  match instr with
  | Ir.IR_Expr e -> Ir.IR_Expr (go_expr e)
  | Ir.IR_Block instrs -> Ir.IR_Block (List.map (subst_var name value) instrs)
  | Ir.IR_FuncDecl f ->
      Ir.IR_FuncDecl
        { f with Ir.body = List.map (subst_var name value) f.Ir.body }
  | other -> other

let collect_funcs ir =
  let env, rest =
    List.fold_left
      (fun (env, acc) instr ->
        match instr with
        | Ir.IR_FuncDecl f ->
            let size = List.length f.Ir.body in
            if size <= 3 && not f.Ir.is_rec then
              (FuncEnv.add f.Ir.name f env, acc)
            else (env, instr :: acc)
        | other -> (env, other :: acc))
      (FuncEnv.empty, []) ir
  in
  (env, List.rev rest)

let subst_params params args body =
  List.fold_left2
    (fun acc param arg -> List.map (subst_var param.Ast.Stmt.name arg) acc)
    body params args

let rec inline_expr fenv = function
  | Ir.IR_Call (Ir.IR_Var fname, args) when FuncEnv.mem fname fenv ->
      let f = FuncEnv.find fname fenv in
      if List.length f.Ir.parameters = List.length args then
        let args' = List.map (inline_expr fenv) args in
        let body' = subst_params f.Ir.parameters args' f.Ir.body in
        match body' with
        | [ Ir.IR_Expr e ] -> inline_expr fenv e
        | instrs -> Ir.IR_BlockExpr instrs
      else Ir.IR_Call (Ir.IR_Var fname, args)
  | Ir.IR_Binary (l, op, r) ->
      Ir.IR_Binary (inline_expr fenv l, op, inline_expr fenv r)
  | Ir.IR_Unary (op, e) -> Ir.IR_Unary (op, inline_expr fenv e)
  | Ir.IR_If (c, t, e) ->
      Ir.IR_If
        (inline_expr fenv c, inline_expr fenv t, Option.map (inline_expr fenv) e)
  | Ir.IR_Tuple xs -> Ir.IR_Tuple (List.map (inline_expr fenv) xs)
  | Ir.IR_Array xs -> Ir.IR_Array (List.map (inline_expr fenv) xs)
  | Ir.IR_Index (a, i) -> Ir.IR_Index (inline_expr fenv a, inline_expr fenv i)
  | Ir.IR_Slice (a, s, e) ->
      Ir.IR_Slice
        ( inline_expr fenv a,
          Option.map (inline_expr fenv) s,
          Option.map (inline_expr fenv) e )
  | Ir.IR_BlockExpr instrs -> Ir.IR_BlockExpr (inline_block fenv instrs)
  | Ir.IR_Ternary (c, t, e) ->
      Ir.IR_Ternary (inline_expr fenv c, inline_expr fenv t, inline_expr fenv e)
  | Ir.IR_Dot (lhs, f) -> Ir.IR_Dot (inline_expr fenv lhs, f)
  | e -> e

and inline_instr fenv = function
  | Ir.IR_Expr e -> Ir.IR_Expr (inline_expr fenv e)
  | Ir.IR_Block instrs -> Ir.IR_Block (inline_block fenv instrs)
  | Ir.IR_FuncDecl f ->
      Ir.IR_FuncDecl { f with Ir.body = inline_block fenv f.Ir.body }
  | other -> other

and inline_block fenv instrs = List.map (inline_instr fenv) instrs

let inline_funcs ir =
  let fenv, ir' = collect_funcs ir in
  inline_block fenv ir'

let rec fold ir =
  let ir', _ = fold_block Env.empty ir in
  if ir' = ir then ir else fold ir'

let optimize ir = ir |> inline_funcs |> fold
