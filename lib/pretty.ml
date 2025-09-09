let indent_level = ref 0
let indent () = String.make (!indent_level * 4) ' '

let token_to_string = function
  | Token.Eq -> "=="
  | Token.Neq -> "!="
  | Token.Leq -> "<="
  | Token.Geq -> ">="
  | Token.LogicalAnd -> "&&"
  | Token.LogicalOr -> "||"
  | Token.Pow -> "**"
  | Token.Inc -> "++"
  | Token.Dec -> "--"
  | Token.MapsTo -> "->"
  | Token.PlusAssign -> "+="
  | Token.MinusAssign -> "-="
  | Token.StarAssign -> "*="
  | Token.SlashAssign -> "/="
  | Token.BitWiseANDAssign -> "&="
  | Token.BitWiseORAssign -> "$="
  | Token.BitWiseXORAssign -> "`="
  | Token.Pipeline -> "|>"
  | Token.LeftShiftAssign -> "<<="
  | Token.RightShiftAssign -> ">>="
  | Token.Plus -> "+"
  | Token.Minus -> "-"
  | Token.Star -> "*"
  | Token.Slash -> "/"
  | Token.Mod -> "%"
  | Token.Carot -> "^"
  | Token.Assign -> "="
  | Token.Less -> "<"
  | Token.Greater -> ">"
  | Token.LParen -> "("
  | Token.RParen -> ")"
  | Token.RBracket -> "]"
  | Token.LBracket -> "["
  | Token.LBrace -> "{"
  | Token.RBrace -> "}"
  | Token.Dot -> "."
  | Token.Colon -> ":"
  | Token.Semi -> ";"
  | Token.Comma -> ","
  | Token.Not -> "not"
  | Token.Pipe -> "|"
  | Token.UnderScore -> "_"
  | Token.Question -> "?"
  | Token.BitWiseOR -> "lor"
  | Token.BitWiseAND -> "land"
  | Token.BitWiseNOT -> "lnot"
  | Token.BitWiseXOR -> "lxor"
  | Token.LeftShift -> "<<"
  | Token.RightShift -> ">>"
  | Token.ArrConcat -> "@"
  | Token.DeRef -> "!"
  | _ -> "<unknown>"

let rec string_of_type t =
  match t with
  | Ast.Type.SymbolType { value } -> value
  | Ast.Type.TupleType (ts, rest) ->
      let ts_str = String.concat ", " (List.map string_of_type ts) in
      let rest_str =
        match rest with
        | Some r ->
            if ts = [] then "..." ^ string_of_type r
            else ", ..." ^ string_of_type r
        | None -> ""
      in
      "(" ^ ts_str ^ rest_str ^ ")"
  | Ast.Type.FunctionType (params, ret) ->
      let params_str = String.concat ", " (List.map string_of_type params) in
      Printf.sprintf "(%s) -> %s" params_str (string_of_type ret)
  | Ast.Type.ArrayType { element_type } -> "[]" ^ string_of_type element_type
  | Ast.Type.Infer -> ""
  | _ -> "<type>"

let string_of_parameter (param : Ast.Stmt.parameter) =
  Printf.sprintf "%s: %s" param.Ast.Stmt.name
    (string_of_type param.Ast.Stmt.param_type)

let rec string_of_expr ?(top_level = true) = function
  | Ast.Expr.IntExpr { value } -> Bigint.to_string value
  | Ast.Expr.FloatExpr { value } -> string_of_float value
  | Ast.Expr.StringExpr { value } ->
      Printf.sprintf "\"%s\"" (String.escaped value)
  | Ast.Expr.BoolExpr { value } -> string_of_bool value
  | Ast.Expr.ByteExpr { value } -> Printf.sprintf "'%c'" value
  | Ast.Expr.VarExpr name -> name
  | Ast.Expr.BinaryExpr { left; operator; right } ->
      Printf.sprintf "%s %s %s"
        (string_of_expr ~top_level:false left)
        (token_to_string operator)
        (string_of_expr ~top_level:false right)
  | Ast.Expr.IfExpr { condition; then_branch; else_branch } ->
      let cond_str = string_of_expr ~top_level:false condition in
      let then_str = string_of_expr ~top_level:false then_branch in
      let else_str =
        match else_branch with
        | Some e -> " else { " ^ string_of_expr ~top_level:false e ^ " }"
        | None -> ""
      in
      (if top_level then indent () else "")
      ^ "if " ^ cond_str ^ then_str ^ else_str
  | Ast.Expr.MatchExpr { expr; cases } ->
      let expr_str = string_of_expr ~top_level:false expr in
      let case_strs =
        List.map
          (fun (pat_opt, body) ->
            let pat_str =
              match pat_opt with
              | Some p -> string_of_expr ~top_level:false p
              | None -> "_"
            in
            let body_str =
              match body with
              | [ Ast.Stmt.ExprStmt e ] ->
                  " -> " ^ string_of_expr ~top_level:false e
              | _ -> " -> <complex block>"
            in
            indent () ^ "| " ^ pat_str ^ body_str)
          cases
      in
      Printf.sprintf "match %s with\n%s" expr_str (String.concat "\n" case_strs)
  | Ast.Expr.ArrayExpr { elements } ->
      if elements = [] then "[]"
      else
        "[ "
        ^ (elements
          |> List.map (string_of_expr ~top_level:false)
          |> String.concat ", ")
        ^ " ]"
  | Ast.Expr.CallExpr { callee; arguments } ->
      let callee_str = string_of_expr ~top_level:false callee in
      let args_str =
        arguments
        |> List.map (fun arg ->
               String.trim (string_of_expr ~top_level:false arg))
        |> String.concat ", "
      in
      (if top_level then indent () else "")
      ^ Printf.sprintf "%s(%s)" callee_str args_str
  | Ast.Expr.UnaryExpr { operator; operand } ->
      Printf.sprintf "%s%s" (token_to_string operator)
        (string_of_expr ~top_level:false operand)
  | Ast.Expr.VarDeclExpr { identifier; explicit_type; assigned_value } ->
      let type_str =
        match explicit_type with
        | Ast.Type.Infer -> ""
        | ty -> ": " ^ string_of_type ty
      in
      let value_str =
        match assigned_value with
        | Some v -> " = " ^ string_of_expr ~top_level:false v
        | None -> ""
      in
      indent () ^ Printf.sprintf "let %s%s%s in" identifier type_str value_str
  | Ast.Expr.EnumExpr { name; members } ->
      let members_str = String.concat ", " members in
      String.concat "\n"
        [
          indent () ^ Printf.sprintf "type %s [" name;
          indent () ^ "    " ^ members_str;
          indent () ^ "]";
        ]
  | Ast.Expr.UnitExpr _ -> "()"
  | Ast.Expr.IndexExpr { array; index } ->
      Printf.sprintf "%s[%s]"
        (string_of_expr ~top_level:false array)
        (string_of_expr ~top_level:false index)
  | Ast.Expr.DotExpr { left; right } ->
      Printf.sprintf "%s.%s" (string_of_expr ~top_level:false left) right
  | Ast.Expr.TernaryExpr { cond; onTrue; onFalse } ->
      Printf.sprintf "(%s) ? %s : %s"
        (string_of_expr ~top_level:false cond)
        (string_of_expr ~top_level:false onTrue)
        (string_of_expr ~top_level:false onFalse)
  | Ast.Expr.TupleExpr elements ->
      "("
      ^ (elements
        |> List.map (string_of_expr ~top_level:false)
        |> String.concat ", ")
      ^ ")"
  | Ast.Expr.PipelineExpr { left; right } ->
      Printf.sprintf "%s |> %s"
        (string_of_expr ~top_level:false left)
        (string_of_expr ~top_level:false right)
  | Ast.Expr.RangeExpr { start; end_ } ->
      let start_str =
        match start with
        | Some s -> string_of_expr ~top_level:false s
        | None -> ""
      in
      let end_str =
        match end_ with
        | Some e -> string_of_expr ~top_level:false e
        | None -> ""
      in
      Printf.sprintf "%s..%s" start_str end_str
  | Ast.Expr.BlockExpr { body } ->
      let header = indent () ^ "{" in
      incr indent_level;
      let body_str = body |> List.map string_of_stmt |> String.concat "\n" in
      decr indent_level;
      String.concat "\n" [ header; body_str; indent () ^ "}" ]
  | Ast.Expr.SliceExpr { array; start; end_ } ->
      let start_str =
        match start with
        | Some s -> string_of_expr ~top_level:false s
        | None -> ""
      in
      let end_str =
        match end_ with
        | Some e -> string_of_expr ~top_level:false e
        | None -> ""
      in
      Printf.sprintf "%s[%s:%s]"
        (string_of_expr ~top_level:false array)
        start_str end_str
  | Ast.Expr.MultiVarDeclExpr { identifier; explicit_type; assigned_value } ->
      let id_str = String.concat ", " identifier in
      let type_str =
        match explicit_type with
        | Ast.Type.Infer -> ""
        | ty -> ": " ^ string_of_type ty
      in
      let value_str =
        if assigned_value <> [] then
          " = "
          ^ String.concat ", "
              (List.map (string_of_expr ~top_level:false) assigned_value)
        else ""
      in
      indent () ^ Printf.sprintf "let %s%s%s in" id_str type_str value_str
  | Ast.Expr.ImportExpr { module_name } ->
      Printf.sprintf "use %s" (String.concat "." module_name)
  | Ast.Expr.ModuleExpr { module_name; block } ->
      let header = indent () ^ Printf.sprintf "module %s {" module_name in
      incr indent_level;
      let body_str =
        block
        |> List.map (string_of_expr ~top_level:false)
        |> String.concat "\n"
      in
      decr indent_level;
      String.concat "\n" [ header; body_str; indent () ^ "}" ]
  | _ -> failwith ""

and string_of_stmt = function
  | Ast.Stmt.ExprStmt expr -> string_of_expr expr
  | Ast.Stmt.FunctionDeclStmt { name; is_rec; parameters; return_type; body } ->
      let rec_keyword = if is_rec then "rec " else "" in
      let params_str =
        parameters |> List.map string_of_parameter |> String.concat ", "
      in
      let header =
        Printf.sprintf "let %s%s(%s): %s {" rec_keyword name params_str
          (string_of_type return_type)
      in

      incr indent_level;
      let body_str = body |> List.map string_of_stmt |> String.concat "\n" in
      decr indent_level;

      let footer = indent () ^ "}" in
      String.concat "\n" [ indent () ^ header; body_str; footer ]
  | Ast.Stmt.BlockStmt { body } ->
      let header = indent () ^ "{" in
      incr indent_level;
      let body_str = body |> List.map string_of_stmt |> String.concat "\n" in
      decr indent_level;
      let footer = indent () ^ "}" in
      String.concat "\n" [ header; body_str; footer ]
  | _ -> raise (Failure "Unsupported statement type")

let string_of_program (stmts : Ast.Stmt.t list) : string =
  stmts |> List.map string_of_stmt |> String.concat "\n\n"
