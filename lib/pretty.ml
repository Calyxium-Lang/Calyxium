open Ast

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
  | Token.Not -> "!"
  | Token.Pipe -> "|"
  | Token.UnderScore -> "_"
  | Token.Question -> "?"
  | Token.BitWiseOR -> "`"
  | Token.BitWiseAND -> "&"
  | Token.BitWiseNOT -> "~"
  | Token.BitWiseXOR -> "$"
  | Token.LeftShift -> "<<"
  | Token.RightShift -> ">>"
  | _ -> "<unknown>"

let rec string_of_type t =
  match t with
  | Type.SymbolType { value } -> value
  | Type.TupleType ts ->
      "(" ^ String.concat ", " (List.map string_of_type ts) ^ ")"
  | Type.FunctionType (params, ret) ->
      let params_str = String.concat ", " (List.map string_of_type params) in
      Printf.sprintf "(%s) -> %s" params_str (string_of_type ret)
  | Type.ArrayType { element_type } -> "[]" ^ string_of_type element_type
  | _ -> "<type>"

let string_of_parameter (param : Stmt.parameter) =
  Printf.sprintf "%s: %s" param.name (string_of_type param.param_type)

let rec string_of_expr = function
  | Expr.IntExpr { value } -> Z.to_string value
  | Expr.FloatExpr { value } -> string_of_float value
  | Expr.StringExpr { value } ->
      let escaped = String.escaped value in
      Printf.sprintf "\"%s\"" escaped
  | Expr.BoolExpr { value } -> string_of_bool value
  | Expr.ByteExpr { value } -> Printf.sprintf "'%c'" value
  | Expr.VarExpr name -> name
  | Expr.BinaryExpr { left; operator; right } ->
      Printf.sprintf "%s %s %s" (string_of_expr left) (token_to_string operator)
        (string_of_expr right)
  | Expr.IfExpr { condition; then_branch; else_branch } ->
      let cond_str = string_of_expr condition in
      let format_then expr =
        match expr with
        | Expr.IfExpr _ | Expr.MatchExpr _ ->
            incr indent_level;
            let s = "\n" ^ indent () ^ string_of_expr expr in
            decr indent_level;
            "then" ^ s
        | _ -> "then " ^ string_of_expr expr
      in
      let format_else expr =
        match expr with
        | Expr.IfExpr _ | Expr.MatchExpr _ ->
            incr indent_level;
            let s = "\n" ^ indent () ^ string_of_expr expr in
            decr indent_level;
            "\n" ^ indent () ^ "else" ^ s
        | _ -> " else " ^ string_of_expr expr
      in
      Printf.sprintf "if %s %s%s" cond_str (format_then then_branch)
        (match else_branch with Some expr -> format_else expr | None -> "")
  | Expr.MatchExpr { expr; cases } ->
      let expr_str = string_of_expr expr in
      let case_strs =
        List.map
          (fun (pat_opt, body) ->
            let pat_str =
              match pat_opt with Some p -> string_of_expr p | None -> "_"
            in
            let body_str =
              match body with
              | [ Stmt.ExprStmt expr ] -> " -> " ^ string_of_expr expr
              | _ -> " -> <complex block>"
            in
            indent () ^ "| " ^ pat_str ^ body_str)
          cases
      in
      Printf.sprintf "match %s with\n%s" expr_str (String.concat "\n" case_strs)
  | Expr.ArrayExpr { elements } ->
      let elems = elements |> List.map string_of_expr |> String.concat ", " in
      "{ " ^ elems ^ " }"
  | Expr.CallExpr { callee; arguments } ->
      let callee_str = string_of_expr callee in
      let args_str =
        arguments |> List.map string_of_expr |> String.concat ", "
      in
      Printf.sprintf "%s(%s)" callee_str args_str
  | Expr.UnaryExpr { operator; operand } ->
      let op_str = token_to_string operator in
      let operand_str = string_of_expr operand in
      Printf.sprintf "%s%s" op_str operand_str
  | _ -> raise (Failure "Unsupported expression type")

let rec string_of_stmt = function
  | Stmt.ExprStmt expr -> indent () ^ string_of_expr expr
  | Stmt.VarDeclarationStmt { identifier; explicit_type; assigned_value } ->
      let type_str = string_of_type explicit_type in
      let value_str =
        match assigned_value with
        | Some v -> " = " ^ string_of_expr v
        | None -> ""
      in
      indent () ^ Printf.sprintf "let %s: %s%s" identifier type_str value_str
  | Stmt.FunctionDeclStmt { name; is_rec; parameters; return_type; body } ->
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

      let footer = "}" in
      String.concat "\n" [ indent () ^ header; body_str; indent () ^ footer ]
  | Stmt.BlockStmt { body } ->
      let header = indent () ^ "{" in
      incr indent_level;
      let body_str = body |> List.map string_of_stmt |> String.concat "\n" in
      decr indent_level;
      let footer = indent () ^ "}" in
      String.concat "\n" [ header; body_str; footer ]
  | Stmt.EnumStmt { name; members } ->
      let members_str = String.concat ", " members in
      let header = Printf.sprintf "enum %s {" name in
      let body = indent () ^ "    " ^ members_str in
      let footer = indent () ^ "}" in
      String.concat "\n" [ indent () ^ header; body; footer ]
  | _ -> raise (Failure "Unsupported statement type")

let string_of_program (stmts : Stmt.t list) : string =
  stmts |> List.map string_of_stmt |> String.concat "\n\n"
