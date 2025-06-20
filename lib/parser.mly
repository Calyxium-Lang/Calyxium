%right Pow
%left Star Slash Mod
%left Plus Minus
%left Carot
%right Assign PlusAssign MinusAssign StarAssign SlashAssign
%nonassoc Inc Dec

%token Function If Else Var Switch Case Default For Import New Return Class True False 
%token Plus Minus Star Slash Mod Pow LParen RParen LBracket RBracket LBrace RBrace Dot Colon Carot Semi Comma Not Greater Less 
%token Implies MapsTo LogicalOr LogicalAnd Eq Neq Geq Leq Dec Inc IntType FloatType StringType ByteType BoolType UnitType Assign PlusAssign MinusAssign StarAssign SlashAssign
%token <string> Ident
%token <int64> Int
%token <float> Float
%token <string> String
%token <char> Byte
%token <bool> Bool
%token <unit> Unit
%token EOF

%start program
%type <Ast.Stmt.t> program

%%

program:
    stmt_list EOF { Ast.Stmt.BlockStmt { body = $1 } }

stmt_list:
    stmt stmt_list { $1 :: $2 }
  | stmt { [$1] }

stmt:
    simple_stmt { $1 }
  | compound_stmt { $1 }
  | expr Assign expr %prec Assign { Ast.Stmt.ExprStmt (Ast.Expr.BinaryExpr { left = $1; operator = Token.Assign; right = $3 }) }
  | expr { Ast.Stmt.ExprStmt $1 }

simple_stmt:
    VarDeclStmt { $1 }
  | NewVarDeclStmt { $1 }
  | ImportStmt { $1 }

compound_stmt:
    FunctionDeclStmt { $1 }
  | ReturnStmt { $1 }
  | IfStmt { $1 }
  | ForStmt { $1 }
  | ClassDeclStmt { $1 }
  | SwitchStmt { $1 }

ReturnStmt:
    Return expr { Ast.Stmt.ReturnStmt $2 }

IfStmt:
    If LParen expr RParen LBrace stmt_list RBrace Else LBrace stmt_list RBrace {
      Ast.Stmt.IfStmt { condition = $3; then_branch = Ast.Stmt.BlockStmt { body = $6 }; else_branch = Some (Ast.Stmt.BlockStmt { body = $10 }) }
    }
  | If LParen expr RParen LBrace stmt_list RBrace {
      Ast.Stmt.IfStmt { condition = $3; then_branch = Ast.Stmt.BlockStmt { body = $6 }; else_branch = None }
    }

ForStmt:
    For LParen stmt_opt Semi expr_opt Semi stmt_opt RParen LBrace stmt_list RBrace {
      let default_condition = Ast.Expr.VarExpr "true" in
      let increment_stmt = match $7 with
        | None -> (match $3 with
          | Some (Ast.Stmt.VarDeclarationStmt { identifier; _ }) -> 
              Some (Ast.Stmt.ExprStmt (Ast.Expr.UnaryExpr { operator = Token.Inc; operand = Ast.Expr.VarExpr identifier }))
          | Some (Ast.Stmt.ExprStmt (Ast.Expr.VarExpr var_name)) -> 
              Some (Ast.Stmt.ExprStmt (Ast.Expr.UnaryExpr { operator = Token.Inc; operand = Ast.Expr.VarExpr var_name }))
          | _ -> None)
        | Some (Ast.Stmt.ExprStmt expr) -> Some (Ast.Stmt.ExprStmt expr)
        | Some _ -> None
      in
      Ast.Stmt.ForStmt { init = $3; condition = Option.value ~default:default_condition $5; increment = increment_stmt; body = Ast.Stmt.BlockStmt { body = $10 } }
    }

stmt_opt:
    stmt { Some $1 }
  | { None }

expr_opt:
    expr { Some $1 }
  | { None }

SwitchStmt:
    Switch expr LBrace case_list default_opt RBrace {
      Ast.Stmt.SwitchStmt { expr = $2; cases = $4; default_case = $5 }
    }

case_list:
    Case expr Colon stmt_list case_list { ($2, $4) :: $5 }
  | Case expr Colon stmt_list { [($2, $4)] }

default_opt:
    Default Colon stmt_list { Some $3 }
  | { None }

FunctionDeclStmt:
    Ident LParen parameter_list RParen Colon type_expr LBrace stmt_list RBrace { Ast.Stmt.FunctionDeclStmt { name = $1; parameters = $3; return_type = Some $6; body = $8 } }
    | Ident LParen parameter_list RParen Colon type_expr Implies stmt_list { Ast.Stmt.FunctionDeclStmt { name = $1; parameters= $3; return_type = Some $6; body = $8 } }

parameter_list:
    parameter Comma parameter_list { $1 :: $3 }
  | parameter { [$1] }
  | { [] }

parameter:
    Ident Colon type_expr { { Ast.Stmt.name = $1; param_type = $3 } }

type_expr:
    IntType { Ast.Type.SymbolType { value = "int" } }
  | FloatType { Ast.Type.SymbolType { value = "float" } }
  | StringType { Ast.Type.SymbolType { value = "string" } }
  | ByteType { Ast.Type.SymbolType { value = "byte" } }
  | BoolType { Ast.Type.SymbolType { value = "bool" } }
  | UnitType { Ast.Type.SymbolType { value = "unit"} }
  | LBracket RBracket type_expr { Ast.Type.ArrayType { element_type = $3 } }

expr:
    True { Ast.Expr.BoolExpr { value = true } }
  | False { Ast.Expr.BoolExpr { value = false } }
  | New Ident { Ast.Expr.NewExpr { class_name = $2; arguments = [] } }
  | expr Dot Ident LParen argument_list RParen { Ast.Expr.CallExpr { callee = Ast.Expr.PropertyAccessExpr { object_name = $1; property_name = $3 }; arguments = $5 } }
  | expr Dot Ident { Ast.Expr.PropertyAccessExpr { object_name = $1; property_name = $3 } }
  | expr Plus expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.Plus; right = $3 } }
  | expr Carot expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.Carot; right = $3 } }
  | expr Minus expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.Minus; right = $3 } }
  | expr Star expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.Star; right = $3 } }
  | expr Slash expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.Slash; right = $3 } }
  | expr Mod expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.Mod; right = $3 } }
  | expr Pow expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.Pow; right = $3 } }
  | expr Greater expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.Greater; right = $3 } }
  | expr Less expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.Less; right = $3 } }
  | expr LogicalOr expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.LogicalOr; right = $3 } }
  | expr LogicalAnd expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.LogicalAnd; right = $3 } }
  | expr Eq expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.Eq; right = $3 } }
  | expr Neq expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.Neq; right = $3 } }
  | expr Geq expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.Geq; right = $3 } }
  | expr Leq expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.Leq; right = $3 } }
  | expr PlusAssign expr %prec PlusAssign { Ast.Expr.BinaryExpr { left = $1; operator = Token.PlusAssign; right = $3 } }
  | expr MinusAssign expr %prec MinusAssign { Ast.Expr.BinaryExpr { left = $1; operator = Token.MinusAssign; right = $3 } }
  | expr StarAssign expr %prec StarAssign { Ast.Expr.BinaryExpr { left = $1; operator = Token.StarAssign; right = $3 } }
  | expr SlashAssign expr %prec SlashAssign { Ast.Expr.BinaryExpr { left = $1; operator = Token.SlashAssign; right = $3 } }
  | Not expr { Ast.Expr.UnaryExpr { operator = Token.Not; operand = $2 } }
  | expr Inc { Ast.Expr.UnaryExpr { operator = Token.Inc; operand = $1 } }
  | expr Dec { Ast.Expr.UnaryExpr { operator = Token.Dec; operand = $1 } }
  | Ident LParen argument_list RParen { Ast.Expr.CallExpr { callee = Ast.Expr.VarExpr $1; arguments = $3 } }
  | Minus Int { Ast.Expr.IntExpr { value = Int64.neg $2 } }
  | Minus Float { Ast.Expr.FloatExpr { value = -. $2 } }
  | Int { Ast.Expr.IntExpr { value = $1 } }
  | Float { Ast.Expr.FloatExpr { value = $1 } }
  | String { Ast.Expr.StringExpr { value = $1 } }
  | Byte { Ast.Expr.ByteExpr { value = $1 } }
  | Bool { Ast.Expr.BoolExpr { value = $1 } }
  | Unit { Ast.Expr.UnitExpr { value = $1 } }
  | Ident { Ast.Expr.VarExpr $1 }
  | LBrace RBrace { Ast.Expr.ArrayExpr { elements = [] } }
  | LBrace expr_list RBrace { Ast.Expr.ArrayExpr { elements = $2 } }
  | Ident LBracket expr RBracket { Ast.Expr.IndexExpr { array = Ast.Expr.VarExpr $1; index = $3 } }

expr_list:
    expr Comma expr_list { $1 :: $3 }
  | expr { [$1] }

argument_list:
    expr Comma argument_list { $1 :: $3 }
  | expr { [$1] }
  | { [] }

ClassDeclStmt:
    Class Ident LBrace class_body RBrace { Ast.Stmt.ClassDeclStmt { name = $2; properties = fst $4; methods = snd $4 } }

class_body:
    class_member class_body { (fst $1 @ fst $2, snd $1 @ snd $2) }
  | class_member { (fst $1, snd $1) }

class_member:
    Var Ident Colon type_expr Semi { ([{ Ast.Stmt.name = $2; param_type = $4 }], []) }
  | FunctionDeclStmt { ([], [$1]) }

ImportStmt:
    Import String { Ast.Stmt.ImportStmt { module_name = $2 } }

VarDeclStmt:
    Var Ident Colon type_expr Assign expr { Ast.Stmt.VarDeclarationStmt { identifier = $2; constant = true; assigned_value = Some $6; explicit_type = $4 } }

NewVarDeclStmt:
    Var Ident Assign New Ident { Ast.Stmt.NewVarDeclarationStmt { identifier = $2; constant = true; assigned_value = Some (Ast.Expr.NewExpr { class_name = $5; arguments = [] }); arguments = [] } }
