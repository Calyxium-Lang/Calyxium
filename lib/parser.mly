%{
  open Ast
%}

%right Assign PlusAssign MinusAssign StarAssign SlashAssign
%left LogicalOr
%left LogicalAnd
%nonassoc Eq Neq Geq Leq Greater Less
%left Plus Minus
%left Star Slash Mod
%right Pow Carot
%nonassoc UnaryMinus NotPrec
%left Dot
%nonassoc Inc Dec
%nonassoc IfThenElse
%nonassoc LowPrec   

%token Recursive If Then Else Let Match With Return For Use Module True False Int64Type FloatType StringType ByteType BoolType UnitType TupleType
%token Eq Neq Geq Leq LogicalOr LogicalAnd Pow Dec Inc MapsTo PlusAssign MinusAssign StarAssign SlashAssign
%token Plus Minus Star Slash Mod Carot Assign Greater Less LParen RParen LBracket RBracket LBrace RBrace Dot Colon Semi Comma Not Pipe UnderScore Question
%token <string> Ident
%token <int64> Int64
%token <float> Float
%token <string> String
%token <char> Byte
%token <bool> Bool
%token <unit> Unit
%token <int> Binary
%token EOF

%start program
%type <Stmt.t> program

%%

program:
    stmt_list EOF { Stmt.BlockStmt { body = $1 } }

stmt_list:
  stmt stmt_list { $1 :: $2 }
  | stmt { [$1] }

stmt:
  | VarDeclStmt { $1 }
  | ImportStmt { $1 }
  | FunctionDeclStmt { $1 }
  | IfStmt { $1 }
  | ForStmt { $1 }
  | MatchStmt { $1 }
  | ModuleStmt { $1 }
  | expr { Stmt.ExprStmt $1 }
  | Return expr %prec LowPrec { Stmt.ExprStmt (Expr.ReturnExpr $2) }
  | If expr Then expr Else expr %prec IfThenElse { Stmt.ExprStmt (Expr.IfExpr { condition = $2; then_branch = $4; else_branch = $6; }) }

stmt_opt:
  | stmt { Some $1 }
  | { None }

expr_opt:
  | expr { Some $1 }
  | { None }

case_list:
  | Pipe match_pattern MapsTo stmt_list case_list { ($2, $4) :: $5 }
  | Pipe match_pattern MapsTo stmt_list { [($2, $4)] }

match_pattern:
  | expr         { Some $1 }
  | UnderScore   { None }

parameter_list:
  | parameter Comma parameter_list { $1 :: $3 }
  | parameter { [$1] }

parameter:
  | Ident Colon type_expr { { Stmt.name = $1; param_type = $3 } }

type_expr:
  | Int64Type { Type.SymbolType { value = "int" } }
  | FloatType { Type.SymbolType { value = "float" } }
  | StringType { Type.SymbolType { value = "string" } }
  | ByteType { Type.SymbolType { value = "byte" } }
  | BoolType { Type.SymbolType { value = "bool" } }
  | UnitType { Type.SymbolType { value = "unit"} }
  | TupleType { Type.SymbolType { value = "tuple" } }
  | LBracket RBracket type_expr { Type.ArrayType { element_type = $3 } }

expr:
  | True { Expr.BoolExpr { value = true } }
  | False { Expr.BoolExpr { value = false } }
  | expr Plus expr { Expr.BinaryExpr { left = $1; operator = Token.Plus; right = $3 } }
  | expr Carot expr { Expr.BinaryExpr { left = $1; operator = Token.Carot; right = $3 } }
  | expr Minus expr { Expr.BinaryExpr { left = $1; operator = Token.Minus; right = $3 } }
  | expr Star expr { Expr.BinaryExpr { left = $1; operator = Token.Star; right = $3 } }
  | expr Slash expr { Expr.BinaryExpr { left = $1; operator = Token.Slash; right = $3 } }
  | expr Mod expr { Expr.BinaryExpr { left = $1; operator = Token.Mod; right = $3 } }
  | expr Pow expr { Expr.BinaryExpr { left = $1; operator = Token.Pow; right = $3 } }
  | expr Greater expr { Expr.BinaryExpr { left = $1; operator = Token.Greater; right = $3 } }
  | expr Less expr { Expr.BinaryExpr { left = $1; operator = Token.Less; right = $3 } }
  | expr LogicalOr expr { Expr.BinaryExpr { left = $1; operator = Token.LogicalOr; right = $3 } }
  | expr LogicalAnd expr { Expr.BinaryExpr { left = $1; operator = Token.LogicalAnd; right = $3 } }
  | expr Eq expr { Expr.BinaryExpr { left = $1; operator = Token.Eq; right = $3 } }
  | expr Neq expr { Expr.BinaryExpr { left = $1; operator = Token.Neq; right = $3 } }
  | expr Geq expr { Expr.BinaryExpr { left = $1; operator = Token.Geq; right = $3 } }
  | expr Leq expr { Expr.BinaryExpr { left = $1; operator = Token.Leq; right = $3 } }
  | expr PlusAssign expr { Expr.BinaryExpr { left = $1; operator = Token.PlusAssign; right = $3 } }
  | expr MinusAssign expr { Expr.BinaryExpr { left = $1; operator = Token.MinusAssign; right = $3 } }
  | expr StarAssign expr { Expr.BinaryExpr { left = $1; operator = Token.StarAssign; right = $3 } }
  | expr SlashAssign expr { Expr.BinaryExpr { left = $1; operator = Token.SlashAssign; right = $3 } }
  | Not expr %prec NotPrec { Expr.UnaryExpr { operator = Token.Not; operand = $2 } }
  | expr Inc { Expr.UnaryExpr { operator = Token.Inc; operand = $1 } }
  | expr Dec { Expr.UnaryExpr { operator = Token.Dec; operand = $1 } }
  | Ident LParen argument_list RParen { Expr.CallExpr { callee = Expr.VarExpr $1; arguments = $3 } }
  | Minus expr %prec UnaryMinus { Expr.UnaryExpr { operator = Token.Minus; operand = $2 } }
  | Int64 { Expr.Int64Expr { value = $1 } }
  | Float { Expr.FloatExpr { value = $1 } }
  | String { Expr.StringExpr { value = $1 } }
  | Byte { Expr.ByteExpr { value = $1 } }
  | Bool { Expr.BoolExpr { value = $1 } }
  | Unit { Expr.UnitExpr { value = $1 } }
  | Ident { Expr.VarExpr $1 }
  | Binary { Expr.BinaryLitExpr { value = $1 } }
  | LBrace RBrace { Expr.ArrayExpr { elements = [] } }
  | LBrace expr_list RBrace { Expr.ArrayExpr { elements = $2 } }
  | expr LBracket expr RBracket { Expr.IndexExpr { array = $1; index = $3 } }
  | expr Dot Ident { Expr.DotExpr { left = $1; right = $3 } }
  | LParen expr RParen Question expr Colon expr { Expr.TernaryExpr { cond = $2; onTrue = $5; onFalse = $7; } }
  | LParen expr_list RParen { match $2 with | [single] -> single | multiple -> Expr.TupleExpr multiple }
  | LParen expr RParen { $2 }

expr_list:
  | expr Comma expr_list { $1 :: $3 }
  | expr { [$1] }

argument_list:
  | /* empty */ { [] }
  | expr Comma argument_list { $1 :: $3 }
  | expr { [$1] }

ident_list:
  | Ident Comma ident_list { $1 :: $3 }
  | Ident { [$1] }

ImportStmt:
  | Use Ident { Stmt.ImportStmt { module_name = $2 } }

ModuleStmt:
  | Module Ident LBrace stmt_list RBrace { Stmt.ModuleStmt { module_name = $2; block = $4 } }

VarDeclStmt:
  | Let Ident Colon type_expr Assign expr { Stmt.VarDeclarationStmt { identifier = $2; assigned_value = Some $6; explicit_type = $4 } }
  | Let ident_list Colon type_expr Assign expr { Stmt.MultiVarDeclarationStmt { identifier = $2; assigned_value = $6; explicit_type = $4 } }
  
FunctionDeclStmt:
  | Ident LParen parameter_list RParen Colon type_expr LBrace stmt_list RBrace { Stmt.FunctionDeclStmt { name = $1; is_rec = false; parameters = $3; return_type = $6; body = $8 } }
  | Recursive Ident LParen parameter_list RParen Colon type_expr LBrace stmt_list RBrace { Stmt.FunctionDeclStmt { name = $2; is_rec = true; parameters = $4; return_type = $7; body = $9 } }
  | Ident LParen RParen Colon type_expr LBrace stmt_list RBrace { Stmt.FunctionDeclStmt { name = $1; is_rec = false; parameters = []; return_type = $5; body = $7 } }
  
MatchStmt:
  | Match expr With case_list { Stmt.MatchStmt { expr = $2; cases = $4; } }

IfStmt:
  | If LParen expr RParen LBrace stmt_list RBrace Else LBrace stmt_list RBrace { Stmt.IfStmt { condition = $3; then_branch = Stmt.BlockStmt { body = $6 }; else_branch = Some (Stmt.BlockStmt { body = $10 }) } }
  | If LParen expr RParen LBrace stmt_list RBrace { Stmt.IfStmt { condition = $3; then_branch = Stmt.BlockStmt { body = $6 }; else_branch = None } }

ForStmt:
  | For LParen stmt_opt Semi expr_opt Semi stmt_opt RParen LBrace stmt_list RBrace { let default_condition = Expr.BoolExpr { value = true } in let increment_stmt = match $7 with | None -> (match $3 with | Some (Stmt.VarDeclarationStmt { identifier; _ }) -> Some (Stmt.ExprStmt (Expr.UnaryExpr { operator = Token.Inc; operand = Expr.VarExpr identifier })) | Some (Stmt.ExprStmt (Expr.VarExpr var_name)) -> Some (Stmt.ExprStmt (Expr.UnaryExpr { operator = Token.Inc; operand = Expr.VarExpr var_name })) | _ -> None) | Some (Stmt.ExprStmt expr) -> Some (Stmt.ExprStmt expr) | Some _ -> None in Stmt.ForStmt { init = $3; condition = Option.value ~default:default_condition $5; increment = increment_stmt; body = Stmt.BlockStmt { body = $10 } } }