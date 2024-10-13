%left Plus Minus Carot
%left Star Slash Mod
%left Pow

%token Function If Else Var Const Switch Case Default For Import Export New Null Return Class True False Plus Minus Star Slash Mod Pow LParen RParen LBracket RBracket LBrace RBrace Dot Colon Carot Semi Comma Not Greater Less LogicalOr LogicalAnd Eq Neq Geq Leq Dec Inc IntType FloatType StringType ByteType BoolType Assign PlusAssign MinusAssign StarAssign SlashAssign
%token <string> Ident
%token <int64> Int
%token <float> Float
%token <string> String
%token <char> Byte
%token <bool> Bool
%token EOF

%start program
%type <Ast.Stmt.t> program

%%

program:
    | stmt_list EOF { Ast.Stmt.BlockStmt { body = $1 } }

stmt_list:
    | stmt Semi stmt_list { $1 :: $3 }
    | stmt Semi { [$1] }

stmt:
    | VarDeclStmt { $1 }
    | NewVarDeclStmt { $1 }
    | FunctionDeclStmt { $1 }
    | ReturnStmt { $1 }
    | IfStmt { $1 }
    | ForStmt { $1 }
    | ClassDeclStmt { $1 }
    | ImportStmt { $1 }
    | ExportStmt { $1 }
    | SwitchStmt { $1 }
    | expr Assign expr { Ast.Stmt.ExprStmt (Ast.Expr.BinaryExpr { left = $1; operator = Ast.Assign; right = $3 }) }
    | expr { Ast.Stmt.ExprStmt $1 }

SwitchStmt:
    | Switch expr LBrace case_list default_opt RBrace {
        Ast.Stmt.SwitchStmt { expr = $2; cases = $4; default_case = $5 }
    }

case_list:
    | Case expr Colon stmt_list case_list { ($2, $4) :: $5 }
    | Case expr Colon stmt_list { [($2, $4)] }

default_opt:
    | Default Colon stmt_list { Some $3 }
    | { None }

stmt_opt:
    | stmt { Some $1 }
    | { None }

expr_opt:
    | expr { Some $1 }
    | { None }

parameter_list:
    | parameter Comma parameter_list { $1 :: $3 }
    | parameter { [$1] }
    | { [] }

parameter:
    | Ident Colon type_expr { { Ast.Stmt.name = $1; param_type = $3 } }

type_expr:
    | IntType { Ast.Type.SymbolType { value = "int" } }
    | FloatType { Ast.Type.SymbolType { value = "float" } }
    | StringType { Ast.Type.SymbolType { value = "string" } }
    | ByteType { Ast.Type.SymbolType { value = "byte" } }
    | BoolType { Ast.Type.SymbolType { value = "bool" } }
    | LBracket RBracket type_expr { Ast.Type.ArrayType { element_type = $3 } }

expr:
    | True { Ast.Expr.BoolExpr { value = true } }
    | False { Ast.Expr.BoolExpr { value = false } }
    | New Ident { Ast.Expr.NewExpr { class_name = $2; arguments = [] } }
    | expr Dot Ident LParen argument_list RParen { Ast.Expr.CallExpr { callee = Ast.Expr.PropertyAccessExpr { object_name = $1; property_name = $3 }; arguments = $5 } }
    | expr Dot Ident { Ast.Expr.PropertyAccessExpr { object_name = $1; property_name = $3 } }
    | expr Plus expr { Ast.Expr.BinaryExpr { left = $1; operator = Ast.Plus; right = $3 } }
    | expr Carot expr { Ast.Expr.BinaryExpr { left = $1; operator = Ast.Carot; right = $3 } }
    | expr Minus expr { Ast.Expr.BinaryExpr { left = $1; operator = Ast.Minus; right = $3 } }
    | expr Star expr { Ast.Expr.BinaryExpr { left = $1; operator = Ast.Star; right = $3 } }
    | expr Slash expr { Ast.Expr.BinaryExpr { left = $1; operator = Ast.Slash; right = $3 } }
    | expr Mod expr { Ast.Expr.BinaryExpr { left = $1; operator = Ast.Mod; right = $3 } }
    | expr Pow expr { Ast.Expr.BinaryExpr { left = $1; operator = Ast.Pow; right = $3 } }
    | expr Greater expr { Ast.Expr.BinaryExpr { left = $1; operator = Ast.Greater; right = $3 } }
    | expr Less expr { Ast.Expr.BinaryExpr { left = $1; operator = Ast.Less; right = $3 } }
    | expr LogicalOr expr { Ast.Expr.BinaryExpr { left = $1; operator = Ast.LogicalOr; right = $3 } }
    | expr LogicalAnd expr { Ast.Expr.BinaryExpr { left = $1; operator = Ast.LogicalAnd; right = $3 } }
    | expr Eq expr { Ast.Expr.BinaryExpr { left = $1; operator = Ast.Eq; right = $3 } }
    | expr Neq expr { Ast.Expr.BinaryExpr { left = $1; operator = Ast.Neq; right = $3 } }
    | expr Geq expr { Ast.Expr.BinaryExpr { left = $1; operator = Ast.Geq; right = $3 } }
    | expr Leq expr { Ast.Expr.BinaryExpr { left = $1; operator = Ast.Leq; right = $3 } }
    | expr PlusAssign expr { Ast.Expr.BinaryExpr { left = $1; operator = Ast.PlusAssign; right = $3 } }
    | expr MinusAssign expr { Ast.Expr.BinaryExpr { left = $1; operator = Ast.MinusAssign; right = $3 } }
    | expr StarAssign expr { Ast.Expr.BinaryExpr { left = $1; operator = Ast.StarAssign; right = $3 } }
    | expr SlashAssign expr { Ast.Expr.BinaryExpr { left = $1; operator = Ast.SlashAssign; right = $3 } }
    | Not expr { Ast.Expr.UnaryExpr { operator = Ast.Not; operand = $2 } }
    | Ident LParen argument_list RParen { Ast.Expr.CallExpr { callee = Ast.Expr.VarExpr $1; arguments = $3 } }
    | Null { Ast.Expr.NullExpr }
    | Minus Int { Ast.Expr.IntExpr { value = Int64.neg $2 } }
    | Minus Float { Ast.Expr.FloatExpr { value = -. $2 } }
    | Int { Ast.Expr.IntExpr { value = $1 } }
    | Float { Ast.Expr.FloatExpr { value = $1 } }
    | String { Ast.Expr.StringExpr { value = $1 } }
    | Byte { Ast.Expr.ByteExpr { value = $1 } }
    | Bool { Ast.Expr.BoolExpr { value = $1 } }
    | Ident { Ast.Expr.VarExpr $1 }
    | expr Inc { Ast.Expr.UnaryExpr { operator = Ast.Inc; operand = $1 } }
    | expr Dec { Ast.Expr.UnaryExpr { operator = Ast.Dec; operand = $1 } }
    | LBrace RBrace { Ast.Expr.ArrayExpr { elements = [] } }
    | LBrace expr_list RBrace { Ast.Expr.ArrayExpr { elements = $2 } }
    | Ident LBracket expr RBracket { Ast.Expr.IndexExpr { array = Ast.Expr.VarExpr $1; index = $3 } }

expr_list:
    | expr Comma expr_list { $1 :: $3 }
    | expr { [$1] }
    | { [] }

argument_list:
    | expr Comma argument_list { $1 :: $3 }
    | expr { [$1] }
    | { [] }

class_body:
    | class_member class_body { (fst $1 @ fst $2, snd $1 @ snd $2) }
    | class_member { (fst $1, snd $1) }

class_member:
    | Var Ident Colon type_expr Semi { ([{ Ast.Stmt.name = $2; param_type = $4 }], []) }
    | FunctionDeclStmt { ([], [$1]) }

ImportStmt:
    | Import String { Ast.Stmt.ImportStmt { module_name = $2 } }

ExportStmt:
    | Export Ident { Ast.Stmt.ExportStmt { identifier = $2 } }

ClassDeclStmt:
    | Class Ident LBrace class_body RBrace { Ast.Stmt.ClassDeclStmt { name = $2; properties = fst $4; methods = snd $4 } }

ForStmt:
    | For LParen stmt_opt Semi expr_opt Semi stmt_opt RParen LBrace stmt_list RBrace {
        let default_condition = Ast.Expr.VarExpr "true" in
        let increment_stmt = match $7 with
            | None -> (match $3 with
                | Some (Ast.Stmt.VarDeclarationStmt { identifier; _ }) -> 
                    Some (Ast.Stmt.ExprStmt (Ast.Expr.UnaryExpr { operator = Ast.Inc; operand = Ast.Expr.VarExpr identifier }))
                | Some (Ast.Stmt.ExprStmt (Ast.Expr.VarExpr var_name)) -> 
                    Some (Ast.Stmt.ExprStmt (Ast.Expr.UnaryExpr { operator = Ast.Inc; operand = Ast.Expr.VarExpr var_name }))
                | _ -> None)
            | Some (Ast.Stmt.ExprStmt expr) -> Some (Ast.Stmt.ExprStmt expr)
            | Some _ -> None in
        Ast.Stmt.ForStmt {  init = $3; condition = Option.value ~default:default_condition $5; increment = increment_stmt; body = Ast.Stmt.BlockStmt { body = $10 }; } }

IfStmt:
    | If LParen expr RParen LBrace stmt_list RBrace Else LBrace stmt_list RBrace {  Ast.Stmt.IfStmt { condition = $3; then_branch = Ast.Stmt.BlockStmt { body = $6 }; else_branch = Some (Ast.Stmt.BlockStmt { body = $10 }); } }
    | If LParen expr RParen LBrace stmt_list RBrace {  Ast.Stmt.IfStmt { condition = $3; then_branch = Ast.Stmt.BlockStmt { body = $6 }; else_branch = None; }  }

FunctionDeclStmt:
    | Function Ident LParen parameter_list RParen Colon type_expr LBrace stmt_list RBrace {  Ast.Stmt.FunctionDeclStmt { name = $2; parameters = $4; return_type = Some $7; body = $9; }  }
    | Function Ident LParen parameter_list RParen LBrace stmt_list RBrace {  Ast.Stmt.FunctionDeclStmt { name = $2; parameters = $4; return_type = None; body = $7; }  }

ReturnStmt:
    | Return expr { Ast.Stmt.ReturnStmt $2 }
    | Return { Ast.Stmt.ReturnStmt (Ast.Expr.VarExpr "") }

VarDeclStmt:
    | Var Ident Colon type_expr Assign expr {  Ast.Stmt.VarDeclarationStmt { identifier = $2; constant = false; assigned_value = Some $6; explicit_type = $4; }  }
    | Const Ident Colon type_expr Assign expr { Ast.Stmt.VarDeclarationStmt { identifier = $2; constant = true; assigned_value = Some $6; explicit_type = $4; } }

NewVarDeclStmt:
    | Var Ident Assign New Ident {  Ast.Stmt.NewVarDeclarationStmt { identifier = $2; constant = false; assigned_value = Some (Ast.Expr.NewExpr { class_name = $5; arguments = [] }); arguments = [] } }
    | Const Ident Assign New Ident { Ast.Stmt.NewVarDeclarationStmt { identifier = $2; constant = true; assigned_value = Some (Ast.Expr.NewExpr { class_name = $5; arguments = [] }); arguments = [] } }
