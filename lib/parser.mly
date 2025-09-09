%right PlusAssign MinusAssign StarAssign SlashAssign
%right BitWiseANDAssign BitWiseORAssign BitWiseXORAssign
%right LeftShiftAssign RightShiftAssign
%right Assign
%left LogicalOr
%left LogicalAnd
%left BitWiseOR
%left BitWiseXOR
%left BitWiseAND
%nonassoc Eq Neq Geq Leq Greater Less
%left LeftShift RightShift
%left Pipeline
%right Lambda
%left Plus Minus
%left Star Slash Mod
%right Pow Carot ArrConcat
%nonassoc UnaryMinus NotPrec
%left Dot
%nonassoc Inc Dec
%right Question
%nonassoc IfThenElse
%nonassoc LowPrec

%token Recursive If Then Else Let Match With Use Module True False In Type Lambda IntType FloatType StringType ByteType BoolType UnitType
%token Eq Neq Geq Leq LogicalOr LogicalAnd Pow Dec Inc MapsTo PlusAssign MinusAssign StarAssign SlashAssign Pipeline Range
%token BitWiseOR BitWiseAND BitWiseXOR BitWiseNOT
%token LeftShift RightShift
%token BitWiseANDAssign BitWiseORAssign BitWiseXORAssign
%token LeftShiftAssign RightShiftAssign
%token Plus Minus Star Slash Mod Carot ArrConcat Assign Greater Less LParen RParen LBracket RBracket LBrace RBrace Dot Colon Comma Not Pipe DeRef UnderScore Question
%token <string> Ident
%token <Bigint.t> Int
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
  | stmt stmt_list { $1 :: $2 }
  | stmt { [$1] }

stmt:
  | FunctionDeclStmt { $1 }
  | RecordStmt { $1 }
  | expr { Ast.Stmt.ExprStmt $1 }

case:
  | Pipe match_pattern MapsTo stmt_list { ($2, $4) }

case_list:
  | case case_list { $1 :: $2 }
  | case { [$1] }

match_pattern:
  | expr         { Some $1 }
  | UnderScore   { None }

parameter_list:
  | parameter Comma parameter_list { $1 :: $3 }
  | parameter { [$1] }

parameter:
  | Ident Colon type_expr { { Ast.Stmt.name = $1; param_type = $3 } }
  | Ident { { Ast.Stmt.name = $1; param_type = Ast.Type.Infer } }

type_expr:
  | IntType { Ast.Type.SymbolType { value = "int" } }
  | FloatType { Ast.Type.SymbolType { value = "float" } }
  | StringType { Ast.Type.SymbolType { value = "string" } }
  | ByteType { Ast.Type.SymbolType { value = "byte" } }
  | BoolType { Ast.Type.SymbolType { value = "bool" } }
  | UnitType { Ast.Type.SymbolType { value = "unit"} }
  | LBracket RBracket type_expr { Ast.Type.ArrayType { element_type = $3 } }
  | Ident { Ast.Type.SymbolType { value = $1 } }
  | LParen type_expr_list RParen { match $2 with | [single] -> single | multiple -> Ast.Type.TupleType (multiple, None) }

type_expr_list:
  | type_expr Comma type_expr_list { $1 :: $3 }
  | type_expr { [$1] }

expr:
  | assignment_expr { $1 }

base_expr:
  | if_expr { $1 }
  | match_expr { $1 }
  | let_in_expr { $1 }
  | lambda_expr { $1 }
  | module_expr { $1 }
  | enum_expr { $1 }
  | import_expr { $1 }
  | pipeline_expr { $1 }

if_expr:
  | If expr Then block_expr Else block_expr %prec IfThenElse { Ast.Expr.IfExpr { condition = $2; then_branch = $4; else_branch = Some $6 } }
  | If expr Then block_expr %prec IfThenElse { Ast.Expr.IfExpr { condition = $2; then_branch = $4; else_branch = None } }

match_expr:
  | Match expr With case_list %prec LowPrec { Ast.Expr.MatchExpr { expr = $2; cases = $4 } }

let_in_expr:
  | Let single_ident Colon type_expr Assign expr In %prec LowPrec { Ast.Expr.VarDeclExpr { identifier = $2; assigned_value = Some $6; explicit_type = $4 } }
  | Let ident_list Colon type_expr Assign expr_list In %prec LowPrec { Ast.Expr.MultiVarDeclExpr { identifier = $2; assigned_value = $6; explicit_type = $4 } }
  | Let single_ident Assign expr In %prec LowPrec { Ast.Expr.VarDeclExpr { identifier = $2; assigned_value = Some $4; explicit_type = Ast.Type.Infer } }
  | Let ident_list Assign expr_list In %prec LowPrec { Ast.Expr.MultiVarDeclExpr { identifier = $2; assigned_value = $4; explicit_type = Ast.Type.Infer } }

lambda_expr:
  | Lambda parameter_list MapsTo block_expr %prec Lambda { Ast.Expr.LambdaExpr { parameters = $2; body = $4 } }

module_expr:
  | Module Ident LBrace expr_list RBrace %prec LowPrec { Ast.Expr.ModuleExpr { module_name = $2; block = $4 } }

enum_expr:
  | Type Ident Assign LBrace enum_member_list RBrace %prec LowPrec { Ast.Expr.EnumExpr { name = $2; members = $5 } }

import_expr:
  | Use path %prec LowPrec { Ast.Expr.ImportExpr { module_name = $2 } }

conditional_expr:
  | base_expr { $1 }
  | base_expr Question expr Colon conditional_expr { Ast.Expr.TernaryExpr { cond = $1; onTrue = $3; onFalse = $5 } }

assignment_expr:
  | conditional_expr { $1 }
  | postfix_expr Assign assignment_expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.Assign; right = $3 } }
  | postfix_expr PlusAssign assignment_expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.PlusAssign; right = $3 } }
  | postfix_expr MinusAssign assignment_expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.MinusAssign; right = $3 } }
  | postfix_expr StarAssign assignment_expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.StarAssign; right = $3 } }
  | postfix_expr SlashAssign assignment_expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.SlashAssign; right = $3 } }
  | postfix_expr BitWiseANDAssign assignment_expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.BitWiseANDAssign; right = $3 } }
  | postfix_expr BitWiseORAssign assignment_expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.BitWiseORAssign; right = $3 } }
  | postfix_expr BitWiseXORAssign assignment_expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.BitWiseXORAssign; right = $3 } }
  | postfix_expr LeftShiftAssign assignment_expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.LeftShiftAssign; right = $3 } }
  | postfix_expr RightShiftAssign assignment_expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.RightShiftAssign; right = $3 } }

pipeline_expr:
  | logical_or_expr { $1 }
  | pipeline_expr Pipeline pipe_term { Ast.Expr.PipelineExpr { left = $1; right = $3 } }

pipe_term:
  | lambda_expr { $1 }
  | Ident { Ast.Expr.VarExpr $1 }

logical_or_expr:
  | logical_and_expr { $1 }
  | logical_or_expr LogicalOr logical_and_expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.LogicalOr; right = $3 } }

logical_and_expr:
  | bitwise_or_expr { $1 }
  | logical_and_expr LogicalAnd bitwise_or_expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.LogicalAnd; right = $3 } }

bitwise_or_expr:
  | bitwise_xor_expr { $1 }
  | bitwise_or_expr BitWiseOR bitwise_xor_expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.BitWiseOR; right = $3 } }

bitwise_xor_expr:
  | bitwise_and_expr { $1 }
  | bitwise_xor_expr BitWiseXOR bitwise_and_expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.BitWiseXOR; right = $3 } }

bitwise_and_expr:
  | equality_expr { $1 }
  | bitwise_and_expr BitWiseAND equality_expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.BitWiseAND; right = $3 } }

equality_expr:
  | relational_expr { $1 }
  | equality_expr Eq relational_expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.Eq; right = $3 } }
  | equality_expr Neq relational_expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.Neq; right = $3 } }
  | equality_expr Geq relational_expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.Geq; right = $3 } }
  | equality_expr Leq relational_expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.Leq; right = $3 } }
  | equality_expr Greater relational_expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.Greater; right = $3 } }
  | equality_expr Less relational_expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.Less; right = $3 } }

relational_expr:
  | shift_expr { $1 }
  | relational_expr LeftShift shift_expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.LeftShift; right = $3 } }
  | relational_expr RightShift shift_expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.RightShift; right = $3 } }

shift_expr:
  | additive_expr { $1 }

additive_expr:
  | multiplicative_expr { $1 }
  | additive_expr Plus multiplicative_expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.Plus; right = $3 } }
  | additive_expr Minus multiplicative_expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.Minus; right = $3 } }

multiplicative_expr:
  | power_expr { $1 }
  | multiplicative_expr Star power_expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.Star; right = $3 } }
  | multiplicative_expr Slash power_expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.Slash; right = $3 } }
  | multiplicative_expr Mod power_expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.Mod; right = $3 } }

power_expr:
  | unary_expr { $1 }
  | power_expr Pow unary_expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.Pow; right = $3 } }
  | power_expr Carot unary_expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.Carot; right = $3 } }
  | power_expr ArrConcat unary_expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.ArrConcat; right = $3 } }

unary_expr:
  | postfix_expr { $1 }
  | Minus unary_expr %prec UnaryMinus { Ast.Expr.UnaryExpr { operator = Token.Minus; operand = $2 } }
  | Not unary_expr %prec NotPrec { Ast.Expr.UnaryExpr { operator = Token.Not; operand = $2 } }
  | BitWiseNOT unary_expr %prec NotPrec { Ast.Expr.UnaryExpr { operator = Token.BitWiseNOT; operand = $2 } }
  | DeRef unary_expr %prec NotPrec { Ast.Expr.UnaryExpr { operator = Token.DeRef; operand = $2 } }

postfix_expr:
  | primary { $1 }
  | postfix_expr LParen argument_list RParen { Ast.Expr.CallExpr { callee = $1; arguments = $3 } }
  | postfix_expr Dot Ident { Ast.Expr.DotExpr { left = $1; right = $3 } }
  | postfix_expr LBracket expr RBracket { Ast.Expr.IndexExpr { array = $1; index = $3 } }
  | postfix_expr LBracket expr Colon expr RBracket { Ast.Expr.SliceExpr { array = $1; start = Some $3; end_ = Some $5 } }
  | postfix_expr LBracket expr Colon RBracket { Ast.Expr.SliceExpr { array = $1; start = Some $3; end_ = None } }
  | postfix_expr LBracket Colon expr RBracket { Ast.Expr.SliceExpr { array = $1; start = None; end_ = Some $4 } }
  | postfix_expr LBracket Colon RBracket { Ast.Expr.SliceExpr { array = $1; start = None; end_ = None } }
  | LBracket expr Range RBracket { Ast.Expr.RangeExpr { start = Some $2; end_ = None } }
  | LBracket expr Range expr RBracket { Ast.Expr.RangeExpr { start = Some $2; end_ = Some $4 } }
  | postfix_expr Inc { Ast.Expr.UnaryExpr { operator = Token.Inc; operand = $1 } }
  | postfix_expr Dec { Ast.Expr.UnaryExpr { operator = Token.Dec; operand = $1 } }

primary:
  | True { Ast.Expr.BoolExpr { value = true } }
  | False { Ast.Expr.BoolExpr { value = false } }
  | Int { Ast.Expr.IntExpr { value = $1 } }
  | Float { Ast.Expr.FloatExpr { value = $1 } }
  | String { Ast.Expr.StringExpr { value = $1 } }
  | Byte { Ast.Expr.ByteExpr { value = $1 } }
  | Bool { Ast.Expr.BoolExpr { value = $1 } }
  | Unit { Ast.Expr.UnitExpr { value = () } }
  | LParen RParen { Ast.Expr.UnitExpr { value = () } }
  | Ident { Ast.Expr.VarExpr $1 }
  | LBracket RBracket { Ast.Expr.ArrayExpr { elements = [] } }
  | LBracket expr_list RBracket { Ast.Expr.ArrayExpr { elements = $2 } }
  | LParen expr RParen { $2 }
  | LParen expr Comma expr_list RParen { Ast.Expr.TupleExpr ($2 :: $4) }

block_expr:
  | expr { $1 }
  | LBrace stmt_list RBrace { Ast.Expr.BlockExpr { body = $2 } }

expr_list:
  | expr { [$1] }
  | expr_list Comma expr { $1 @ [$3] }

argument_list:
  | /* empty */ { [] }
  | expr { [$1] }
  | argument_list Comma expr { $1 @ [$3] }

single_ident:
  | Ident { $1 }
  | UnderScore { "_" }

ident_list:
  | single_ident Comma single_ident { [$1; $3] }
  | ident_list Comma single_ident { $1 @ [$3] }

path:
  | path Dot Ident { $1 @ [$3] }
  | Ident { [$1] }

enum_member_list:
  | Ident Comma enum_member_list { $1 :: $3 }
  | Ident { [$1] }

FunctionDeclStmt:
  | Let Ident LParen parameter_list RParen Colon type_expr LBrace stmt_list RBrace { Ast.Stmt.FunctionDeclStmt { name = $2; is_rec = false; parameters = $4; return_type = $7; body = $9 } }
  | Let Recursive Ident LParen parameter_list RParen Colon type_expr LBrace stmt_list RBrace { Ast.Stmt.FunctionDeclStmt { name = $3; is_rec = true; parameters = $5; return_type = $8; body = $10 } }
  | Let Ident LParen RParen Colon type_expr LBrace stmt_list RBrace { Ast.Stmt.FunctionDeclStmt { name = $2; is_rec = false; parameters = []; return_type = $6; body = $8 } }
  | Let Ident LParen parameter_list RParen LBrace stmt_list RBrace { Ast.Stmt.FunctionDeclStmt { name = $2; is_rec = false; parameters = $4; return_type = Ast.Type.Infer; body = $7 } }
  | Let Recursive Ident LParen parameter_list RParen LBrace stmt_list RBrace { Ast.Stmt.FunctionDeclStmt { name = $3; is_rec = true; parameters = $5; return_type = Ast.Type.Infer; body = $8 } }
  | Let Ident LParen RParen LBrace stmt_list RBrace { Ast.Stmt.FunctionDeclStmt { name = $2; is_rec = false; parameters = []; return_type = Ast.Type.Infer; body = $6 } }
  | Let Recursive Ident LParen RParen LBrace stmt_list RBrace { Ast.Stmt.FunctionDeclStmt { name = $3; is_rec = true; parameters = []; return_type = Ast.Type.Infer; body = $7 } }

RecordStmt:
  | Type Ident Assign LBrace expr_list RBrace { Ast.Stmt.RecordStmt { name = $2; fields = $5; } }