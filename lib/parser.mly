%{
  open Ast
%}

%right PlusAssign MinusAssign StarAssign SlashAssign
%left LogicalOr
%left LogicalAnd
%left BitWiseOR
%left BitWiseXOR
%left BitWiseAND
%nonassoc Eq Neq Geq Leq Greater Less
%left LeftShift RightShift RightShiftLogical
%left Pipeline
%left Plus Minus
%left Star Slash Mod
%right Pow Carot ArrConcat
%nonassoc UnaryMinus NotPrec
%left Dot
%nonassoc Inc Dec
%nonassoc IfThenElse
%nonassoc LowPrec

%token Recursive If Then Else Let Match With Use Module True False Enum Struct Int64Type FloatType StringType ByteType BoolType UnitType
%token Eq Neq Geq Leq LogicalOr LogicalAnd Pow Dec Inc MapsTo PlusAssign MinusAssign StarAssign SlashAssign Pipeline Range DoubleColon
%token BitWiseOR BitWiseAND BitWiseXOR BitWiseNOT
%token LeftShift RightShift RightShiftLogical
%token BitWiseANDAssign BitWiseORAssign BitWiseXORAssign
%token LeftShiftAssign RightShiftAssign
%token Plus Minus Star Slash Mod Carot ArrConcat Assign Greater Less LParen RParen LBracket RBracket LBrace RBrace Dot Colon Semi Comma Not Pipe UnderScore Question
%token <string> Ident
%token <int64> Int64
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
    stmt_list EOF { Stmt.BlockStmt { body = $1 } }

stmt_list:
  | stmt stmt_list { $1 :: $2 }
  | stmt { [$1] }

stmt:
  | VarDeclStmt { $1 }
  | ImportStmt { $1 }
  | FunctionDeclStmt { $1 }
  | ModuleStmt { $1 }
  | EnumStmt { $1 }
  | StructStmt { $1 }
  | expr { Stmt.ExprStmt $1 }

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
  | Ident { { Stmt.name = $1; param_type = Type.Infer } }

type_expr:
  | Int64Type { Type.SymbolType { value = "int" } }
  | FloatType { Type.SymbolType { value = "float" } }
  | StringType { Type.SymbolType { value = "string" } }
  | ByteType { Type.SymbolType { value = "byte" } }
  | BoolType { Type.SymbolType { value = "bool" } }
  | UnitType { Type.SymbolType { value = "unit"} }
  | LBracket RBracket type_expr { Type.ArrayType { element_type = $3 } }
  | Ident { Type.SymbolType { value = $1 } }
  | LParen type_expr_list RParen { match $2 with | [single] -> single | multiple -> Type.TupleType multiple }

type_expr_list:
  | type_expr Comma type_expr_list { $1 :: $3 }
  | type_expr { [$1] }

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
  | Unit { Expr.UnitExpr { value = () } }
  | LParen RParen { Expr.UnitExpr { value = () } }
  | Ident { Expr.VarExpr $1 }
  | LBracket RBracket { Expr.ArrayExpr { elements = [] } }
  | LBracket expr_list RBracket { Expr.ArrayExpr { elements = $2 } }
  | expr LBracket expr RBracket { Expr.IndexExpr { array = $1; index = $3 } }
  | expr Dot Ident { Expr.DotExpr { left = $1; right = $3 } }
  | LParen expr RParen Question expr Colon expr { Expr.TernaryExpr { cond = $2; onTrue = $5; onFalse = $7; } }
  | LParen expr_list RParen { match $2 with | [single] -> single | multiple -> Expr.TupleExpr multiple }
  | LParen expr RParen { $2 }
  | expr BitWiseOR expr { Expr.BinaryExpr { left = $1; operator = Token.BitWiseOR; right = $3 } }
  | expr BitWiseXOR expr { Expr.BinaryExpr { left = $1; operator = Token.BitWiseXOR; right = $3 } }
  | expr BitWiseAND expr { Expr.BinaryExpr { left = $1; operator = Token.BitWiseAND; right = $3 } }
  | expr LeftShift expr { Expr.BinaryExpr { left = $1; operator = Token.LeftShift; right = $3 } }
  | expr RightShift expr { Expr.BinaryExpr { left = $1; operator = Token.RightShift; right = $3 } }
  | expr RightShiftLogical expr { Expr.BinaryExpr { left = $1; operator = Token.RightShiftLogical; right = $3 } }
  | expr BitWiseANDAssign expr { Expr.BinaryExpr { left = $1; operator = Token.BitWiseANDAssign; right = $3 } }
  | expr BitWiseORAssign expr { Expr.BinaryExpr { left = $1; operator = Token.BitWiseORAssign; right = $3 } }
  | expr BitWiseXORAssign expr { Expr.BinaryExpr { left = $1; operator = Token.BitWiseXORAssign; right = $3 } }
  | expr LeftShiftAssign expr { Expr.BinaryExpr { left = $1; operator = Token.LeftShiftAssign; right = $3 } }
  | expr RightShiftAssign expr { Expr.BinaryExpr { left = $1; operator = Token.RightShiftAssign; right = $3 } }
  | BitWiseNOT expr %prec NotPrec { Expr.UnaryExpr { operator = Token.BitWiseNOT; operand = $2 } }
  | expr Pipeline expr { Expr.PipelineExpr { left = $1; right = $3 } }
  | Match expr With case_list { Expr.MatchExpr { expr = $2; cases = $4; } }
  | If expr Then block_expr Else block_expr { Expr.IfExpr { condition = $2; then_branch = $4; else_branch = Some $6; } }
  | If expr Then block_expr { Expr.IfExpr { condition = $2; then_branch = $4; else_branch = None; } }
  | LBracket expr Range RBracket { Expr.RangeExpr { start = Some $2; end_ = None } }
  | LBracket expr Range expr RBracket { Expr.RangeExpr { start = Some $2; end_ = Some $4 } }
  | expr ArrConcat expr { Expr.BinaryExpr { left = $1; operator = Token.ArrConcat; right = $3 } }
  | expr LBracket expr Colon expr RBracket { Expr.SliceExpr { array = $1; start = Some $3; end_ = Some $5 } }
  | expr LBracket expr Colon RBracket { Expr.SliceExpr { array = $1; start = Some $3; end_ = None } }
  | expr LBracket Colon expr RBracket { Expr.SliceExpr { array = $1; start = None; end_ = Some $4 } }
  | expr LBracket Colon RBracket { Expr.SliceExpr { array = $1; start = None; end_ = None } }

block_expr:
  | expr { $1 }
  | LBrace stmt_list RBrace { Expr.BlockExpr { body = $2 } }

expr_list:
  | expr Comma expr_list { $1 :: $3 }
  | expr { [$1] }

argument_list:
  | expr Comma argument_list { $1 :: $3 }
  | expr { [$1] }
  | /* empty */ { [] }

ident_list:
  | Ident Comma ident_list { $1 :: $3 }
  | Ident { [$1] }

path:
  | path Dot Ident { $1 @ [$3] }
  | Ident { [$1] }

enum_member_list:
  | Ident Comma enum_member_list { $1 :: $3 }
  | Ident { [$1] }

ImportStmt:
  | Use path { Stmt.ImportStmt { module_name = $2 } }

ModuleStmt:
  | Module Ident LBrace stmt_list RBrace { Stmt.ModuleStmt { module_name = $2; block = $4 } }

VarDeclStmt:
  | Let Ident Colon type_expr Assign expr { Stmt.VarDeclarationStmt { identifier = $2; assigned_value = Some $6; explicit_type = $4 } }
  | Let ident_list Colon type_expr Assign expr_list { Stmt.MultiVarDeclarationStmt { identifier = $2; assigned_value = $6; explicit_type = $4; } }
  | Let Ident Assign expr { Stmt.VarDeclarationStmt { identifier = $2; assigned_value = Some $4; explicit_type = Type.Infer } }
  | Let ident_list Assign expr_list { Stmt.MultiVarDeclarationStmt { identifier = $2; assigned_value = $4; explicit_type = Type.Infer } }
  
FunctionDeclStmt:
  | Let Ident LParen parameter_list RParen Colon type_expr LBrace stmt_list RBrace { Stmt.FunctionDeclStmt { name = $2; is_rec = false; parameters = $4; return_type = $7; body = $9 } }
  | Let Recursive Ident LParen parameter_list RParen Colon type_expr LBrace stmt_list RBrace { Stmt.FunctionDeclStmt { name = $3; is_rec = true; parameters = $5; return_type = $8; body = $10 } }
  | Let Ident LParen RParen Colon type_expr LBrace stmt_list RBrace { Stmt.FunctionDeclStmt { name = $2; is_rec = false; parameters = []; return_type = $6; body = $8 } }
  // | Let Ident LParen parameter_list RParen LBrace stmt_list RBrace { Stmt.FunctionDeclStmt { name = $2; is_rec = false; parameters = $4; return_type = Type.Infer; body = $7 } }
  // | Let Recursive Ident LParen parameter_list RParen LBrace stmt_list RBrace { Stmt.FunctionDeclStmt { name = $3; is_rec = true; parameters = $5; return_type = Type.Infer; body = $8 } }
  // | Let Ident LParen RParen LBrace stmt_list RBrace { Stmt.FunctionDeclStmt { name = $2; is_rec = false; parameters = []; return_type = Type.Infer; body = $6 } }

EnumStmt:
  | Enum Ident LBrace enum_member_list RBrace { Stmt.EnumStmt { name = $2; members = $4; }} 

StructStmt:
  | Struct Ident LBrace stmt_list RBrace { Stmt.StructStmt { name = $2; fields = $4; } }