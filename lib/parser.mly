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
%right Question
%right Lambda
%nonassoc Inc Dec
%nonassoc IfThenElse
%nonassoc LowPrec

%token Recursive If Then Else Let Match With Use Module True False Type In Lambda IntType FloatType StringType ByteType BoolType UnitType
%token Eq Neq Geq Leq LogicalOr LogicalAnd Pow Dec Inc MapsTo PlusAssign MinusAssign StarAssign SlashAssign Pipeline Range Cons
%token BitWiseOR BitWiseAND BitWiseXOR BitWiseNOT
%token LeftShift RightShift RightShiftLogical
%token BitWiseANDAssign BitWiseORAssign BitWiseXORAssign
%token LeftShiftAssign RightShiftAssign
%token Plus Minus Star Slash Mod Carot ArrConcat Assign Greater Less LParen RParen LBracket RBracket LBrace RBrace Dot Colon Semi Comma Not Pipe DeRef UnderScore Question
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
  | LParen type_expr_list RParen { match $2 with | [single] -> single | multiple -> Ast.Type.TupleType multiple }

type_expr_list:
  | type_expr Comma type_expr_list { $1 :: $3 }
  | type_expr { [$1] }

expr:
  | True { Ast.Expr.BoolExpr { value = true } }
  | False { Ast.Expr.BoolExpr { value = false } }
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
  | expr PlusAssign expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.PlusAssign; right = $3 } }
  | expr MinusAssign expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.MinusAssign; right = $3 } }
  | expr StarAssign expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.StarAssign; right = $3 } }
  | expr SlashAssign expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.SlashAssign; right = $3 } }
  | Not expr %prec NotPrec { Ast.Expr.UnaryExpr { operator = Token.Not; operand = $2 } }
  | expr Inc { Ast.Expr.UnaryExpr { operator = Token.Inc; operand = $1 } }
  | expr Dec { Ast.Expr.UnaryExpr { operator = Token.Dec; operand = $1 } }
  | Ident LParen argument_list RParen { Ast.Expr.CallExpr { callee = Ast.Expr.VarExpr $1; arguments = $3 } }
  | Minus expr %prec UnaryMinus { Ast.Expr.UnaryExpr { operator = Token.Minus; operand = $2 } }
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
  | expr LBracket expr RBracket { Ast.Expr.IndexExpr { array = $1; index = $3 } }
  | expr Dot Ident { Ast.Expr.DotExpr { left = $1; right = $3 } }
  | expr Question expr Colon expr { Ast.Expr.TernaryExpr { cond = $1; onTrue = $3; onFalse = $5 } }
  | LParen expr_list RParen { match $2 with | [single] -> single | multiple -> Ast.Expr.TupleExpr multiple }
  | LParen expr RParen { $2 }
  | expr BitWiseOR expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.BitWiseOR; right = $3 } }
  | expr BitWiseXOR expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.BitWiseXOR; right = $3 } }
  | expr BitWiseAND expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.BitWiseAND; right = $3 } }
  | expr LeftShift expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.LeftShift; right = $3 } }
  | expr RightShift expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.RightShift; right = $3 } }
  | expr BitWiseANDAssign expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.BitWiseANDAssign; right = $3 } }
  | expr BitWiseORAssign expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.BitWiseORAssign; right = $3 } }
  | expr BitWiseXORAssign expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.BitWiseXORAssign; right = $3 } }
  | expr LeftShiftAssign expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.LeftShiftAssign; right = $3 } }
  | expr RightShiftAssign expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.RightShiftAssign; right = $3 } }
  | BitWiseNOT expr %prec NotPrec { Ast.Expr.UnaryExpr { operator = Token.BitWiseNOT; operand = $2 } }
  | expr Pipeline expr { Ast.Expr.PipelineExpr { left = $1; right = $3 } }
  | Match expr With case_list { Ast.Expr.MatchExpr { expr = $2; cases = $4; } }
  | If expr Then block_expr Else block_expr { Ast.Expr.IfExpr { condition = $2; then_branch = $4; else_branch = Some $6; } }
  | If expr Then block_expr { Ast.Expr.IfExpr { condition = $2; then_branch = $4; else_branch = None; } }
  | LBracket expr Range RBracket { Ast.Expr.RangeExpr { start = Some $2; end_ = None } }
  | LBracket expr Range expr RBracket { Ast.Expr.RangeExpr { start = Some $2; end_ = Some $4 } }
  | expr ArrConcat expr { Ast.Expr.BinaryExpr { left = $1; operator = Token.ArrConcat; right = $3 } }
  | expr LBracket expr Colon expr RBracket { Ast.Expr.SliceExpr { array = $1; start = Some $3; end_ = Some $5 } }
  | expr LBracket expr Colon RBracket { Ast.Expr.SliceExpr { array = $1; start = Some $3; end_ = None } }
  | expr LBracket Colon expr RBracket { Ast.Expr.SliceExpr { array = $1; start = None; end_ = Some $4 } }
  | expr LBracket Colon RBracket { Ast.Expr.SliceExpr { array = $1; start = None; end_ = None } }
  | Let Ident Colon type_expr Assign expr In { Ast.Expr.VarDeclExpr { identifier = $2; assigned_value = Some $6; explicit_type = $4 } }
  | Let ident_list Colon type_expr Assign expr_list In { Ast.Expr.MultiVarDeclExpr { identifier = $2; assigned_value = $6; explicit_type = $4; } }
  | Let Ident Assign expr In { Ast.Expr.VarDeclExpr { identifier = $2; assigned_value = Some $4; explicit_type = Ast.Type.Infer } }
  | Let ident_list Assign expr_list In { Ast.Expr.MultiVarDeclExpr { identifier = $2; assigned_value = $4; explicit_type = Ast.Type.Infer } }
  | Use path { Ast.Expr.ImportExpr { module_name = $2 } }
  | Module Ident LBrace expr_list RBrace { Ast.Expr.ModuleExpr { module_name = $2; block = $4 } }
  | Type Ident Assign LBrace enum_member_list RBrace { Ast.Expr.EnumExpr { name = $2; members = $5; }}
  | Lambda parameter_list MapsTo block_expr { Ast.Expr.LambdaExpr { parameters = $2; body = $4 } }

block_expr:
  | expr { $1 }
  | LBrace stmt_list RBrace { Ast.Expr.BlockExpr { body = $2 } }

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
  
FunctionDeclStmt:
  | Let Ident LParen parameter_list RParen Colon type_expr LBrace stmt_list RBrace { Ast.Stmt.FunctionDeclStmt { name = $2; is_rec = false; parameters = $4; return_type = $7; body = $9 } }
  | Let Recursive Ident LParen parameter_list RParen Colon type_expr LBrace stmt_list RBrace { Ast.Stmt.FunctionDeclStmt { name = $3; is_rec = true; parameters = $5; return_type = $8; body = $10 } }
  | Let Ident LParen RParen Colon type_expr LBrace stmt_list RBrace { Ast.Stmt.FunctionDeclStmt { name = $2; is_rec = false; parameters = []; return_type = $6; body = $8 } }
  | Let Ident LParen parameter_list RParen LBrace stmt_list RBrace { Ast.Stmt.FunctionDeclStmt { name = $2; is_rec = false; parameters = $4; return_type = Ast.Type.Infer; body = $7 } }
  | Let Recursive Ident LParen parameter_list RParen LBrace stmt_list RBrace { Ast.Stmt.FunctionDeclStmt { name = $3; is_rec = true; parameters = $5; return_type = Ast.Type.Infer; body = $8 } }
  | Let Ident LParen RParen LBrace stmt_list RBrace { Ast.Stmt.FunctionDeclStmt { name = $2; is_rec = false; parameters = []; return_type = Ast.Type.Infer; body = $6 } }

RecordStmt:
  | Type Ident Assign LBrace expr_list RBrace {Ast. Stmt.RecordStmt { name = $2; fields = $5; } }