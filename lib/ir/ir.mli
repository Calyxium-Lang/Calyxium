open Calyxium_parser

(** The intermediate representation (IR) for Calyxium. *)

type instr = ..
(** Represents an IR instruction. *)

type expr = ..
(** Represents an IR expression. *)

type var = { var_name : string; typ : Ast.Type.t; value : expr option }
(** Represents a declared variable in the IR.

    @param var_name The variable's identifier.
    @param typ The variable's static type.
    @param value An optional initializer expression. *)

type func_decl = {
  name : string;
  is_rec : bool;
  parameters : Ast.Stmt.parameter list;
  return_type : Ast.Type.t;
  body : instr list;
}
(** Represents a function declaration in the IR.

    @param name The function's name.
    @param is_rec Whether the function is recursive.
    @param parameters The list of function parameters.
    @param return_type The function's return type.
    @param body The list of IR instructions forming the function body. *)

(** IR instruction constructors. *)
type instr +=
  | IR_Expr of expr  (** A standalone expression used as a statement. *)
  | IR_Block of instr list
        (** A block of instructions executed sequentially. *)
  | IR_FuncDecl of func_decl  (** A function declaration. *)

(** IR expression constructors. *)
type expr +=
  | IR_Int of Bigint.t  (** Integer literal. *)
  | IR_Float of float  (** Floating-point literal. *)
  | IR_String of string  (** String literal. *)
  | IR_Byte of char  (** Character literal. *)
  | IR_Bool of bool  (** Boolean literal. *)
  | IR_Unit  (** The unit value [()]. *)
  | IR_Var of string  (** A reference to a variable by name. *)
  | IR_Binary of expr * Token.t * expr  (** A binary operator application. *)
  | IR_Unary of Token.t * expr  (** A unary operator application. *)
  | IR_Call of expr * expr list
        (** A function call with a callee expression and a list of arguments. *)
  | IR_Array of expr list  (** Array literal. *)
  | IR_Index of expr * expr  (** Array indexing [arr.(idx)]. *)
  | IR_Tuple of expr list  (** Tuple literal. *)
  | IR_If of expr * expr * expr option
        (** Conditional expression optionally with an else branch. *)
  | IR_BlockExpr of instr list  (** A block treated as an expression. *)
  | IR_Ternary of expr * expr * expr  (** Ternary conditional [cond ? a : b]. *)
  | IR_Pipeline of expr * expr  (** Pipeline operator [lhs |> rhs]. *)
  | IR_Lambda of { parameters : Ast.Stmt.parameter list; body : instr list }
        (** Anonymous function/lambda expression. *)
  | IR_Range of expr option * expr option
        (** Numeric or iterable range, possibly open-ended. *)
  | IR_Slice of expr * expr option * expr option
        (** Slice operator [expr[start:end]]. *)
  | IR_Enum of { name : string; members : (string * int) list }
        (** Enumeration definition. *)
  | IR_Dot of expr * string  (** Field or member access. *)
  | IR_Import of { module_name : string list }  (** Module import expression. *)
  | IR_Match of expr * (expr option * instr list) list
        (** Pattern matching construct. *)
  | IR_VarDecl of var  (** Variable declaration. *)
  | IR_MultiVarDecl of { names : string list; value : expr; typ : Ast.Type.t }
        (** Multi-variable declaration with a shared initializer. *)

type t = instr list
(** A full IR program consists of a list of instructions. *)

val ast_to_ir_expr : Ast.Expr.t -> expr
(** Convert an AST expression into an IR expression.

    @param expr The AST expression to convert.
    @return The equivalent IR expression. *)

val ast_to_ir_stmt : Ast.Stmt.t -> instr
(** Convert an AST statement into an IR instruction.

    @param stmt The AST statement to convert.
    @return The equivalent IR instruction. *)

val ast_to_ir : Ast.Stmt.t -> instr list
(** Convert a full AST statement (typically a top-level block or program) into a
    list of IR instructions.

    @param root The AST node representing a program or top-level block.
    @return The list of generated IR instructions. *)
