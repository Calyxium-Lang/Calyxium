exception TypeError of string
exception UnifyError of string

val fresh_var_counter : int ref
val fresh_tyvar : unit -> Ast.Type.t
val occurs_check : Subst.t -> int -> Ast.Type.t -> bool
val string_of_type : Ast.Type.t -> string
val type_eq : Ast.Type.t -> Ast.Type.t -> bool
