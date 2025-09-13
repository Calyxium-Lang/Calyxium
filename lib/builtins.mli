val enum_variants : (string, string list) Hashtbl.t
val stdlib_used : bool ref
val built_in_modules : (string * (string * Ast.Type.t) list) list
val builtins : (string * (int list * Ast.Type.t) list) list
