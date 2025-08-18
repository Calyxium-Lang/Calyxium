type t = (int, Ast.Type.t) Hashtbl.t

val empty : unit -> ('a, 'b) Hashtbl.t
val find_opt : ('a, 'b) Hashtbl.t -> 'a -> 'b option
val add : ('a, 'b) Hashtbl.t -> 'a -> 'b -> unit
val apply : (int, Ast.Type.t) Hashtbl.t -> Ast.Type.t -> Ast.Type.t
