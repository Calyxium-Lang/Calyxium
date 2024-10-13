module TypeChecker : sig
  module Env : sig
    type key = string
    type 'a t = 'a Map.Make(String).t

    val empty : 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t

    val merge :
      (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t

    val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val filter_map : (key -> 'a -> 'b option) -> 'a t -> 'b t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val cardinal : 'a t -> int
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val min_binding_opt : 'a t -> (key * 'a) option
    val max_binding : 'a t -> key * 'a
    val max_binding_opt : 'a t -> (key * 'a) option
    val choose : 'a t -> key * 'a
    val choose_opt : 'a t -> (key * 'a) option
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val find : key -> 'a t -> 'a
    val find_opt : key -> 'a t -> 'a option
    val find_first : (key -> bool) -> 'a t -> key * 'a
    val find_first_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val find_last : (key -> bool) -> 'a t -> key * 'a
    val find_last_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val to_seq : 'a t -> (key * 'a) Seq.t
    val to_rev_seq : 'a t -> (key * 'a) Seq.t
    val to_seq_from : key -> 'a t -> (key * 'a) Seq.t
    val add_seq : (key * 'a) Seq.t -> 'a t -> 'a t
    val of_seq : (key * 'a) Seq.t -> 'a t
  end

  type func_sig = {
    param_types : Syntax.Ast.Type.t list;
    return_type : Syntax.Ast.Type.t;
  }

  val print_func_sig : func_sig
  val input_func_sig : func_sig
  val println_func_sig : func_sig

  type class_info = {
    class_type : Syntax.Ast.Type.t;
    properties : (string * Syntax.Ast.Type.t) list;
  }

  type env = {
    var_type : Syntax.Ast.Type.t Env.t;
    func_env : func_sig Env.t;
    class_env : class_info Env.t;
    modules : string list;
    exports : string list;
  }

  val len_func_sig : func_sig
  val to_string_func_sig : func_sig
  val to_int_func_sig : func_sig
  val to_float_func_sig : func_sig
  val register_builtin_functions : env -> env
  val empty_env : env
  val register_module_functions : env -> string -> func_sig Env.t
  val load_module : env -> string -> env
  val check_function_call : env -> Env.key -> func_sig
  val lookup_var : env -> Env.key -> Syntax.Ast.Type.t
  val lookup_func : env -> Env.key -> func_sig
  val lookup_class : env -> Env.key -> class_info
  val check_import : env -> string -> env
  val check_export : env -> Env.key -> env
  val check_expr : env -> Syntax.Ast.Expr.t -> Syntax.Ast.Type.t

  val check_var_decl :
    env -> Env.key -> Syntax.Ast.Type.t -> Syntax.Ast.Expr.t option -> env

  val check_func_decl :
    env ->
    Env.key ->
    Syntax.Ast.Stmt.parameter list ->
    Syntax.Ast.Type.t ->
    Syntax.Ast.Stmt.t list ->
    env

  val check_stmt :
    env ->
    expected_return_type:Syntax.Ast.Type.t option ->
    Syntax.Ast.Stmt.t ->
    env

  val check_block :
    env ->
    Syntax.Ast.Stmt.t list ->
    expected_return_type:Syntax.Ast.Type.t option ->
    env
end
