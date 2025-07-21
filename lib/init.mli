type stdlib_module = string * (string * Gc.value) list

val define_module : string -> (string * Gc.value) list -> stdlib_module
val native1 : (Gc.value -> Gc.value) -> Gc.value
val native2 : (Gc.value -> Gc.value -> Gc.value) -> Gc.value
val stdlib_definitions : stdlib_module list
val stdlib_modules : (string, (string, Gc.value) Hashtbl.t) Hashtbl.t
