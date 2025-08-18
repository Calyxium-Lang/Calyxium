val ensure_dir_exists_rec : string -> unit
val parse_emit_flag : string list -> string -> string option
val parse_file : flags:string list -> string -> unit
val split_flags_and_files : string list -> string list * string list
val check_file : string -> unit
