(** [ensure_dir_exists_rec path] ensures that the directory at [path] exists. If
    any intermediate directories do not exist, they are created recursively.

    @param path The directory path to ensure existence.
    @raise Unix_error if a directory cannot be created. *)

val ensure_dir_exists_rec : string -> unit

(** [parse_emit_flag flags default_flag] searches [flags] for a specific emit
    flag and returns its value if present, otherwise returns [default_flag].

    @param flags The list of command-line flags.
    @param default_flag The fallback flag value.
    @return The value of the emit flag, or [default_flag] if not found. *)

val parse_emit_flag : string list -> string -> string option

(** [parse_file ~flags filename] parses the file [filename] with the given
    [flags].

    @param flags Command-line flags affecting parsing.
    @param filename The file path to parse.
    @raise Failure if parsing fails. *)

val parse_file : flags:string list -> string -> unit

(** [split_flags_and_files args] separates a list of command-line [args] into
    flags and file paths.

    @param args The list of command-line arguments.
    @return
      A tuple [(flags, files)] where [flags] are the recognized flags and
      [files] are the remaining file paths. *)

val split_flags_and_files : string list -> string list * string list

(** [check_file filename] performs checks on the given file, such as existence
    and readability.

    @param filename The path of the file to check.
    @raise Failure if the file cannot be read or fails validation. *)

val check_file : string -> unit
