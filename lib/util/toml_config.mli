(** Project and runtime configuration types and parsing utilities. *)

type project_config = {
  name : string;  (** Project name. *)
  version : string;  (** Project version, e.g., "1.0.0". *)
  description : string;  (** Short description of the project. *)
  authors : string list;  (** List of project authors. *)
  license : string;  (** License identifier, e.g., "MIT". *)
  homepage : string;  (** URL of the project homepage. *)
  repository : string;  (** URL of the project's source repository. *)
}
(** Metadata about a project. *)

type run_config = {
  main : string;  (** Entry point file for the program. *)
  emit : string option;
      (** Optional output target, e.g., bytecode or binary. *)
  should_run : bool;
      (** Whether to automatically run the program after building. *)
}
(** Configuration for running a project. *)

type config = {
  project : project_config;  (** Project metadata. *)
  run_cfg : run_config;  (** Runtime configuration. *)
}
(** Complete project configuration. *)

val parse_line :
  string -> [> `Entry of string * string | `Ignore | `Section of string ]
(** [parse_line line] parses a single line of a configuration file.

    @return
      - [`Entry (key, value)] for a key-value pair
      - [`Ignore] for comments or empty lines
      - [`Section name] for section headers *)

val parse_value :
  string -> [> `Array of string list | `Bool of bool | `String of string ]
(** [parse_value s] parses a string value from a configuration file.

    @return
      - [`Array lst] if the value represents a list of strings
      - [`Bool b] if the value represents a boolean
      - [`String s] for plain string values *)

val parse_file : string -> config option
(** [parse_file filename] parses a TOML or similar configuration file at
    [filename] and returns a [config] if successful. *)

val create_project : string -> unit
(** [create_project path] initializes a new project at [path] by generating
    default configuration files and directories. *)
