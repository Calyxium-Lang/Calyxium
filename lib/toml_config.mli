type project_config = {
  name : string;
  version : string;
  description : string;
  authors : string list;
  license : string;
  homepage : string;
  repository : string;
}

type run_config = { main : string; emit : string option; should_run : bool }
type config = { project : project_config; run_cfg : run_config }

val parse_line :
  string -> [> `Entry of string * string | `Ignore | `Section of string ]

val parse_value :
  string -> [> `Array of string list | `Bool of bool | `String of string ]

val parse_file : string -> config option
val create_project : string -> unit
