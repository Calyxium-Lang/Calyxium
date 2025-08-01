type project_config = {
  name : string;
  version : string;
  description : string;
  authors : string list;
  license : string;
  homepage : string;
  repository : string;
}

type run_config = { main : string; emit : string option; run : bool }
type config = { project : project_config; run : run_config }

val parse_file : string -> config option
val create_project : string -> unit
