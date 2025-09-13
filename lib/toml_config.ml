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

let parse_line line =
  let line = String.trim line in
  if line = "" || String.starts_with ~prefix:"#" line then `Ignore
  else if String.starts_with ~prefix:"[" line then
    `Section (String.sub line 1 (String.length line - 2))
  else
    match String.split_on_char '=' line with
    | [ k; v ] ->
        let key = String.trim k in
        let value =
          let v = String.trim v in
          let v =
            match String.index_opt v '#' with
            | Some i -> String.sub v 0 i |> String.trim
            | None -> v
          in
          v
        in
        `Entry (key, value)
    | _ -> `Ignore

let parse_value v =
  let len = String.length v in
  if len >= 2 && v.[0] = '"' && v.[len - 1] = '"' then
    `String (String.sub v 1 (len - 2))
  else if v = "true" then `Bool true
  else if v = "false" then `Bool false
  else if v.[0] = '[' && v.[len - 1] = ']' then
    let items =
      String.sub v 1 (len - 2)
      |> String.split_on_char ',' |> List.map String.trim
      |> List.map (fun s ->
             if s.[0] = '"' then String.sub s 1 (String.length s - 2) else s)
    in
    `Array items
  else `String v

let parse_file (filename : string) : config option =
  let ic = open_in filename in
  let rec loop section acc =
    try
      let line = input_line ic in
      match parse_line line with
      | `Ignore -> loop section acc
      | `Section s -> loop (Some s) acc
      | `Entry (k, v) -> (
          match section with
          | Some "project" ->
              let acc = ("project." ^ k, parse_value v) :: acc in
              loop section acc
          | Some "run" ->
              let acc = ("run." ^ k, parse_value v) :: acc in
              loop section acc
          | _ -> loop section acc)
    with End_of_file -> acc
  in
  let raw = loop None [] in
  close_in ic;

  let get_string key =
    match List.assoc_opt key raw with Some (`String s) -> Some s | _ -> None
  in
  let get_bool key =
    match List.assoc_opt key raw with Some (`Bool b) -> Some b | _ -> None
  in
  let get_array key =
    match List.assoc_opt key raw with Some (`Array a) -> Some a | _ -> None
  in

  match
    ( get_string "project.name",
      get_string "project.version",
      get_string "project.description",
      get_array "project.authors",
      get_string "project.license",
      get_string "project.homepage",
      get_string "project.repository",
      get_string "run.main",
      get_bool "run.run" )
  with
  | ( Some name,
      Some version,
      Some description,
      Some authors,
      Some license,
      Some homepage,
      Some repository,
      Some main,
      Some should_run ) ->
      let emit = get_string "run.emit" in
      Some
        {
          project =
            {
              name;
              version;
              description;
              authors;
              license;
              homepage;
              repository;
            };
          run_cfg = { main; emit; should_run };
        }
  | _ -> None

let create_project name =
  if Sys.file_exists name then (
    Printf.eprintf "%sError:%s Directory '%s' already exists.\n" Color.red
      Color.reset name;
    exit 1);
  let src = Filename.concat name "src" in
  List.iter (fun d -> Unix.mkdir d 0o755) [ name; src ];
  let write_file path content =
    let ch = open_out path in
    output_string ch content;
    close_out ch
  in
  write_file
    (Filename.concat src "main.cx")
    (Printf.sprintf "-- src/main.cx\nprint(\"Hello from %s!\\n\")\n" name);
  write_file
    (Filename.concat name "calyxium.toml")
    (Printf.sprintf
       {|[project]
name = "%s"
version = "0.1.0"
description = "A Calyxium Project"
authors = ["Your Name <you@example.com>"]
license = "LICENSE"
homepage = "https://example.com/%s/home"
repository = "https://example.com/%s"

[run]
main = "src/main.cx"
emit = "build/output.cxc"
run = true
|}
       name name name);
  Printf.printf "%sNew project created:%s %s/\n" Color.green Color.reset name
