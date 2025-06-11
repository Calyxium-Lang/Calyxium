let version = (0, 1, 0)
let codename = "Amber"

let detect_system () =
  match Sys.os_type with
  | "Win32" -> "Windows"
  | "Unix" ->
      String.capitalize_ascii
        (try
           let ic = Unix.open_process_in "umame" in
           let r = input_line ic in
           let _ = Unix.close_process_in ic in
           r
         with _ -> "unknown")
  | _ -> "Uknown"

let version_string () =
  let major, minor, patch = version in
  Printf.sprintf "%d.%d.%d (%s) on %s" major minor patch codename
    (detect_system ())
