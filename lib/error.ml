open Color

let get_line filename n =
  let ic = open_in filename in
  let rec aux i =
    match input_line ic with
    | l when i = 1 ->
        close_in ic;
        Some l
    | _ -> aux (i - 1)
    | exception End_of_file ->
        close_in ic;
        None
  in
  aux n

let print_error ~file ~line ~col ~msg ~source =
  Printf.printf "%s%sError:%s %s\n" bold red reset msg;
  Printf.printf "  --> %s:%d:%d\n" file line col;
  match source with
  | Some txt ->
      Printf.printf "   %d | %s\n       %s%s^%s\n" line txt
        (String.make (max 0 (col - 1)) ' ')
        (bold ^ red) reset
  | None -> ()
