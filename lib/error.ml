open ANSITerminal

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
  print_string [ red; Bold ] "Error: ";
  print_string [ white ] msg;
  Printf.printf "\n  --> %s:%d:%d\n" file line col;
  match source with
  | Some txt ->
      Printf.printf "   %d | %s\n     | %s" line txt
        (String.make (max 0 (col - 1)) ' ');
      print_string [ red; Bold ] "^\n"
  | None -> ()
