module String = struct
  let contains (str : string) (substr : string) : bool =
    let len_str = String.length str in
    let len_sub = String.length substr in
    let rec aux i =
      if i > len_str - len_sub then false
      else if String.sub str i len_sub = substr then true
      else aux (i + 1)
    in
    if len_sub = 0 then true else aux 0

  let split (str : string) (sep : string) : string list =
    let len = String.length str in
    let len_sep = String.length sep in
    if len_sep = 0 then failwith "split: empty separator"
    else
      let rec aux acc start =
        try
          let i = String.index_from str start sep.[0] in
          if i + len_sep <= len && String.sub str i len_sep = sep then
            let part = String.sub str start (i - start) in
            aux (part :: acc) (i + len_sep)
          else aux acc (i + 1)
        with Not_found ->
          let part = String.sub str start (len - start) in
          List.rev (part :: acc)
      in
      aux [] 0

  let replace (str : string) (target : string) (replacement : string) : string =
    let parts = split str target in
    String.concat replacement parts

  let strip (str : string) : string =
    let is_space c = c = ' ' || c = '\n' || c = '\t' || c = '\r' in
    let len = String.length str in
    let rec left i =
      if i >= len then len else if is_space str.[i] then left (i + 1) else i
    in
    let rec right i =
      if i < 0 then -1 else if is_space str.[i] then right (i - 1) else i
    in
    let l = left 0 in
    let r = right (len - 1) in
    if r < l then "" else String.sub str l (r - l + 1)
end
