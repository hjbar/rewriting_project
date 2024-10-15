(* to_string function *)

let to_string ?(sep = "=") (name, w1, w2) =
  let name = match name with None -> "R" | Some name -> name in
  Format.sprintf "%s: %s %s %s" name (Word.to_string w1) sep (Word.to_string w2)

(* Printing functions *)

let print r = Print.print_flush @@ to_string r

let println r = Print.println_flush @@ to_string r
