(* to_string function *)

let to_string (name, w1, w2) =
  let name = match name with None -> "R" | Some name -> name in
  Format.sprintf "%s: %s --> %s" name (Word.to_string w1) (Word.to_string w2)

let to_string_eq (_, w1, w2) = Format.sprintf "%s = %s" (Word.to_string w1) (Word.to_string w2)

(* Printing functions *)

let print r = Print.print_flush @@ to_string r

let println r = Print.println_flush @@ to_string r
