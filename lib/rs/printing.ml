open Def

(* to_string functions *)

let orient_to_string = function Left -> "<--" | Right -> "-->"

let to_string rs =
  let orient = orient_to_string rs.orient in
  List.fold_left (fun acc rule -> acc ^ Rule.to_string ~sep:orient rule ^ "\n") "" rs.rules

(* Printing functions *)

let print rs = Print.print_flush @@ to_string rs

let println rs = Print.println_flush @@ to_string rs
