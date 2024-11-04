open Def

(* to_string functions *)

let orient_to_string = function Left -> "<--" | Right -> "-->"

let to_string rs =
  let orient = orient_to_string rs.orient in
  let len = List.length rs.rules in

  List.fold_left
    begin
      fun (i, acc) rule ->
        let i' = i + 1 in
        let s = acc ^ Rule.to_string ~sep:orient rule in

        if i' < len then (i', s ^ "\n") else (i', s)
    end
    (0, "") rs.rules
  |> snd

(* Printing functions *)

let print rs = Print.print_flush @@ to_string rs

let println rs = Print.println_flush @@ to_string rs
