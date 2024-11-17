(* to_string functions *)

let to_string rs =
  let len = List.length rs in

  List.fold_left
    begin
      fun (i, acc) rule ->
        let i' = i + 1 in
        let s = acc ^ Rule.to_string rule in

        if i' < len then (i', s ^ "\n") else (i', s)
    end
    (0, "") rs
  |> snd

(* Printing functions *)

let print rs = Print.print_flush @@ to_string rs

let println rs = Print.println_flush @@ to_string rs
