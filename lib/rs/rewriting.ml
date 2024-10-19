open Def

(* Normalise un mot en fonction du es règles de ré-écriture *)

let normalize rs word =
  let red =
    match rs.orient with
    | Left -> fun w1 word w2 -> Word.search_and_replace w2 word w1
    | Right -> Word.search_and_replace
  in

  let rec loop word = function
    | [] -> word
    | (_, w1, w2) :: rules -> begin
      match red w1 word w2 with None -> loop word rules | Some word' -> loop word' rs.rules
    end
  in

  loop word rs.rules
