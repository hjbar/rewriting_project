open Utils

(* Normalise un mot en fonction des règles de ré-écriture *)

let normalize ?(limit = max_int) rs word =
  let red = Word.search_and_replace in

  let rec loop word rules cpt =
    let cpt = cpt - 1 in
    if cpt < 0 then abort "Too many recursions in normalize";

    match rules with
    | [] -> word
    | (_, w1, w2) :: rules' -> begin
      match red w1 word w2 with
      | None -> loop word rules' cpt
      | Some word' -> loop word' rs cpt
    end
  in

  loop word rs limit

(* Renvoie toutes les formes normales (avec au moins une réduction) d'un mot en fonction des règles de ré-écriture *)

let normalize_all ?(limit = max_int) rs word =
  let red = Word.search_and_replace_all in

  let rec loop word ((_, w1, w2) as rule) cpt =
    let cpt = cpt - 1 in
    if cpt < 0 then abort "Too many recursions in normalize_all";

    match red w1 word w2 with
    | [] -> [ word ]
    | l -> List.map (fun word' -> loop word' rule cpt) l |> List.flatten
  in

  List.fold_left (fun acc rule -> loop word rule limit :: acc) [] rs
  |> List.flatten |> List.sort_uniq compare
