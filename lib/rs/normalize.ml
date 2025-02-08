open Utils

(* Affichage de déboggage *)

let print_word w =
  let red = "\027[31m" in
  let green = "\027[32m" in
  let blue = "\027[36m" in
  let purple = "\027[35m" in
  let reset = "\027[0m" in

  let to_color cpt =
    match cpt mod 4 with 0 -> red | 1 -> green | 2 -> blue | 3 -> purple | _ -> assert false
  in

  let rec loop cpt = function
    | [] -> ()
    | 'a' :: 'b' :: 'b' :: l (* Motif à modifier *) ->
      Format.printf "%sABB%s" (* Format à modifier *) (to_color cpt) reset;
      loop (cpt + 1) l
    | c :: l ->
      Format.printf "%c" c;
      loop cpt l
  in

  w |> String.to_seq |> List.of_seq |> loop 0

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
