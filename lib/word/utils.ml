open Def

(* Some utilities functions *)

let make s : word = s

(* Génère tous les mots de taille n avec un alphabet de taille m *)

let get_words ~alpha_len ~word_len =
  if word_len < 0 then failwith @@ Format.sprintf "Word_len %d is not sufficient" word_len;
  if alpha_len < 0 then failwith @@ Format.sprintf "Alpha_len %d is not sufficient" alpha_len;
  if alpha_len > 26 then failwith @@ Format.sprintf "Alpha_len %d is too large" alpha_len;

  let starting_code = Char.code 'a' in
  let tab = Array.init (max 2 (word_len + 1)) (fun _ -> []) in
  let alpha = List.init alpha_len (fun i -> starting_code + i |> Char.chr |> Char.escaped) in

  tab.(0) <- [];
  if word_len <> 0 then tab.(1) <- alpha;

  for i = 2 to word_len do
    let prev_words = tab.(i - 1) in

    let cur_words =
      List.fold_left
        (fun acc prev_word ->
          List.fold_left (fun acc alpha_word -> (prev_word ^ alpha_word) :: acc) acc alpha )
        [] prev_words
    in

    tab.(i) <- cur_words
  done;

  Array.fold_left (fun acc words -> acc @ words) [] tab |> List.sort compare
