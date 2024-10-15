open Def

(* Some utilities functions *)

let make s : word = s

(* Génère tous les mots de taille n avec un alphabet de taille m *)

let get_words ~alpha_len ~word_len =
  if word_len < 0 then failwith @@ Format.sprintf "Word_len %d is not sufficient" word_len;
  if alpha_len < 0 then failwith @@ Format.sprintf "Alpha_len %d is not sufficient" alpha_len;
  if alpha_len > 26 then failwith @@ Format.sprintf "Alpha_len %d is too large" alpha_len;

  let starting_code = Char.code 'a' in
  let tab = Array.init word_len (fun _ -> []) in

  let rec loop = function
    | 0 -> ()
    | 1 ->
      let words =
        List.init alpha_len (fun i -> Char.chr (starting_code + i) |> Char.escaped)
      in
      tab.(0) <- words
    | n ->
      loop (n - 1);
      let prev_words = tab.(n - 2) in
      let base_words = tab.(0) in
      let words =
        List.fold_left
          (fun acc prev_word ->
            List.fold_left (fun acc base_word -> (prev_word ^ base_word) :: acc) acc base_words
            )
          [] prev_words
      in
      tab.(n - 1) <- words
  in

  loop word_len;
  let words = Array.fold_left (fun acc words -> acc @ words) [] tab in
  List.sort compare words
