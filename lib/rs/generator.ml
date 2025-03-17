(* Donne tous les sous-ensembles d'une liste *)

let rec subsets = function
  | [] -> [ [] ]
  | x :: l ->
    let subsets = subsets l in
    let new_subsets = List.map (fun subset -> x :: subset) subsets in
    subsets @ new_subsets

(* Donne la liste des sous-chaines contigues *)

let all_sub_strings s =
  let n = String.length s in

  let rec aux i j acc =
    if i >= n then List.rev acc
    else if j > n then aux (i + 1) (i + 1) acc
    else aux i (j + 1) (String.sub s i (j - i) :: acc)
  in

  aux 0 1 [] |> List.sort_uniq compare |> List.filter (( <> ) "")

(* Donne le premier générateur disponible *)

let first_new_gen rs =
  let max_char = ref 'a' in

  List.iter
    begin
      fun (_, w1, w2) ->
        String.iter (fun c -> max_char := max !max_char c) w1;
        String.iter (fun c -> max_char := max !max_char c) w2
    end
    rs;

  Char.code !max_char + 1 |> Char.chr |> Char.escaped

(* Donne la liste des nouvelles règles à considérer *)

let new_rules rs =
  let new_gen = first_new_gen rs in

  let new_rules_of_word w =
    let cur_gen = ref new_gen in
    List.fold_left
      (fun acc s ->
        let r = Rule.make !cur_gen s in
        cur_gen := 1 + Char.code !cur_gen.[0] |> Char.chr |> Char.escaped;
        r :: acc )
      [] (all_sub_strings w)
  in

  let new_rules_of_rule (_, w1, w2) = new_rules_of_word w1 @ new_rules_of_word w2 in

  let new_rules_of_rs rs =
    List.fold_left (fun acc rule -> acc @ new_rules_of_rule rule) [] rs
  in

  rs |> new_rules_of_rs |> List.sort_uniq (fun (_, _, w1) (_, _, w2) -> compare w1 w2)

(* Donne tous les sous-ensembles des nouvelles règles *)

let new_subset_rules rs = subsets @@ new_rules rs

(* Donne la liste des nouveaux mots à considérer *)

let new_words rs =
  let new_gen = first_new_gen rs in

  let new_rules_of_word w =
    let cur_gen = ref new_gen in
    List.fold_left
      (fun acc s ->
        let p = (!cur_gen, s) in
        cur_gen := 1 + Char.code !cur_gen.[0] |> Char.chr |> Char.escaped;
        p :: acc )
      [] (all_sub_strings w)
  in

  let new_rules_of_rule (_, w1, w2) = new_rules_of_word w1 @ new_rules_of_word w2 in

  let new_rules_of_rs rs =
    List.fold_left (fun acc rule -> acc @ new_rules_of_rule rule) [] rs
  in

  rs |> new_rules_of_rs |> List.sort_uniq (fun (_, w1) (_, w2) -> compare w1 w2)

(* Donne tous les sous-ensembles des nouveaux mots *)

let new_subset_words rs = subsets @@ new_words rs
