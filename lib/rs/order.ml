(* Type d'orientations *)

type kind_orient =
  | Default
  | InvDefault
  | WeightLexico
  | InvWeightLexico
  | RevLexico
  | InvRevLexico
  | Dico
  | InvDico
  | Length
  | InvLength

(* Ordre lexicographique *)

let orient_rule (name, w1, w2) =
  let len1 = String.length w1 in
  let len2 = String.length w2 in

  if len1 < len2 then (name, w2, w1)
  else if len2 < len1 then (name, w1, w2)
  else (name, max w1 w2, min w1 w2)

let orient_rs rs = List.map orient_rule rs

let orient_by_invdefault rule =
  let name, w1, w2 = orient_rule rule in
  (name, w2, w1)

(* Variante de l'ordre lexicographique *)

let orient_by_weightlexico (name, w1, w2) =
  let weight1 = String.fold_left (fun acc c -> acc + Char.code c) 0 w1 in
  let weight2 = String.fold_left (fun acc c -> acc + Char.code c) 0 w2 in

  let len1 = String.length w1 in
  let len2 = String.length w2 in

  if len1 < len2 then (name, w2, w1)
  else if len2 < len1 then (name, w1, w2)
  else if weight1 < weight2 then (name, w2, w1)
  else if weight2 < weight1 then (name, w1, w2)
  else (name, max w1 w2, min w1 w2)

let orient_by_invweightlexico rule =
  let name, w1, w2 = orient_by_weightlexico rule in
  (name, w2, w1)

let orient_by_revlexico (name, w1, w2) =
  let rev_w1 =
    w1 |> String.to_seq |> List.of_seq |> List.rev |> List.to_seq |> String.of_seq
  in
  let rev_w2 =
    w2 |> String.to_seq |> List.of_seq |> List.rev |> List.to_seq |> String.of_seq
  in

  let len1 = String.length w1 in
  let len2 = String.length w2 in

  if len1 < len2 then (name, w2, w1)
  else if len2 < len1 then (name, w1, w2)
  else if rev_w1 < rev_w2 then (name, w2, w1)
  else if rev_w2 < rev_w1 then (name, w1, w2)
  else assert false

let orient_by_invrevlexico rule =
  let name, w1, w2 = orient_by_revlexico rule in
  (name, w2, w1)

(* Ordre du dictionnaire *)

let orient_by_dico (name, w1, w2) = (name, max w1 w2, min w1 w2)

let orient_by_invdico (name, w1, w2) = (name, min w1 w2, max w1 w2)

(* Ordre de la longueur (pas bien fondé) *)

let orient_by_length ((name, w1, w2) as rule) =
  if String.(length w1 >= length w2) then rule else (name, w2, w1)

let orient_by_invlength ((name, w1, w2) as rule) =
  if String.(length w1 <= length w2) then rule else (name, w2, w1)

(* Choisir son orientation *)

let orient_rule_gen k =
  match k with
  | Default -> orient_rule
  | InvDefault -> orient_by_invdefault
  | WeightLexico -> orient_by_weightlexico
  | InvWeightLexico -> orient_by_invweightlexico
  | RevLexico -> orient_by_revlexico
  | InvRevLexico -> orient_by_invrevlexico
  | Dico -> orient_by_dico
  | InvDico -> orient_by_invdico
  | Length -> orient_by_length
  | InvLength -> orient_by_invlength

let orient_rs_gen k = List.map (orient_rule_gen k)

(* Liste des orientations *)

let orient_rule_list : (Rule.rule -> Rule.rule) list =
  List.map orient_rule_gen [ Default; InvDefault; Dico; InvDico ]

let weak_orient_rule_list : (Rule.rule -> Rule.rule) list =
  List.map orient_rule_gen [ Length; InvLength ]
