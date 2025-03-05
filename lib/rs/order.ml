(* Oriente le système de ré-écriture *)

let orient_rule (name, w1, w2) =
  let len1 = String.length w1 in
  let len2 = String.length w2 in

  if len1 < len2 then (name, w2, w1)
  else if len2 < len1 then (name, w1, w2)
  else (name, max w1 w2, min w1 w2)

let orient_rs rs = List.map orient_rule rs

(* Autres orientations *)

type kind_orient =
  | Default
  | Length
  | Dico
  | InvDefault
  | InvLength
  | InvDico

let orient_by_length ((name, w1, w2) as rule) =
  if String.(length w1 >= length w2) then rule else (name, w2, w1)

let orient_by_invlength ((name, w1, w2) as rule) =
  if String.(length w1 <= length w2) then rule else (name, w2, w1)

let orient_by_dico (name, w1, w2) = (name, max w1 w2, min w1 w2)

let orient_by_invdico (name, w1, w2) = (name, min w1 w2, max w1 w2)

let orient_by_invdefault (name, w1, w2) =
  let len1 = String.length w1 in
  let len2 = String.length w2 in

  if len1 > len2 then (name, w2, w1)
  else if len2 > len1 then (name, w1, w2)
  else (name, min w1 w2, max w1 w2)

let orient_rule_gen k =
  match k with
  | Default -> orient_rule
  | Length -> orient_by_length
  | Dico -> orient_by_dico
  | InvDefault -> orient_by_invdefault
  | InvLength -> orient_by_invlength
  | InvDico -> orient_by_invdico

let orient_rs_gen k = List.map (orient_rule_gen k)

(* Liste des orientations *)

let orient_rule_list : (Rule.rule -> Rule.rule) list =
  [ orient_rule_gen Default
  ; orient_rule_gen Dico
  ; orient_rule_gen InvDefault
  ; orient_rule_gen InvDico
  ]

let weak_orient_rule_list : (Rule.rule -> Rule.rule) list =
  [ orient_rule_gen Length; orient_rule_gen InvLength ]
