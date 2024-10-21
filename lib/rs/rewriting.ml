open Def
open Utils

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

(* Trouve les paires critiques d'un système de ré-écriture *)

let critical_rules ?(orient = Right) ((_, w1, _) as r1) ((_, w2, _) as r2) =
  let pairs = ref [] in

  let gaz = String.make (String.length w1 - 1) '$' in
  let w2 = gaz ^ w2 ^ gaz in
  let len_w2 = String.length w2 in

  let rs1 = make ~orient [ r1 ] in
  let rs2 = make ~orient [ r2 ] in

  let rec loop w1 =
    if Word.comp_per_char w1 w2 then begin
      let w = Word.unify w1 w2 in
      let n1 = normalize rs1 w in
      let n2 = normalize rs2 w in
      if n1 <> n2 then pairs := (n1, n2) :: !pairs
    end;

    if String.length w1 < len_w2 then loop ("$" ^ w1)
  in

  loop w1;
  List.sort_uniq compare !pairs

(* Effectue une complétion de Knuth-Bendix *)

let knuth_bendix rs =
  let name =
    let cpt = ref ~-1 in
    fun () ->
      incr cpt;
      Format.sprintf "K%d" !cpt
  in

  let make_rule =
    match rs.orient with
    | Left -> fun w1 w2 -> Rule.make ~name:(name ()) w2 w1
    | Right -> fun w1 w2 -> Rule.make ~name:(name ()) w1 w2
  in

  let queue = ref rs.rules in
  let rules = ref rs.rules in

  let add r =
    rules := r :: !rules;

    rules :=
      List.map
        begin
          fun ((name, w1, w2) as r) ->
            let rules = List.filter (fun r' -> not @@ Rule.eq r r') !rules in
            let rs = { rs with rules } in
            (name, normalize rs w1, normalize rs w2)
        end
        !rules;

    rules := List.filter (fun (_, w1, w2) -> not (Word.eq w1 w2)) !rules;
    queue := !queue @ [ r ]
  in

  while not @@ List.is_empty !queue do
    let r = List.hd !queue in
    queue := List.tl !queue;

    let critical_pairs =
      List.map (fun r' -> critical_rules r r' @ critical_rules r' r) !rules |> List.flatten
    in

    List.iter
      begin
        fun (w1, w2) ->
          let rs = { rs with rules = !rules } in

          let w1' = normalize rs w1 in
          let w2' = normalize rs w2 in

          if not @@ Word.eq w1' w2' then add @@ make_rule w1' w2'
      end
      critical_pairs
  done;

  { rs with rules = !rules }
