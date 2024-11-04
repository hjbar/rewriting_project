open Def
open Utils

(* Exception quand on échoue dans la ré-écritrue *)

exception Abort of string

let abort s = raise @@ Abort s

(* Oriente le système de ré-écriture *)

let orient rs =
  let rules = rs.rules in

  if List.for_all (fun (_, w1, w2) -> String.length w1 >= String.length w2) rules then
    Some { rs with orient = Right }
  else if List.for_all (fun (_, w1, w2) -> String.length w1 <= String.length w2) rules then
    Some { rs with orient = Left }
  else None

(* Oriente les systèmes de ré-écriture selon les deux règles *)

let orient_all ((_, w1, w2) as r1) ((_, w1', w2') as r2) =
  if String.(length w1 < length w2 && length w1' < length w2') then
    let orient = Left in
    (make ~orient [ r1 ], make ~orient [ r2 ], w2, w2')
  else
    let orient = Right in
    (make ~orient [ r1 ], make ~orient [ r2 ], w1, w1')

(* Normalise un mot en fonction des règles de ré-écriture *)

let normalize ?(limit = max_int) rs word =
  let cpt = ref limit in

  let red =
    match rs.orient with
    | Left -> fun w1 word w2 -> Word.search_and_replace w2 word w1
    | Right -> Word.search_and_replace
  in

  let rec loop word rules =
    decr cpt;
    if !cpt <= 0 then abort "Too many recursions in normalize";

    match rules with
    | [] -> word
    | (_, w1, w2) :: rules' -> begin
      match red w1 word w2 with None -> loop word rules' | Some word' -> loop word' rs.rules
    end
  in

  loop word rs.rules

(* Renvoie toutes les formes normales (avec au moins une réduction) d'un mot en fonction des règles de ré-écriture *)

let normalize_all ?(limit = max_int) rs word =
  let cpt = ref limit in

  let red =
    match rs.orient with
    | Left -> fun w1 word w2 -> Word.search_and_replace_all w2 word w1
    | Right -> Word.search_and_replace_all
  in

  let rec loop word ((_, w1, w2) as rule) =
    decr cpt;
    if !cpt <= 0 then abort "Too many recursions in normalize_all";

    match red w1 word w2 with
    | [] -> [ word ]
    | l -> List.map (fun word' -> loop word' rule) l |> List.flatten
  in

  List.fold_left (fun acc rule -> loop word rule :: acc) [] rs.rules
  |> List.flatten |> List.sort_uniq compare

(* Trouve les paires critiques d'un système de ré-écriture *)

let critical_rules ?(limit = max_int) r1 r2 : (Word.word * Word.word) list =
  let normalize_all = normalize_all ~limit in
  let rs1, rs2, w1, w2 = orient_all r1 r2 in

  let gaz = String.make (String.length w1 - 1) '$' in
  let w2 = gaz ^ w2 ^ gaz in
  let len_w2 = String.length w2 in

  let pairs = ref [] in

  let rec loop w1 =
    if Word.comp_per_char w1 w2 then begin
      let w = Word.unify w1 w2 in
      let n1 = normalize_all rs1 w in
      let n2 = normalize_all rs2 w in
      List.iter
        (fun w1 -> List.iter (fun w2 -> if w1 <> w2 then pairs := (w1, w2) :: !pairs) n2)
        n1
    end;

    if String.length w1 < len_w2 then loop ("$" ^ w1)
  in

  loop w1;
  List.sort_uniq compare !pairs

(* Effectue une complétion de Knuth-Bendix *)

let knuth_bendix ?(limit_norm = max_int) ?limit_pairs rs =
  let rs = Option.value ~default:rs @@ orient rs in

  let normalize = normalize ~limit:limit_norm in
  let get_critical_rules =
    let critical_rules = critical_rules ~limit:limit_norm in
    match limit_pairs with
    | None ->
      fun r rules ->
        !rules
        |> List.map (fun r' -> critical_rules r r' @ critical_rules r' r)
        |> List.flatten |> List.sort_uniq compare
    | Some limit ->
      fun r rules ->
        let pairs =
          !rules
          |> List.map (fun r' -> critical_rules r r' @ critical_rules r' r)
          |> List.flatten |> List.sort_uniq compare
        in
        if List.length pairs > limit then abort "Too much critical rules in knuth_bendix";
        pairs
  in

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

  while !queue <> [] do
    let r = List.hd !queue in
    queue := List.tl !queue;

    List.iter
      begin
        fun (w1, w2) ->
          let rs = { rs with rules = !rules } in

          let w1' = normalize rs w1 in
          let w2' = normalize rs w2 in

          if not @@ Word.eq w1' w2' then add @@ make_rule w1' w2'
      end
      (get_critical_rules r rules)
  done;

  { rs with rules = !rules }
