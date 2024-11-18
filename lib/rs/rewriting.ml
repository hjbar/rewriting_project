open Utils

(* Exception quand on échoue dans la ré-écritrue *)

exception Abort of string

let abort s = raise @@ Abort s

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
  | Length
  | InvLength
  | Dico
  | InvDico
  | Default

let orient_by_length ((name, w1, w2) as rule) =
  if String.(length w1 >= length w2) then rule else (name, w2, w1)

let orient_by_invlength ((name, w1, w2) as rule) =
  if String.(length w1 < length w2) then rule else (name, w2, w1)

let orient_by_dico (name, w1, w2) = (name, max w1 w2, min w1 w2)

let orient_by_invdico (name, w1, w2) = (name, min w1 w2, max w1 w2)

let orient_rule_gen k r =
  match k with
  | Length -> orient_by_length r
  | InvLength -> orient_by_invlength r
  | Dico -> orient_by_dico r
  | InvDico -> orient_by_invdico r
  | Default -> orient_rule r

let orient_rs_gen k = List.map (orient_rule_gen k)

(* Normalise un mot en fonction des règles de ré-écriture *)

let normalize ?(limit = max_int) rs word =
  let cpt = ref limit in
  let red = Word.search_and_replace in

  let rec loop word rules =
    decr cpt;
    if !cpt <= 0 then abort "Too many recursions in normalize";

    match rules with
    | [] -> word
    | (_, w1, w2) :: rules' -> begin
      match red w1 word w2 with None -> loop word rules' | Some word' -> loop word' rs
    end
  in

  loop word rs

(* Renvoie toutes les formes normales (avec au moins une réduction) d'un mot en fonction des règles de ré-écriture *)

let normalize_all ?(limit = max_int) rs word =
  let cpt = ref limit in
  let red = Word.search_and_replace_all in

  let rec loop word ((_, w1, w2) as rule) =
    decr cpt;
    if !cpt <= 0 then abort "Too many recursions in normalize_all";

    match red w1 word w2 with
    | [] -> [ word ]
    | l -> List.map (fun word' -> loop word' rule) l |> List.flatten
  in

  List.fold_left (fun acc rule -> loop word rule :: acc) [] rs
  |> List.flatten |> List.sort_uniq compare

(* Trouve les paires critiques d'un système de ré-écriture *)

let critical_rules ?(limit = max_int) ((_, w1, _) as r1) ((_, w2, _) as r2) =
  let normalize_all = normalize_all ~limit in
  let rs1, rs2 = (make [ r1 ], make [ r2 ]) in

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

let knuth_bendix_bis ?(limit_pairs = max_int) normalize critical_rules orient_rs orient_rule rs
    =
  let name =
    let cpt = ref ~-1 in
    fun () ->
      incr cpt;
      Format.sprintf "K%d" !cpt
  in

  let make_rule w1 w2 = Rule.make ~name:(name ()) w1 w2 |> orient_rule in

  let rs = orient_rs rs in
  let rules = ref rs in
  let queue = rs |> List.to_seq |> Queue.of_seq in

  let add r =
    rules := r :: !rules;

    rules :=
      List.map
        begin
          fun ((name, w1, w2) as r) ->
            let rs = List.filter (fun r' -> not @@ Rule.eq r r') !rules in
            (name, normalize rs w1, normalize rs w2)
        end
        !rules;

    rules := List.filter (fun (_, w1, w2) -> not @@ Word.eq w1 w2) !rules;
    Queue.push r queue
  in

  while not @@ Queue.is_empty queue do
    let r = Queue.pop queue in

    let critical_pairs =
      !rules
      |> List.map (fun r' -> critical_rules r r' @ critical_rules r' r)
      |> List.flatten |> List.sort_uniq compare
    in

    if List.length critical_pairs > limit_pairs then
      abort "Too much critical rules in knuth_bendix";

    List.iter
      begin
        fun (w1, w2) ->
          let rs = !rules in

          let w1' = normalize rs w1 in
          let w2' = normalize rs w2 in

          if not @@ Word.eq w1' w2' then add @@ make_rule w1' w2'
      end
      critical_pairs
  done;

  !rules

let knuth_bendix ?(limit_norm = max_int) ?(limit_pairs = max_int) rs =
  let normalize = normalize ~limit:limit_norm in
  let critical_rules = critical_rules ~limit:limit_norm in
  let kd_bis = knuth_bendix_bis ~limit_pairs normalize critical_rules in

  let orient_rule_list =
    [ orient_rule_gen Default
    ; orient_rule_gen Length
    ; orient_rule_gen Dico
    ; orient_rule_gen InvLength
    ; orient_rule_gen InvDico
    ]
  in
  let orient_rs_list =
    [ orient_rs_gen Default
    ; orient_rs_gen Length
    ; orient_rs_gen Dico
    ; orient_rs_gen InvLength
    ; orient_rs_gen InvDico
    ]
  in

  let error_msg = ref "" in

  let res =
    List.fold_left
      begin
        fun acc orient_rs ->
          List.fold_left
            begin
              fun acc orient_rule ->
                if Option.is_some acc then acc
                else begin
                  try kd_bis orient_rs orient_rule rs |> Option.some
                  with Abort s ->
                    error_msg := Format.sprintf "%s%s\n" !error_msg s;
                    None
                end
            end
            acc orient_rule_list
      end
      None orient_rs_list
  in

  match res with None -> raise @@ Abort !error_msg | Some res -> res
