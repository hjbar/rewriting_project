open Utils
open Order
open Generator
open Normalize

(* Debug *)

let debug = false

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

let knuth_bendix_bis ?(limit_pairs = max_int) normalize critical_rules orient_rule rs =
  (* Fonctions auxiliaires pour la complétion *)
  let name =
    let cpt = ref ~-1 in
    fun () ->
      incr cpt;
      Format.sprintf "K%d" !cpt
  in

  let make_rule w1 w2 = Rule.make ~name:(name ()) w1 w2 |> orient_rule in

  let rs = List.map orient_rule rs in
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

  (* Coeur de la complétion *)
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

          let w1 = normalize rs w1 in
          let w2 = normalize rs w2 in

          if not @@ Word.eq w1 w2 then add @@ make_rule w1 w2
      end
      critical_pairs
  done;

  (* On renvoie le système complété *)
  !rules

let knuth_bendix ?(fast = true) ?(limit_norm = max_int) ?(limit_pairs = max_int) rs =
  (* On vérifie que le rs n'est pas vide *)
  if rs = [] then abort "Le système de réécriture est vide";

  (* On initialise quelques fonctions *)
  let normalize = normalize ~limit:limit_norm in
  let critical_rules = critical_rules ~limit:limit_norm in
  let kd_bis = knuth_bendix_bis ~limit_pairs normalize critical_rules in

  (* Message d'erreur en cas d'échec *)
  let error_msg = ref "" in

  (* Cache pour ne pas re-calculer deux fois la même complétion *)
  let cache = Hashtbl.create 16 in

  (* On teste différentes orientations *)
  let kd_first_step orient_rule_list =
    List.iter
      begin
        fun (kind, orient_rule) ->
          if not @@ Hashtbl.mem cache (kind, rs) then begin
            try return @@ kd_bis orient_rule rs
            with Abort s -> error_msg := Format.sprintf "%s%s\n" !error_msg s
          end;

          Hashtbl.replace cache (kind, rs) ()
      end
      orient_rule_list
  in

  (* On teste les différentes nouvelles règles *)
  let kd_second_step orient_rule_list rules =
    List.iter
      begin
        fun (kind, orient_rule) ->
          List.iter
            begin
              fun rule ->
                let rs = rs @ [ rule ] in

                if not @@ Hashtbl.mem cache (kind, rs) then begin
                  try return @@ kd_bis orient_rule rs
                  with Abort s -> error_msg := Format.sprintf "%s%s\n" !error_msg s
                end;

                Hashtbl.replace cache (kind, rs) ()
            end
            rules
      end
      orient_rule_list
  in

  (* On teste les différents nouveaux mots *)
  let kd_third_step orient_rule_list words =
    let tries word =
      List.iter
        begin
          fun (kind, orient_rule) ->
            List.iter
              begin
                fun (gen, s) ->
                  let () =
                    let word = s ^ word in
                    let rs = rs @ [ Rule.make gen word ] in

                    if not @@ Hashtbl.mem cache (kind, rs) then begin
                      try return @@ kd_bis orient_rule rs
                      with Abort err_s ->
                        error_msg := Format.sprintf "%s%s\n" !error_msg err_s
                    end;

                    Hashtbl.replace cache (kind, rs) ()
                  in

                  let () =
                    let word = word ^ s in
                    let rs = rs @ [ Rule.make gen word ] in

                    if not @@ Hashtbl.mem cache (kind, rs) then begin
                      try return @@ kd_bis orient_rule rs
                      with Abort err_s ->
                        error_msg := Format.sprintf "%s%s\n" !error_msg err_s
                    end;

                    Hashtbl.replace cache (kind, rs) ()
                  in

                  let () =
                    let word = s ^ word ^ s in
                    let rs = rs @ [ Rule.make gen word ] in

                    if not @@ Hashtbl.mem cache (kind, rs) then begin
                      try return @@ kd_bis orient_rule rs
                      with Abort err_s ->
                        error_msg := Format.sprintf "%s%s\n" !error_msg err_s
                    end;

                    Hashtbl.replace cache (kind, rs) ()
                  in

                  ()
              end
              words
        end
        orient_rule_list
    in

    List.iter
      begin
        fun (_, w1, w2) ->
          tries w1;
          tries w2
      end
      rs
  in

  (* On teste les différentes sous-ensembles de nouvelles règles *)
  let kd_fourth_step orient_rule_list subset_rules =
    List.iter
      begin
        fun (kind, orient_rule) ->
          List.iter
            begin
              fun rules ->
                let rs = rs @ rules in

                if not @@ Hashtbl.mem cache (kind, rs) then begin
                  try return @@ kd_bis orient_rule rs
                  with Abort s -> error_msg := Format.sprintf "%s%s\n" !error_msg s
                end;

                Hashtbl.replace cache (kind, rs) ()
            end
            subset_rules
      end
      orient_rule_list
  in

  (* On teste les différents sous-ensembles nouveaux mots *)
  let kd_fifth_step orient_rule_list subsets_words =
    let tries word =
      List.iter
        begin
          fun (kind, orient_rule) ->
            List.iter
              begin
                fun subsets ->
                  let () =
                    let words = List.map (fun (gen, s) -> Rule.make gen (s ^ word)) subsets in
                    let rs = rs @ words in

                    if not @@ Hashtbl.mem cache (kind, rs) then begin
                      try return @@ kd_bis orient_rule rs
                      with Abort err_s ->
                        error_msg := Format.sprintf "%s%s\n" !error_msg err_s
                    end;

                    Hashtbl.replace cache (kind, rs) ()
                  in

                  let () =
                    let words = List.map (fun (gen, s) -> Rule.make gen (word ^ s)) subsets in
                    let rs = rs @ words in

                    if not @@ Hashtbl.mem cache (kind, rs) then begin
                      try return @@ kd_bis orient_rule rs
                      with Abort err_s ->
                        error_msg := Format.sprintf "%s%s\n" !error_msg err_s
                    end;

                    Hashtbl.replace cache (kind, rs) ()
                  in

                  let () =
                    let words =
                      List.map (fun (gen, s) -> Rule.make gen (s ^ word ^ s)) subsets
                    in
                    let rs = rs @ words in

                    if not @@ Hashtbl.mem cache (kind, rs) then begin
                      try return @@ kd_bis orient_rule rs
                      with Abort err_s ->
                        error_msg := Format.sprintf "%s%s\n" !error_msg err_s
                    end;

                    Hashtbl.replace cache (kind, rs) ()
                  in

                  ()
              end
              subsets_words
        end
        orient_rule_list
    in

    List.iter
      begin
        fun (_, w1, w2) ->
          tries w1;
          tries w2
      end
      rs
  in

  (* Si on ne réussit pas, on soulève une erreur *)
  let is_weak = ref false in
  try
    let new_rules = new_rules rs in
    let new_words = new_words rs in
    let new_subset_rules = new_subset_rules rs in
    let new_subset_words = new_subset_words rs in

    if fast then begin
      is_weak := false;
      kd_first_step orient_rule_list;
      kd_second_step orient_rule_list new_rules;
      kd_third_step orient_rule_list new_words;

      is_weak := true;
      kd_first_step weak_orient_rule_list;
      kd_second_step weak_orient_rule_list new_rules;
      kd_third_step weak_orient_rule_list new_words;

      is_weak := false;
      kd_fourth_step orient_rule_list new_subset_rules;
      kd_fifth_step orient_rule_list new_subset_words;

      is_weak := true;
      kd_fourth_step weak_orient_rule_list new_subset_rules;
      kd_fifth_step weak_orient_rule_list new_subset_words
    end
    else begin
      is_weak := false;
      kd_first_step orient_rule_list;
      kd_second_step orient_rule_list new_rules;
      kd_third_step orient_rule_list new_words;
      kd_fourth_step orient_rule_list new_subset_rules;
      kd_fifth_step orient_rule_list new_subset_words;

      is_weak := true;
      kd_first_step weak_orient_rule_list;
      kd_second_step weak_orient_rule_list new_rules;
      kd_third_step weak_orient_rule_list new_words;
      kd_fourth_step weak_orient_rule_list new_subset_rules;
      kd_fifth_step weak_orient_rule_list new_subset_words
    end;

    abort !error_msg
  with Return res ->
    if !is_weak then
      Print.println_warning
      @@ Format.sprintf "Weak completion of :\n%s\nto :\n%s\n" (Printing.to_string rs)
           (Printing.to_string res);

    res
