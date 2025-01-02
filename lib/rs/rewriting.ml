open Utils
open Order
open Normalize

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

          let w1' = normalize rs w1 in
          let w2' = normalize rs w2 in

          if not @@ Word.eq w1' w2' then add @@ make_rule w1' w2'
      end
      critical_pairs
  done;

  (* On renvoie le système complété *)
  !rules

let knuth_bendix ?(limit_norm = max_int) ?(limit_pairs = max_int) rs =
  (* On vérifie que le rs n'est pas vide *)
  if rs = [] then abort "Le système de réécriture est vide";

  (* On initialise quelques fonctions *)
  let normalize = normalize ~limit:limit_norm in
  let critical_rules = critical_rules ~limit:limit_norm in
  let kd_bis = knuth_bendix_bis ~limit_pairs normalize critical_rules in

  (* Message d'erreur en cas d'échec *)
  let error_msg = ref "" in

  (* On teste différentes orientations *)
  let kd_first_step () =
    List.iter
      begin
        fun orient_rule ->
          try return @@ kd_bis orient_rule rs
          with Abort s -> error_msg := Format.sprintf "%s%s\n" !error_msg s
      end
      orient_rule_list
  in

  (* On teste des générateurs *)
  let get_gen_left w =
    let len = String.length w in
    let res = ref [] in

    for i = 0 to len - 1 do
      res := String.sub w i (len - i) :: !res
    done;

    !res
  in

  let get_gen_right rs =
    let max_char = ref 'a' in

    List.iter
      begin
        fun (_, w1, w2) ->
          String.iter (fun c -> max_char := max !max_char c) w1;
          String.iter (fun c -> max_char := max !max_char c) w2
      end
      rs;

    Char.code !max_char + 1 |> Char.chr |> Char.escaped
  in

  let kd_second_step () =
    let gen_right = get_gen_right rs in

    let tries gen_left =
      List.iter
        begin
          fun orient_rule ->
            List.iter
              begin
                fun s ->
                  let rs = rs @ [ Rule.make s gen_right ] in
                  try return @@ kd_bis orient_rule rs
                  with Abort s -> error_msg := Format.sprintf "%s%s\n" !error_msg s
              end
              gen_left
        end
        orient_rule_list
    in

    let _, w1, w2 = List.hd rs in
    tries @@ get_gen_left w1;
    tries @@ get_gen_left w2
  in

  (* Si on ne réussit pas, on soulève une erreur *)
  try
    kd_first_step ();
    kd_second_step ();

    abort !error_msg
  with Return rs -> rs
