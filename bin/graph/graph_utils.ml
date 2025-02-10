(* HOMOGENEOUS *)

let is_homogeneous (_, w1, w2) (_, w1', w2') =
  String.(length w1 = length w2 && length w1' = length w2')

(* REV *)

let rev_string s =
  s |> String.to_seq |> List.of_seq |> List.rev |> List.to_seq |> String.of_seq

let is_rev (_, w1, w1') (_, w2, w2') = rev_string w1 = w1' || rev_string w2 = w2'

(* SUBGRAPHS *)

let iter_subsets f l =
  let rec loop set prefix =
    match set with
    | [] -> f prefix
    | x :: set ->
      loop set (x :: prefix);
      loop set prefix
  in
  loop l []

let get_subsets_args implies =
  let homogeneous =
    let ht = Hashtbl.create 16 in

    Hashtbl.iter
      begin
        fun ((_, w1, w2) as source) targets ->
          if String.(length w1 = length w2) then
            Hashtbl.replace ht source
            @@ List.filter (fun target -> is_homogeneous source target) targets
      end
      implies;

    Hashtbl.iter
      begin
        fun _ targets ->
          List.iter
            (fun target -> if not @@ Hashtbl.mem ht target then Hashtbl.replace ht target [])
            targets
      end
      ht;

    ht
  in

  let assoc =
    let ht = Hashtbl.create 16 in
    let cpt = ref ~-1 in

    Hashtbl.iter
      begin
        fun key _ ->
          incr cpt;
          Hashtbl.replace ht !cpt key
      end
      homogeneous;

    ht
  in

  (assoc, homogeneous)
