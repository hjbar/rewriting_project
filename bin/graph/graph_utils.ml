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

let rules_to_string = function
  | [] -> "∅"
  | rules ->
    List.fold_left
      (fun acc rule -> acc ^ Format.sprintf {|%s\n|} (Rule.to_string rule))
      "" rules

let is_implies left_rules right_rules systems =
  List.for_all
    begin
      fun (_, w1, w2) ->
        List.exists
          begin
            fun left_rule ->
              let rs = Utils.lookup systems left_rule in

              let w1' = Rs.normalize rs w1 in
              let w2' = Rs.normalize rs w2 in

              w1' = w2'
          end
          left_rules
    end
    right_rules
