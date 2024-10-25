open Print

let draft () =
  let rules = Rule.get_rules ~alpha_len:2 ~word_len:5 in
  let soft_rules = List.filter (fun (_, w1, w2) -> String.(length w1 >= length w2)) rules in
  let rss = List.map (fun r -> Rs.make [ r ]) soft_rules in
  let krs = List.map Rs.knuth_bendix rss in
  List.iter
    (fun rs ->
      println_flush "Système de réécriture complété :";
      Rs.println rs )
    krs
