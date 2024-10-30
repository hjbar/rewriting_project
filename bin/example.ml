open Print

let ex_knuth_bendix () =
  let rs =
    Rs.make [ Rule.make "ab" "c"; Rule.make "bc" "a"; Rule.make "ca" "b"; Rule.make "ba" "c" ]
  in
  let rs' = Rs.knuth_bendix rs in

  println_flush "Le système de réécriture de base :";
  Rs.println rs;
  println_flush "Le système après la complétion de Knuth-Bendix :";
  Rs.print rs'