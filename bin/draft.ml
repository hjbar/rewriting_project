let example () =
  let rs =
    Rs.make
      [ Rule.make "ababa" "bcbc"
      ; Rule.make "c" "d"
      ; Rule.make "d" "be"
      ; Rule.make "e" "f"
      ; Rule.make "f" "g"
      ]
  in
  let rs = Rs.knuth_bendix rs in
  Rs.println rs
