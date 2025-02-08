let len4_failed =
  Rs.
    [ make [ (Some "R216", "ab", "baba") ]
    ; make [ (Some "R221", "ab", "bbab") ]
    ; make [ (Some "R234", "aba", "baab") ]
    ; make [ (Some "R265", "abab", "abba") ]
    ; make [ (Some "R271", "abab", "baab") ]
    ; make [ (Some "R306", "abba", "baba") ]
    ]

let do_nb = max_int

let main_alt () =
  List.iteri
    begin
      fun i rs ->
        if i < do_nb then begin
          Print.println_flush "";
          Print.println_flush @@ Format.sprintf "Complétion de %s :" (Rs.to_string rs);

          try
            Rs.knuth_bendix ~limit_norm:(2 * Globals.limit_norm) rs
              ~limit_pairs:(2 * Globals.limit_pairs)
            |> ignore
          with Rs.Abort s -> Print.println_flush @@ Format.sprintf "Failed : \n%s" s
        end
    end
    len4_failed
