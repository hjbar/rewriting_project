open Print

let draft () =
  let sep = String.make 30 '=' in
  let debug = false in

  let rs_list =
    Rule.get_rules ~alpha_len:2 ~word_len:3
    |> Rule.isomorphism_filter
    |> List.map (fun r -> Rs.make [ r ])
  in

  let completed = ref 0 in
  let completion = ref 0 in

  let limit_norm = 1000 in
  let limit_pairs = 500 in

  List.iter
    begin
      fun rs ->
        try
          incr completion;
          let rs' = Rs.knuth_bendix ~limit_norm ~limit_pairs rs in
          incr completed;

          println_flush sep;
          println_flush "Système de réécriture avant :";
          Rs.println rs;
          print_newline ();
          println_flush "Système de réécriture après :";
          Rs.println rs';
          print_newline ();
          println_ok "COMPLETED";
          println_flush sep
        with
        | Rs.Abort s ->
          if debug then begin
            println_flush sep;

            println_flush "Système de réécriture avant :";
            Rs.println rs;
            print_newline ();
            println_warning s;

            println_flush sep
          end
        | exn -> raise exn
    end
    rs_list;

  let rate = float !completed /. float !completion *. 100. |> int_of_float in
  println_data
  @@ Format.sprintf "%d completed systems out of %d, %d%s" !completed !completion rate "%"
