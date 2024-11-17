open Print

let complete_rs ~alpha_len ~word_len =
  let sep = String.make 30 '=' in
  let debug_success = false in
  let debug_failed = true in

  let rs_list =
    Rule.get_rules ~alpha_len ~word_len
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

          if debug_success then begin
            println_flush sep;
            println_flush "Système de réécriture avant :";
            Rs.println rs;
            print_newline ();
            println_flush "Système de réécriture après :";
            Rs.println rs';
            print_newline ();
            println_ok "COMPLETED";
            println_flush sep
          end
        with
        | Rs.Abort s ->
          if debug_failed then begin
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
  @@ Format.sprintf "%d completed systems out of %d, %d%s" !completed !completion rate "%";

  rate

let draft () =
  let sep = String.make 30 '*' in

  let run ~alpha_len ~word_len =
    println_flush
    @@ Format.sprintf "Try to complete with alpha_len=%d and word_len=%d :" alpha_len word_len;
    let rate = complete_rs ~alpha_len ~word_len in

    if rate <> 100 then begin
      println_error
      @@ Format.sprintf "\nalpha_len=%d ; word_len=%d ==> %d%s\n" alpha_len word_len rate "%"
    end
  in

  println_flush sep;
  run ~alpha_len:2 ~word_len:1;
  println_flush sep;
  run ~alpha_len:2 ~word_len:2;
  println_flush sep;
  run ~alpha_len:2 ~word_len:3;
  println_flush sep
