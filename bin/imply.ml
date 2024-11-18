(* IMPORT STUFF *)

open Print

(* UTILS FUNCTIONS *)

let get_all_rules ~alpha_len ~word_len =
  Rule.get_rules ~alpha_len ~word_len |> Rule.isomorphism_filter

let get_filename ~alpha_len ~word_len =
  let dir = "archives" in
  let filename = Format.sprintf "alpha_len-%d_word_len-%d.data" alpha_len word_len in
  (dir, filename)

let lookup ht rule : Rs.rs =
  let rs = Rs.make [ rule ] in
  Hashtbl.find ht rs

(* SUCCESS FUNCTION *)

let print_implies ~alpha_len ~word_len =
  let sep = String.make 30 '=' in
  let dir, filename = get_filename ~alpha_len ~word_len in
  let ht = Parse.parse_file dir filename in
  let rules = get_all_rules ~alpha_len ~word_len in

  List.iter
    begin
      fun source ->
        println_flush sep;
        println_flush
        @@ Format.sprintf "Implications of the rule %s :"
        @@ Rule.to_string source;
        print_newline ();

        let () =
          try
            let rs = lookup ht source in

            List.iter
              begin
                fun ((_, w1, w2) as target) ->
                  let w1' = Rs.normalize rs w1 in
                  let w2' = Rs.normalize rs w2 in

                  if w1' = w2' then Rule.println target
              end
              rules
          with Not_found ->
            println_flush
            @@ Format.sprintf "The rule %s in not completed yet"
            @@ Rule.to_string source
        in
        println_flush sep
    end
    rules

(* MAIN *)

let imply () =
  let sep = String.make 30 '*' in
  let run ~alpha_len ~word_len =
    println_newline ();
    println_flush sep;

    println_flush
    @@ Format.sprintf "Success for alpha_len = %d and word_len = %d" alpha_len word_len;
    print_implies ~alpha_len ~word_len;

    println_flush sep;
    println_newline ()
  in

  run ~alpha_len:2 ~word_len:1;
  run ~alpha_len:2 ~word_len:2;
  run ~alpha_len:2 ~word_len:3
