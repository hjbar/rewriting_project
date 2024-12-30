(* IMPORT STUFF *)

open Print
open Utils
open Globals

(* SUCCESS FUNCTION *)

let print_implies ~alpha_len ~word_len =
  let dir, filename, _path = get_filename ~alpha_len ~word_len in
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
  let run ~alpha_len ~word_len =
    println_newline ();
    println_flush sep2;

    println_flush
    @@ Format.sprintf "Success for alpha_len = %d and word_len = %d" alpha_len word_len;
    print_implies ~alpha_len ~word_len;

    println_flush sep2;
    println_newline ()
  in

  run ~alpha_len:2 ~word_len:1;
  run ~alpha_len:2 ~word_len:2;
  run ~alpha_len:2 ~word_len:3
