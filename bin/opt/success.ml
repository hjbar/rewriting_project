(* IMPORT STUFF *)

open Print
open Utils
open Globals

(* SUCCESS FUNCTION *)

let print_success ~alpha_len ~word_len =
  let sep = String.make 30 '=' in
  let dir, filename, _path = get_filename ~alpha_len ~word_len in
  let ht = Parse.parse_file dir filename in

  Hashtbl.iter
    begin
      fun rs rs' ->
        println_flush sep;
        println_flush "Système de réécriture avant :";

        Rs.println rs;

        print_newline ();
        println_flush "Système de réécriture après :";

        Rs.println rs';

        println_flush sep
    end
    ht

(* MAIN *)

let success () =
  let run ~alpha_len ~word_len =
    println_newline ();
    println_flush sep2;

    println_flush
    @@ Format.sprintf "Success for alpha_len = %d and word_len = %d" alpha_len word_len;
    print_success ~alpha_len ~word_len;

    println_flush sep2;
    println_newline ()
  in

  run ~alpha_len:2 ~word_len:1;
  run ~alpha_len:2 ~word_len:2;
  run ~alpha_len:2 ~word_len:3
