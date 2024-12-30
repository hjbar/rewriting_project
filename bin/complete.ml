(* IMPORT STUFF *)

open Print
open Utils
open Globals

(* COMPLETION FUNCTION *)

let complete_rs ~alpha_len ~word_len =
  (* init *)
  let rs_list = get_all_rs ~alpha_len ~word_len in
  let completed, completion = (ref 0, ref 0) in

  let dir, filename, path = get_filename ~alpha_len ~word_len in
  let htbl = Parse.parse_file dir filename in
  let out_c = open_out_file dir path in

  (* completion *)
  List.iter
    begin
      fun rs ->
        if Hashtbl.mem htbl rs then begin
          incr completion;
          incr completed;

          let rs' = Hashtbl.find htbl rs in
          print_completion debug_success rs rs';
          write_completion out_c rs rs'
        end
        else begin
          try
            incr completion;
            let rs' = Rs.knuth_bendix ~limit_norm ~limit_pairs rs in

            incr completed;
            print_completion debug_success rs rs';
            write_completion out_c rs rs'
          with
          | Rs.Abort s -> print_failure debug_failed s rs
          | exn -> raise exn
        end
    end
    rs_list;

  (* return *)
  close_out out_c;

  let completed, completion = (!completed, !completion) in
  let rate = get_rate ~completed ~completion in
  print_res ~rate ~completed ~completion

(* MAIN *)

let complete () =
  println_flush sep;
  complete_rs ~alpha_len:2 ~word_len:1;
  println_flush sep;
  complete_rs ~alpha_len:2 ~word_len:2;
  println_flush sep;
  complete_rs ~alpha_len:2 ~word_len:3;
  println_flush sep
