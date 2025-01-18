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
  let out_c = open_out_append dir path in

  (* completion *)
  List.iter
    begin
      fun rs ->
        match Hashtbl.find_opt htbl rs with
        | None -> begin
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
        | Some rs' ->
          incr completion;
          incr completed;

          print_completion debug_success rs rs';
          write_completion out_c rs rs'
    end
    rs_list;

  (* return *)
  close_out out_c;

  let completed, completion = (!completed, !completion) in
  let rate = get_rate ~completed ~completion in
  print_res ~rate ~completed ~completion

(* MAIN *)

let complete () =
  let run ~alpha_len ~word_len =
    println_newline ();
    println_flush sep;

    println_flush
    @@ Format.sprintf "Complete for alpha_len = %d and word_len = %d" alpha_len word_len;
    complete_rs ~alpha_len ~word_len;

    println_flush sep;
    println_newline ()
  in

  List.iter
    begin
      fun alpha_len ->
        List.iter
          begin
            fun word_len -> run ~alpha_len ~word_len
          end
          word_lengths
    end
    alpha_lengths
