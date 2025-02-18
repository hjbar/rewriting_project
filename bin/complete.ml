(* IMPORT STUFF *)

open Print
open Utils
open Globals

(* EXCEPTION *)

exception Next of (string option * string * string) list

let is_banned =
  (* let l =
    [ (5, [ Some "R418"; Some "R769"; Some "R786"; Some "R1088"; Some "R1307"; Some "R1319" ])
    ] in *)
  let l = [] in
  fun len rs ->
    let name, _, _ = List.hd rs in
    match List.assoc_opt len l with
    | None -> false
    | Some bans -> List.exists (( = ) name) bans

(* COMPLETION FUNCTION *)

let complete_rs ~alpha_len ~word_len =
  (* init *)
  let do_subsets = List.mem word_len subset_gens in
  let excess = excess_limit in
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
            if is_banned word_len rs then raise @@ Next rs;

            incr completion;
            let rs' = Rs.knuth_bendix ~limit_norm ~limit_pairs ~do_subsets ~excess rs in

            incr completed;
            print_completion debug_success rs rs';
            write_completion out_c rs rs'
          with
          | Rs.Abort s -> print_failure debug_failed s rs
          | Next rs ->
            Format.printf "The next system is skiped : ";
            Rs.println rs
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
    print_newline ();
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
