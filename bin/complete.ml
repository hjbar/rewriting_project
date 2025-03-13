(* IMPORT STUFF *)

open Print
open Utils
open Globals

(* COMPLETION FUNCTION *)

let complete_rs ~alpha_len ~word_len =
  (* init *)
  let rs_list = get_all_rs ~alpha_len ~word_len in

  let completed_mutex = Mutex.create () in
  let completed = ref 0 in

  let completion_mutex = Mutex.create () in
  let completion = ref 0 in

  let dir, filename, path = get_filename ~alpha_len ~word_len in

  let htbl_mutex = Mutex.create () in
  let htbl = Parse.parse_file dir filename in

  let out_c_mutex = Mutex.create () in
  let out_c = open_out_append dir path in

  (* completion *)
  let domains =
    List.map
      begin
        fun rs ->
          Domain.spawn @@ fun () ->
          Mutex.lock htbl_mutex;
          let opt = Hashtbl.find_opt htbl rs in
          Mutex.unlock htbl_mutex;

          match opt with
          | None -> begin
            try
              Mutex.lock completion_mutex;
              incr completion;
              Mutex.unlock completion_mutex;

              let rs' = Rs.knuth_bendix ~fast ~limit_norm ~limit_pairs rs in

              Mutex.lock completed_mutex;
              incr completed;
              Mutex.unlock completed_mutex;

              print_completion debug_success rs rs';

              Mutex.lock out_c_mutex;
              write_completion out_c rs rs';
              Mutex.unlock out_c_mutex
            with Rs.Abort s -> print_failure debug_failed s rs
          end
          | Some rs' ->
            Mutex.lock completion_mutex;
            incr completion;
            Mutex.unlock completion_mutex;

            Mutex.lock completed_mutex;
            incr completed;
            Mutex.unlock completed_mutex;

            print_completion debug_success rs rs';

            Mutex.lock out_c_mutex;
            write_completion out_c rs rs';
            Mutex.unlock out_c_mutex
      end
      rs_list
  in

  List.iter Domain.join domains;

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
