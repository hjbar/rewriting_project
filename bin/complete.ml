(* IMPORT STUFF *)

open Print

(* FILE FUNCTIONS *)

let write_rule out_c (name, w1, w2) =
  let name = Option.value name ~default:"R" in
  output_string out_c @@ Format.sprintf "%s : %s --> %s ;;\n" name w1 w2

let write_rs out_c rs = List.iter (write_rule out_c) rs

let write_completion out_c rs rs_completed =
  write_rs out_c rs;
  output_string out_c "**\n";
  write_rs out_c rs_completed;
  output_string out_c "##\n"

(* SOME CONSTANTS *)

let sep = String.make 30 '='

let debug_success = false

let debug_failed = true

let limit_norm = 1000

let limit_pairs = 500

(* UTILS FUNCTIONS *)

let get_all_rs ~alpha_len ~word_len =
  Rule.get_rules ~alpha_len ~word_len
  |> Rule.isomorphism_filter
  |> List.map (fun r -> Rs.make [ r ])

let get_rate ~completed ~completion =
  float completed /. float completion *. 100. |> int_of_float

let print_res ~rate ~completed ~completion =
  println_data
  @@ Format.sprintf "%d completed systems out of %d, %d%s" completed completion rate "%"

let get_filename ~alpha_len ~word_len =
  let dir = "archives" in
  let filename = Format.sprintf "alpha_len-%d_word_len-%d.data" alpha_len word_len in
  let file = Format.sprintf "%s/%s" dir filename in
  (dir, filename, file)

let open_out_file dir file =
  if not @@ Sys.file_exists dir then Sys.mkdir dir 0o777;
  open_out_gen [ Open_wronly; Open_creat ] 0o666 file

let print_completion flag rs rs_completed =
  if flag then begin
    println_flush sep;

    println_flush "Système de réécriture avant :";
    Rs.println rs;
    print_newline ();

    println_flush "Système de réécriture après :";
    Rs.println rs_completed;
    print_newline ();

    println_ok "COMPLETED";
    println_flush sep
  end

let print_failure flag msg rs =
  if flag then begin
    println_flush sep;

    println_flush "Système de réécriture avant :";
    Rs.println rs;
    print_newline ();

    println_warning msg;
    println_flush sep
  end

(* COMPLETION FUNCTION *)

let complete_rs ~alpha_len ~word_len =
  (* init *)
  let rs_list = get_all_rs ~alpha_len ~word_len in
  let completed, completion = (ref 0, ref 0) in

  let dir, filename, file = get_filename ~alpha_len ~word_len in
  let htbl = Parse.parse_file dir filename in
  let out_c = open_out_file dir file in

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
