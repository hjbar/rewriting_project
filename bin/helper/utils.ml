(* IMPORT STUFF *)

open Print
open Globals

(* Fonctions sur le nom des fichiers *)

let get_filename ~alpha_len ~word_len =
  let dir = "archives" in
  let filename = Format.sprintf "alpha_len-%d_word_len-%d.data" alpha_len word_len in
  let path = Format.sprintf "%s/%s" dir filename in
  (dir, filename, path)

(* Fonctions de lecture *)

let open_out_trunc dir file =
  if not @@ Sys.file_exists dir then Sys.mkdir dir 0o777;
  open_out_gen [ Open_wronly; Open_creat; Open_trunc ] 0o666 file

let open_out_append dir file =
  if not @@ Sys.file_exists dir then Sys.mkdir dir 0o777;
  open_out_gen [ Open_wronly; Open_creat; Open_append ] 0o666 file

(* Fonctions d'écriture *)

let output_tab out_c = output_string out_c "\t"

let output_newline out_c = output_string out_c "\n"

let output_rule out_c rule =
  output_string out_c @@ Format.sprintf {|"%s"|} @@ Rule.to_string rule

let output_imply out_c source target =
  output_string out_c
  @@ Format.sprintf {|"%s" -> "%s"|} (Rule.to_string source) (Rule.to_string target)

let write_rule out_c (name, w1, w2) =
  let name = Option.value name ~default:"R" in
  output_string out_c @@ Format.sprintf "%s : %s --> %s ;;\n" name w1 w2

let write_rs out_c rs = List.iter (write_rule out_c) rs

let write_completion out_c rs rs_completed =
  write_rs out_c rs;
  output_string out_c "**\n";
  write_rs out_c rs_completed;
  output_string out_c "##\n"

(* Fonctions sur les règles et la complétion *)

let get_all_rules ~alpha_len ~word_len =
  Rule.get_rules ~alpha_len ~word_len |> Rule.isomorphism_filter

let get_all_rs ~alpha_len ~word_len =
  get_all_rules ~alpha_len ~word_len |> List.map (fun r -> Rs.make [ r ])

let get_rate ~completed ~completion =
  float completed /. float completion *. 100. |> int_of_float

(* Fonctions sur les environnements *)

let lookup ht rule : Rs.rs =
  let rs = Rs.make [ rule ] in
  Hashtbl.find ht rs

(* Fonctions d'affichage *)

let print_res ~rate ~completed ~completion =
  println_data
  @@ Format.sprintf "%d completed systems out of %d, %d%s" completed completion rate "%"

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
