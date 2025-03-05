(* IMPORT STUFF *)

open Globals
open Utils
open Print

(* CALCULE LE NOMBRE D'ARRETES *)

let compute_number_egdes implies =
  let nb = Hashtbl.fold (fun _ targets acc -> acc + List.length targets) implies 0 in

  println_flush @@ Format.sprintf "Number of edges : %d" nb

(* CALCULE LE NOMBRE DE FEUILLES *)

let compute_number_leafs rules implies =
  let nb =
    List.fold_left
      (fun acc rule -> acc + match Hashtbl.find_opt implies rule with None -> 1 | Some _ -> 0)
      0 rules
  in

  println_flush @@ Format.sprintf "Number of leafs : %d" nb

(* CALCULE TOUTES LES STATS *)

let compute_all_stats ~alpha_len ~word_len =
  (* On import les systèmes déjà complétés *)
  let dir, filename, _path = get_filename ~alpha_len ~word_len in
  let ht = Parse.parse_file dir filename in
  let rules = get_all_rules ~alpha_len ~word_len in

  (* On récupère les implications *)
  let implies, _arr = Imply.compute_implies rules ht in
  Imply.simplify_implies implies;

  (* On calcul le nombre d'arrêtes *)
  compute_number_egdes implies;

  (* On calcul le nombre de feuilles *)
  compute_number_leafs rules implies

(* MAIN *)

let stats () =
  let run ~alpha_len ~word_len =
    println_newline ();
    println_flush sep2;

    println_flush
    @@ Format.sprintf "Stats for alpha_len = %d and word_len = %d" alpha_len word_len;
    print_newline ();
    compute_all_stats ~alpha_len ~word_len;

    println_flush sep2;
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
