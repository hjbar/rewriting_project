(* IMPORT STUFF *)

open Print
open Utils
open Globals

(* CALCULE LES IMPLICATIONS *)

let compute_implies rules systems =
  (* Pour stocker les informations concernant les implications *)
  let implies = Hashtbl.create 16 in
  let arr = Array.init (List.length rules) (fun _i -> false) in

  (* On calcule les implications *)
  List.iteri
    begin
      fun i source ->
        (* Affichage pour le terminal *)
        println_flush sep;
        println_flush
        @@ Format.sprintf "Implications of the rule %s :"
        @@ Rule.to_string source;
        print_newline ();

        (* On calcule les implications de la règle source *)
        let () =
          try
            let rs = lookup systems source in

            List.iteri
              begin
                fun j ((_, w1, w2) as target) ->
                  if i <> j then begin
                    let w1' = Rs.normalize rs w1 in
                    let w2' = Rs.normalize rs w2 in

                    if w1' = w2' then begin
                      (* La target est impliquée par source *)
                      Rule.println target;
                      update implies source target;

                      (* Les deux règles participent au graphe *)
                      if arr.(i) = false then arr.(i) <- true;
                      if arr.(j) = false then arr.(j) <- true
                    end
                  end
              end
              rules
          with Not_found ->
            (* La source n'a pas son système complété *)
            println_flush
            @@ Format.sprintf "The rule %s in not completed yet"
            @@ Rule.to_string source
        in
        println_flush sep
    end
    rules;

  (* Renvoie les implications et les équations non utilisées *)
  (implies, arr)

(* SIMPLIFIE LES IMPLICATIONS *)

let simplify_implies implies =
  (* For safety of hashtbl iteration *)
  let remove_elem l elem = l := List.filter (fun elem' -> elem <> elem') !l in
  let tmp = Hashtbl.copy implies in

  (* Simplifie implies by transitivity *)
  Hashtbl.iter
    begin
      fun source targets ->
        let update_targets = ref targets in

        List.iter
          begin
            fun target ->
              match Hashtbl.find_opt tmp target with
              | None -> ()
              | Some tgt_targets -> List.iter (remove_elem update_targets) tgt_targets
          end
          targets;

        Hashtbl.replace implies source !update_targets
    end
    tmp

(* GÉNÈRE LA DIAGRAMME DE HASSE *)

let make_hasse dir implies =
  (* Récupère le path et ouvre le fichier *)
  let path = Format.sprintf "%s/%s" dir "graph.dot" in
  let out_c = open_out_trunc dir path in
  write_graph_head out_c;

  (* Écrit le graphe *)
  Hashtbl.iter
    begin
      fun source targets ->
        List.iter
          begin
            fun target ->
              output_tab out_c;
              output_imply out_c source target;
              output_newline out_c
          end
          targets
    end
    implies;

  (* Ferme le fichier *)
  write_graph_bot out_c;
  close_out out_c;

  (* Génère le pdf *)
  Sys.command
  @@ Format.sprintf "dot -Tpdf %s -o %s" path (Filename.remove_extension path ^ ".pdf")
  |> ignore

(* GÉNÈRE LE GRAPH ANNEXE *)

let make_annexe dir rules arr =
  (* Récupère le path et ouvre le fichier *)
  let path = Format.sprintf "%s/%s" dir "annexe.dot" in
  let out_c = open_out_trunc dir path in
  write_graph_head out_c;

  (* Écrit le graphe *)
  Array.iteri
    begin
      fun i implies ->
        if not implies then begin
          output_tab out_c;
          output_rule out_c @@ List.nth rules i;
          output_newline out_c
        end
    end
    arr;

  (* Ferme le fichier *)
  write_graph_bot out_c;
  close_out out_c;

  (* Génère le pdf *)
  Sys.command
  @@ Format.sprintf "dot -Tpdf %s -o %s" path (Filename.remove_extension path ^ ".pdf")
  |> ignore

(* COMPUTE IMPLIES AND MAKE GRAPHS *)

let compute_all ~alpha_len ~word_len =
  (* On import les systèmes déjà complétés *)
  let dir, filename, _path = get_filename ~alpha_len ~word_len in
  let ht = Parse.parse_file dir filename in
  let rules = get_all_rules ~alpha_len ~word_len in

  (* On calcule les implications *)
  let implies, arr = compute_implies rules ht in

  (* On simplifie les implications *)
  simplify_implies implies;

  (* On crée le Diagramme de Hasse *)
  make_hasse dir implies;

  (* On crée le graphe annexe *)
  make_annexe dir rules arr

(* MAIN *)

let imply () =
  let run ~alpha_len ~word_len =
    println_newline ();
    println_flush sep2;

    println_flush
    @@ Format.sprintf "Imply for alpha_len = %d and word_len = %d" alpha_len word_len;
    compute_all ~alpha_len ~word_len;

    println_flush sep2;
    println_newline ()
  in

  run ~alpha_len:2 ~word_len:1;
  run ~alpha_len:2 ~word_len:2;
  run ~alpha_len:2 ~word_len:3;
  run ~alpha_len:2 ~word_len:4
