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
        if debug_imply then begin
          println_flush sep;
          println_flush
          @@ Format.sprintf "Implications of the rule %s :"
          @@ Rule.to_string source;
          print_newline ()
        end;

        (* On calcule les implications de la règle source *)
        let () =
          try
            let rs = lookup systems source in

            List.iteri
              begin
                fun j ((_, w1, w2) as target) ->
                  try
                    if i <> j then begin
                      let w1' = Rs.normalize ~limit:10000 rs w1 in
                      let w2' = Rs.normalize ~limit:10000 rs w2 in

                      if w1' = w2' then begin
                        (* La target est impliquée par source *)
                        if debug_imply then Rule.println target;
                        update implies source target;

                        (* Les deux règles participent au graphe *)
                        if arr.(i) = false then arr.(i) <- true;
                        if arr.(j) = false then arr.(j) <- true
                      end
                    end
                  with Rs.Abort _ ->
                    println_flush
                    @@ Format.sprintf "In RS %s, the rule %s don't normalize" (Rs.to_string rs)
                         (Rule.to_string target)
              end
              rules
          with Not_found ->
            (* La source n'a pas son système complété *)
            println_flush
            @@ Format.sprintf "The rule %s in not completed yet"
            @@ Rule.to_string source
        in
        if debug_imply then println_flush sep
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
  Graph_hasse.make_hasse dir implies;

  (* On crée le graphe annexe *)
  Graph_annexe.make_annexe dir rules arr;

  (* On crée le sous-graphe homogène *)
  Graph_homogeneous.make_homogeneous dir implies;

  (* On crée le sous-graphe par renversement *)
  Graph_rev.make_rev dir implies;

  (* On crée le sous-graphe à plusieurs relations *)
  Graph_relations.make_relations dir implies;

  (* On crée le graphe des sous-graphs *)
  if List.mem word_len subgraph_lengths then Graph_subgraph.make_subgraphs dir implies

(* MAIN *)

let imply () =
  let run ~alpha_len ~word_len =
    println_newline ();
    println_flush sep2;

    println_flush
    @@ Format.sprintf "Imply for alpha_len = %d and word_len = %d" alpha_len word_len;
    print_newline ();
    compute_all ~alpha_len ~word_len;

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
