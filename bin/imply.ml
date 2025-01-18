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
                  if i <> j then begin
                    let w1' = Rs.normalize rs w1 in
                    let w2' = Rs.normalize rs w2 in

                    if w1' = w2' then begin
                      (* La target est impliquée par source *)
                      if debug_imply then Rule.println target;
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

(* GÉNÈRE LE SOUS-GRAPH HOMOGÈNE *)

let make_homogeneous dir implies =
  (* Récupère le path et ouvre le fichier *)
  let path = Format.sprintf "%s/%s" dir "homogeneous.dot" in
  let out_c = open_out_trunc dir path in
  write_graph_head out_c;

  (* Fonction pour savoir si on a une implication homogène *)
  let is_homogeneous (_, w1, w2) (_, w1', w2') =
    String.(length w1 = length w2 && length w1' = length w2')
  in

  (* Écrit le graph *)
  Hashtbl.iter
    begin
      fun source targets ->
        List.iter
          begin
            fun target ->
              if is_homogeneous source target then begin
                output_tab out_c;
                output_imply out_c source target;
                output_newline out_c
              end
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

(* GÉNÈRE LE SOUS-GRAPH REV *)

let make_rev dir implies =
  (* Récupère le path et ouvre le fichier *)
  let path = Format.sprintf "%s/%s" dir "rev.dot" in
  let out_c = open_out_trunc dir path in
  write_graph_head out_c;

  (* Renverse la string *)
  let rev_string s =
    s |> String.to_seq |> List.of_seq |> List.rev |> List.to_seq |> String.of_seq
  in

  (* Fonction pour savoir si on a une équation par renversement *)
  let is_rev (_, w1, w1') (_, w2, w2') = rev_string w1 = w1' || rev_string w2 = w2' in

  (* Écrit le graph *)
  Hashtbl.iter
    begin
      fun source targets ->
        List.iter
          begin
            fun target ->
              if is_rev source target then begin
                output_tab out_c;
                output_imply out_c source target;
                output_newline out_c
              end
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

(* GÉNÈRE LE GRAPHE À PLUSIEURS RELATIONS *)

let rec distance rules implies =
  let loop cpt =
    match rules with
    | [] -> cpt
    | l ->
      cpt
      + List.fold_left
          (fun acc target ->
            let dist =
              match Hashtbl.find_opt implies target with
              | None -> acc
              | Some targets -> distance targets implies
            in
            if dist > acc then dist else acc )
          0 l
  in
  loop 1

let give_relations implies =
  let relations = Hashtbl.create 16 in
  Hashtbl.iter
    (fun source targets -> Hashtbl.replace relations (ref [ source ]) (ref targets))
    implies;
  relations

let simplify_relations relations implies =
  Hashtbl.iter
    begin
      fun sources targets ->
        if distance !targets implies = 1 then begin
          sources := List.hd !targets :: !sources;
          targets := []
        end
    end
    relations

let make_relations dir implies =
  (* Récupère le path et ouvre le fichier *)
  let path = Format.sprintf "%s/%s" dir "relations.dot" in
  let out_c = open_out_trunc dir path in
  write_graph_head out_c;

  (* Transformation *)
  let relations = give_relations implies in
  simplify_relations relations implies;

  (* Écrit le graph *)
  Hashtbl.iter
    begin
      fun sources targets ->
        match !targets with
        | [] -> ()
        | targets ->
          output_tab out_c;
          output_relations out_c !sources targets;
          output_newline out_c
    end
    relations;

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
  make_annexe dir rules arr;

  (* On crée le sous-graphe homogène *)
  make_homogeneous dir implies;

  (* On crée le sous-graphe par renversement *)
  make_rev dir implies;

  (* On crée le sous-graphe à plusieurs relations *)
  make_relations dir implies

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
