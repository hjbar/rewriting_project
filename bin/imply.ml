(* IMPORT STUFF *)

open Print
open Utils
open Globals

(* EXCEPTION *)

exception Next of Rs.rs * Word.word

let is_banned =
  let l =
    [ ([ (None, "aab", "abbaa") ], "aaaab")
    ; ([ (None, "baa", "aabba") ], "baaaa")
    ; ([ (None, "abb", "bbaab") ], "aabbb")
    ]
  in
  fun rs w ->
    List.exists
      begin
        fun (rs', w') ->
          w = w'
          && List.for_all2 (fun (_, w1, w2) (_, w1', w2') -> w1 = w1' && w2 = w2') rs rs'
      end
      l

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
                    if is_banned rs w1 then raise @@ Next (rs, w1);
                    if is_banned rs w2 then raise @@ Next (rs, w2);

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
          with
          | Not_found ->
            (* La source n'a pas son système complété *)
            println_flush
            @@ Format.sprintf "The rule %s in not completed yet"
            @@ Rule.to_string source
          | Next (rs, w) ->
            (* Implication que l'on éviter de calculer pour une raison ou une autre *)
            println_flush
            @@ Format.sprintf "We go next with the rs : (%s) and the word : (%s)"
                 (Rs.to_string rs) (Word.to_string w)
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
  compile_graph path

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
  compile_graph path

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
  compile_graph path

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
  compile_graph path

(* GÉNÈRE LE GRAPHE À PLUSIEURS RELATIONS *)

let make_relations dir implies =
  let remove_elem l elem = List.filter (fun elem' -> elem <> elem') l in

  (* Récupère le path et ouvre le fichier *)
  let path = Format.sprintf "%s/%s" dir "relations.dot" in
  let out_c = open_out_trunc dir path in
  write_graph_head out_c;

  (* Transformation *)
  let relations = Hashtbl.create 16 in
  Hashtbl.iter
    begin
      fun source targets ->
        let dep, arr =
          List.fold_left
            begin
              fun (dep, arr) target ->
                match Hashtbl.find_opt implies target with
                | None | Some [] -> (target :: dep, arr)
                | _ -> (dep, target :: arr)
            end
            ([ source ], []) targets
        in
        let dep, arr = List.(rev dep, rev arr) in
        Hashtbl.replace relations dep arr
    end
    implies;

  let index = Hashtbl.create 16 in
  List.iter
    begin
      fun l -> Hashtbl.replace index (List.hd l) l
    end
    (relations |> Hashtbl.to_seq_keys |> List.of_seq);

  let new_relations = Hashtbl.create 16 in
  Hashtbl.iter
    begin
      fun sources targets ->
        List.iter
          begin
            fun target ->
              match Hashtbl.find_opt index target with
              | None -> ()
              | Some l -> begin
                Hashtbl.replace relations sources
                @@ remove_elem (Hashtbl.find relations sources) target;
                match Hashtbl.find_opt new_relations sources with
                | None -> Hashtbl.replace new_relations sources [ l ]
                | Some l' -> Hashtbl.replace new_relations sources (l :: l')
              end
          end
          targets
    end
    relations;

  (* Écrit le graph *)
  Hashtbl.iter
    begin
      fun sources targets ->
        match targets with
        | [] ->
          output_tab out_c;
          output_relations out_c sources;
          output_newline out_c
        | targets ->
          output_tab out_c;
          output_both_relations out_c sources targets;
          output_newline out_c
    end
    relations;

  Hashtbl.iter
    begin
      fun sources targets ->
        match targets with
        | [] ->
          output_tab out_c;
          output_relations out_c sources;
          output_newline out_c
        | targets ->
          List.iter
            (fun targets ->
              output_tab out_c;
              output_both_relations out_c sources targets;
              output_newline out_c )
            targets
    end
    new_relations;

  (* Ferme le fichier *)
  write_graph_bot out_c;
  close_out out_c;

  (* Génère le pdf *)
  compile_graph path

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
