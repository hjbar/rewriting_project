(* IMPORT  *)

open Utils

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
