(* IMPORT *)

open Utils

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
