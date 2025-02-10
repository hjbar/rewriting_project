(* IMPORT  *)

open Utils

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
