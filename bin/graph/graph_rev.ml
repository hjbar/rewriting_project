(* IMPORT *)

open Utils
open Graph_utils

(* GÉNÈRE LE SOUS-GRAPH REV *)

let make_rev dir implies =
  (* Récupère le path et ouvre le fichier *)
  let path = Format.sprintf "%s/%s" dir "rev.dot" in
  let out_c = open_out_trunc dir path in
  write_graph_head out_c;

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
