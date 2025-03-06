(* IMPORT *)

open Graph_utils

(* GRAPH *)

let write_graph_head out_c =
  output_string out_c "\ndigraph {\n\n";

  output_string out_c "\trankdir=TB;\n";
  output_string out_c "\tnode [shape = ellipse];\n\n"

let write_graph_body out_c rules systems =
  (* On définit des labels pour les noeuds *)
  let ht = Hashtbl.create 16 in
  let cpt = ref ~-1 in

  iter_subsets
    begin
      fun subset ->
        incr cpt;

        let name = Format.sprintf "NODE_%c" @@ Char.chr (Char.code 'a' + !cpt) in
        Hashtbl.replace ht subset name;

        let str = rules_to_string subset in
        output_string out_c @@ "\t" ^ Format.sprintf {|%s [label="%s"];|} name str ^ "\n"
    end
    rules;

  (* Alignement *)
  output_string out_c "\n";

  (* On calcule les arrêtes *)
  iter_subsets
    begin
      fun left_rules ->
        iter_subsets
          begin
            fun right_rules ->
              if left_rules <> right_rules && is_implies left_rules right_rules systems then begin
                let left_node = Hashtbl.find ht left_rules in
                let right_node = Hashtbl.find ht right_rules in

                output_string out_c @@ Format.sprintf "\t%s -> %s;\n" left_node right_node
              end
          end
          rules
    end
    rules

let write_graph_bot out_c = output_string out_c "\n}\n"

(* GRAPH *)

let write_graph out_c rules systems =
  write_graph_head out_c;
  write_graph_body out_c rules systems;
  write_graph_bot out_c

(* MAKE SUBGRAPHS *)

let make_subgraphs dir rules systems =
  (* On récupère le path *)
  let path = Format.sprintf "%s/%s" dir "subgraph.dot" in

  (* Si le fichier existe déjà, on ne re-calcule pas *)
  if not @@ Sys.file_exists path then begin
    (* On ouvre le fichier *)
    let out_c = Utils.open_out_trunc dir path in

    (* On écrit le graphe *)
    write_graph out_c rules systems;

    (* On ferme le fichier *)
    close_out out_c;

    (* Compile *)
    Utils.compile_graph path
  end
