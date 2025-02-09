(* GRAPH TO ALL SUBGRAPH *)

let rec subsets : (Rule.rule * Rule.rule) list -> (Rule.rule * Rule.rule) list list = function
  | [] -> [ [] ]
  | x :: l ->
    let l1 = subsets l in
    let l2 = List.map (fun subset -> x :: subset) l1 in
    l1 @ l2

let inline (l : (Rule.rule * Rule.rule list) list) : (Rule.rule * Rule.rule) list =
  List.fold_left (fun acc (r, implies) -> acc @ List.map (fun r' -> (r, r')) implies) [] l

let graph_to_subgraphs (implies : (Rule.rule, Rule.rule list) Hashtbl.t) :
  (Rule.rule * Rule.rule) list list =
  implies |> Hashtbl.to_seq |> List.of_seq |> inline |> subsets

(* SUBGRAPH *)

let write_subgraph_head out_c n =
  let n = string_of_int n in

  output_string out_c @@ "\tsubgraph cluster_" ^ n ^ " {\n\n";

  output_string out_c @@ "\t\t" ^ {|label = "subgraph_|} ^ n ^ {|";|} ^ "\n"

let write_subgraph_bot out_c = output_string out_c "\n\t}\n\n"

let write_subgraph_body out_c n l =
  List.iter
    begin
      fun ((_, sw1, sw2), (_, tw1, tw2)) ->
        output_string out_c @@ "\t\t"
        ^ Format.sprintf {|"%d: %s = %s" -> "%d: %s = %s"|} n sw1 sw2 n tw1 tw2
        ^ "\n"
    end
    l

let write_subgraph out_c n l =
  write_subgraph_head out_c n;
  write_subgraph_body out_c n l;
  write_subgraph_bot out_c

(* GRAPH *)

let write_graph_head out_c =
  output_string out_c "digraph {\n\n";

  output_string out_c "\trankdir=TB;\n";
  output_string out_c "\tnode [shape = ellipse];\n\n"

let write_graph_bot out_c = output_string out_c "}\n"

let write_graph_body out_c g = List.iteri (fun i implies -> write_subgraph out_c i implies) g

let write_graph out_c implies =
  let l = graph_to_subgraphs implies in

  write_graph_head out_c;
  write_graph_body out_c l;
  write_graph_bot out_c
