(* IMPORT *)

open Graph_utils

(* SUBGRAPH *)

let write_subgraph_head out_c n =
  let n = string_of_int n in

  output_string out_c @@ "\tsubgraph cluster_" ^ n ^ " {\n\n";

  output_string out_c @@ "\t\t" ^ {|label = "subgraph_|} ^ n ^ {|";|} ^ "\n"

let write_subgraph_bot out_c = output_string out_c "\n\t}\n\n"

let write_subgraph_body out_c n offsets assoc implies =
  match offsets with
  | [] -> output_string out_c @@ "\t\t" ^ Format.sprintf {|""|} ^ "\n"
  | l ->
    List.iter
      begin
        fun offset ->
          let ((_, sw1, sw2) as source) = Hashtbl.find assoc offset in

          match Hashtbl.find implies source with
          | [] ->
            output_string out_c @@ "\t\t" ^ Format.sprintf {|"%d: %s = %s"|} n sw1 sw2 ^ "\n"
          | l ->
            let flag = ref true in

            List.iter
              begin
                fun ((_, tw1, tw2) as target) ->
                  if List.exists (fun ofs -> Hashtbl.find assoc ofs = target) offsets then begin
                    flag := false;

                    output_string out_c @@ "\t\t"
                    ^ Format.sprintf {|"%d: %s = %s" -> "%d: %s = %s"|} n sw1 sw2 n tw1 tw2
                    ^ "\n"
                  end
              end
              l;

            if !flag then
              output_string out_c @@ "\t\t" ^ Format.sprintf {|"%d: %s = %s"|} n sw1 sw2 ^ "\n"
      end
      l

let write_subgraph out_c n ofs asc imp =
  write_subgraph_head out_c n;
  write_subgraph_body out_c n ofs asc imp;
  write_subgraph_bot out_c

(* GRAPH *)

let write_graph_head out_c =
  output_string out_c "digraph {\n\n";

  output_string out_c "\trankdir=TB;\n";
  output_string out_c "\tnode [shape = ellipse];\n\n"

let write_graph_bot out_c = output_string out_c "}\n"

let write_graph_body out_c assoc implies =
  let len = Hashtbl.length implies in
  let cpt = ref ~-1 in

  iter_subsets
    begin
      fun subset ->
        incr cpt;
        write_subgraph out_c !cpt subset assoc implies
    end
    (List.init len Fun.id)

let write_graph out_c assoc implies =
  write_graph_head out_c;
  write_graph_body out_c assoc implies;
  write_graph_bot out_c

(* MAKE SUBGRAPHS *)

let make_subgraphs dir implies =
  (* On récupère les args pour écrire le graph *)
  let assoc, homogeneous = get_subsets_args implies in

  (* On récupère le path et on ouvre le fichier *)
  let path = Format.sprintf "%s/%s" dir "subgraph.dot" in
  let out_c = Utils.open_out_trunc dir path in

  (* On écrit le graphe *)
  write_graph out_c assoc homogeneous;

  (* Ferme le fichier *)
  close_out out_c;

  (* Compile *)
  Utils.compile_graph path
