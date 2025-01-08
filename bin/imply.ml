(* IMPORT STUFF *)

open Print
open Utils
open Globals

(* GRAPHVIZ *)

let write_graph_head out_c =
  output_string out_c "digraph finite_state_machine {";
  output_newline out_c;

  output_tab out_c;
  output_string out_c "rankdir=TB;";
  output_newline out_c;

  output_tab out_c;
  output_string out_c "node [shape = ellipse];";
  output_newline out_c

let write_graph_bot out_c = output_string out_c "}"

(* SUCCESS FUNCTION *)

let print_implies ~alpha_len ~word_len =
  (* On import les systèmes déjà complétés *)
  let dir, filename, _path = get_filename ~alpha_len ~word_len in
  let ht = Parse.parse_file dir filename in
  let rules = get_all_rules ~alpha_len ~word_len in

  (* Pour stocker les informations concernant les implications *)
  let implies = Hashtbl.create 16 in
  let arr = Array.init (List.length rules) (fun _i -> false) in

  (* On calcule les implications *)
  List.iteri
    begin
      fun i source ->
        (* Affichage pour le terminal *)
        println_flush sep;
        println_flush
        @@ Format.sprintf "Implications of the rule %s :"
        @@ Rule.to_string source;
        print_newline ();

        (* On calcule les implications de la règle source *)
        let () =
          try
            let rs = lookup ht source in

            List.iteri
              begin
                fun j ((_, w1, w2) as target) ->
                  if i <> j then begin
                    let w1' = Rs.normalize rs w1 in
                    let w2' = Rs.normalize rs w2 in

                    if w1' = w2' then begin
                      (* La target est impliquée par source *)
                      Rule.println target;
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
        println_flush sep
    end
    rules;

  (* On simplifie les implications *)
  let remove_elem l elem = l := List.filter (fun elem' -> elem <> elem') !l in
  let tmp = Hashtbl.copy implies in

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
    tmp;

  (* On crée le Diagramme de Hasse *)
  let path = Format.sprintf "%s/%s--%s" dir (Filename.remove_extension filename) "graph.dot" in
  let out_c = open_out_trunc dir path in
  write_graph_head out_c;

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

  write_graph_bot out_c;
  close_out out_c;

  Sys.command
  @@ Format.sprintf "dot -Tpdf %s -o %s" path (Filename.remove_extension path ^ ".pdf")
  |> ignore;

  (* On crée le graphe annexe *)
  let path =
    Format.sprintf "%s/%s--%s" dir (Filename.remove_extension filename) "annexe.dot"
  in
  let out_c = open_out_trunc dir path in
  write_graph_head out_c;

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

  write_graph_bot out_c;
  close_out out_c;

  Sys.command
  @@ Format.sprintf "dot -Tpdf %s -o %s" path (Filename.remove_extension path ^ ".pdf")
  |> ignore

(* MAIN *)

let imply () =
  let run ~alpha_len ~word_len =
    println_newline ();
    println_flush sep2;

    println_flush
    @@ Format.sprintf "Imply for alpha_len = %d and word_len = %d" alpha_len word_len;
    print_implies ~alpha_len ~word_len;

    println_flush sep2;
    println_newline ()
  in

  run ~alpha_len:2 ~word_len:1;
  run ~alpha_len:2 ~word_len:2;
  run ~alpha_len:2 ~word_len:3;
  run ~alpha_len:2 ~word_len:4
