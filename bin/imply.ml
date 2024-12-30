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
  let dir, filename, _path = get_filename ~alpha_len ~word_len in
  let ht = Parse.parse_file dir filename in
  let rules = get_all_rules ~alpha_len ~word_len in

  let path = Format.sprintf "%s/%s--%s" dir (Filename.remove_extension filename) "graph.dot" in
  let out_c = open_out_trunc dir path in
  write_graph_head out_c;

  let arr = Array.init (List.length rules) (fun _i -> false) in

  List.iteri
    begin
      fun i source ->
        println_flush sep;
        println_flush
        @@ Format.sprintf "Implications of the rule %s :"
        @@ Rule.to_string source;
        print_newline ();

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
                      Rule.println target;

                      output_tab out_c;
                      output_imply out_c source target;
                      output_newline out_c;

                      if arr.(i) = false then arr.(i) <- true
                    end
                  end
              end
              rules
          with Not_found ->
            println_flush
            @@ Format.sprintf "The rule %s in not completed yet"
            @@ Rule.to_string source
        in
        println_flush sep
    end
    rules;

  write_graph_bot out_c;
  close_out out_c;

  Sys.command
  @@ Format.sprintf "dot -Tpdf %s -o %s" path (Filename.remove_extension path ^ ".pdf")
  |> ignore;

  let path =
    Format.sprintf "%s/%s--%s" dir (Filename.remove_extension filename) "annexe.dot"
  in
  let out_c = open_out_trunc dir path in
  write_graph_head out_c;

  Array.iteri
    (fun i implies ->
      if not implies then begin
        output_tab out_c;
        output_rule out_c @@ List.nth rules i;
        output_newline out_c
      end )
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
    @@ Format.sprintf "Success for alpha_len = %d and word_len = %d" alpha_len word_len;
    print_implies ~alpha_len ~word_len;

    println_flush sep2;
    println_newline ()
  in

  run ~alpha_len:2 ~word_len:1;
  run ~alpha_len:2 ~word_len:2;
  run ~alpha_len:2 ~word_len:3
