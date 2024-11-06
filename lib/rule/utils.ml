open Def

(* Some utilities functions *)

let make ?(name = "R") w1 w2 : rule = (Some name, w1, w2)

let eq (_, w1, w2) (_, w1', w2') = w1 = w1' && w2 = w2'

(* Génère toutes les règles à partir de mots de taille n avec un alphabet de taille m *)

let get_rules ~alpha_len ~word_len : rule list =
  let words = Word.get_words ~alpha_len ~word_len in

  let idx =
    let cpt = ref ~-1 in
    fun () ->
      incr cpt;
      !cpt
  in

  let rules =
    List.fold_left
      (fun acc w1 ->
        List.fold_left
          (fun acc w2 ->
            if w1 <> w2 then
              let name = Format.sprintf "R%d" @@ idx () in
              make ~name w1 w2 :: acc
            else acc )
          acc words )
      [] words
  in

  List.rev rules

(* Filtre les règles à isomorphisme près *)

let isomorphism_filter rules : rule list =
  let mapping_string word ht gen =
    String.map
      begin
        fun c ->
          match Hashtbl.find_opt ht c with
          | None ->
            let c' = gen () in
            Hashtbl.replace ht c c';
            c'
          | Some c' -> c'
      end
      word
  in

  let mapping_list (name, w1, w2) =
    let char =
      let cpt = ref @@ (Char.code 'a' - 1) in
      fun () ->
        incr cpt;
        Char.chr !cpt
    in

    let ht = Hashtbl.create 16 in

    let w1' = mapping_string w1 ht char in
    let w2' = mapping_string w2 ht char in

    (name, w1', w2')
  in

  rules |> List.map mapping_list
  |> List.sort_uniq (fun (_, w1, w2) (_, w1', w2') -> compare (w1, w2) (w1', w2'))
