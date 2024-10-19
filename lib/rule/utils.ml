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
