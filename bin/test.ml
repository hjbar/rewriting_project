open Print

(* Afficher les mots de taille 1, 2 et 3 *)

let print_words () =
  println_flush "Mots de taille 0 avec alpha de taille 16 :";
  List.iter println_flush (Word.get_words ~alpha_len:16 ~word_len:0);

  println_newline ();

  println_flush "Mots de taille 16 avec alpha de taille 0 :";
  List.iter println_flush (Word.get_words ~alpha_len:0 ~word_len:16);

  println_newline ();

  println_flush "Mots de taille 1 avec alpha de taille 1 :";
  List.iter println_flush (Word.get_words ~alpha_len:1 ~word_len:1);

  println_newline ();

  println_flush "Mots de taille 5 avec alpha de taille 1 :";
  List.iter println_flush (Word.get_words ~alpha_len:1 ~word_len:5);

  println_newline ();

  println_flush "Mots de taille 2 avec alpha de taille 2 :";
  List.iter println_flush (Word.get_words ~alpha_len:2 ~word_len:2);

  println_newline ();

  println_flush "Mots de taille 1 avec alpha de taille 3 :";
  List.iter println_flush (Word.get_words ~alpha_len:3 ~word_len:1);

  println_newline ();

  println_flush "Mots de taille 2 avec alpha de taille 3 :";
  List.iter println_flush (Word.get_words ~alpha_len:3 ~word_len:2);

  println_newline ();

  println_flush "Mots de taille 3 avec alpha de taille 3 :";
  List.iter println_flush (Word.get_words ~alpha_len:3 ~word_len:3);

  println_newline ();

  let () =
    try Word.get_words ~alpha_len:3 ~word_len:~-1 |> ignore
    with exn ->
      begin
        println_flush @@ Printexc.to_string exn;
        println_ok "Succes"
      end
  in

  println_newline ();

  let () =
    try Word.get_words ~alpha_len:~-1 ~word_len:5 |> ignore
    with exn ->
      begin
        println_flush @@ Printexc.to_string exn;
        println_ok "Succes"
      end
  in

  println_newline ();

  let () =
    try Word.get_words ~alpha_len:27 ~word_len:5 |> ignore
    with exn ->
      begin
        println_flush @@ Printexc.to_string exn;
        println_ok "Succes"
      end
  in

  ()

(* Afficher les règles engendrées par les mots *)

let print_rules () =
  let rules = Rule.get_rules ~alpha_len:3 ~word_len:3 in
  List.iter Rule.println rules
