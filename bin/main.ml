open Print

let print_sep () = println_flush "------------------------------"

let () =
  print_sep ();
  Test.print_words ();
  print_sep ()
