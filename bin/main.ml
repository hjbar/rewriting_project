open Print

let test_fun f =
  let print_sep () = println_flush "------------------------------" in
  print_sep ();
  f ();
  print_sep ()

let () =
  test_fun Test.print_words;
  test_fun Test.print_rules
