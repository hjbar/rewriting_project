open Print

let test_fun f =
  let print_sep () = println_flush "------------------------------" in
  print_sep ();
  f ();
  print_sep ()

let test_all () = List.iter test_fun [ Word.test; Rule.test ]
