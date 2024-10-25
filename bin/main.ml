open Print

let () =
  Test.test_all ();
  println_flush "--- --- --- --- ---\nExamples :";
  Example.ex_knuth_bendix ();
  println_flush "--- --- --- --- ---\nDrafts :";
  Draft.draft ()
