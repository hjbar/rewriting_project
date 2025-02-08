(* Ressources *)

open Print
open Globals

(* Functions *)

let run mode msg f =
  if mode then begin
    println_flush @@ Format.sprintf "%s\n\n%s :" sep3 msg;
    f ();
    println_flush @@ Format.sprintf "\n%s" sep3
  end

(* MAIN *)

let () =
  if not do_main_alt then begin
    run do_test "Tests" Test.test_all;
    run do_example "Examples" Example.ex_knuth_bendix;
    run do_success "Success" Success.success;
    run do_complete "Complete" Complete.complete;
    run do_imply "Imply" Imply.imply
  end
  else Main_alt.main_alt ()
