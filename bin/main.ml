(* Ressources *)

open Print

(* Constantes *)

let do_test = true

let do_example = false

let do_success = false

let do_complete = true

let do_imply = false

(* Functions *)

let run mode msg f =
  let sep = String.init 30 (fun _ -> '&') in
  if mode then begin
    println_flush @@ Format.sprintf "%s\n\n%s :" sep msg;
    f ();
    println_flush @@ Format.sprintf "\n%s" sep
  end

(* MAIN *)

let () =
  run do_test "Tests" Test.test_all;
  run do_example "Examples" Example.ex_knuth_bendix;
  run do_success "Success" Success.success;
  run do_complete "Complete" Complete.complete;
  run do_imply "Imply" Imply.imply
