(* Outils de debug *)

let debug_success = false

let debug_failed = true

let debug_imply = false

(* Étapes optionnelles *)

let do_test = true

let do_example = false

(* Visualiser les résultats *)

let do_success = false

(* Étapes obligatoires *)

let do_complete = true

let do_imply = true

(* Sep pour affichage *)

let sep = String.make 30 '='

let sep2 = String.make 30 '*'

let sep3 = String.init 30 (fun _ -> '&')

(* Alphabets et longueurs *)

let alpha_lengths = [ 2 ]

let word_lengths = [ 1; 2; 3 ]

(* Limites de calculs *)

let max_norm = 999

let max_pairs = 58

let limit_norm = float max_norm *. 1.5 |> int_of_float

let limit_pairs = float max_pairs *. 1.5 |> int_of_float
