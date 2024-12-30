(* Sep pour affichage *)

let sep = String.make 30 '='

let sep2 = String.make 30 '*'

let sep3 = String.init 30 (fun _ -> '&')

(* Outils de debug *)

let debug_success = false

let debug_failed = true

(* Limites de calculs *)

let limit_norm = 1000

let limit_pairs = 500

(* Étapes optionnelles *)

let do_test = false

let do_example = false

(* Visualiser les résultats *)

let do_success = false

(* Étapes obligatoires *)

let do_complete = true

let do_imply = false
