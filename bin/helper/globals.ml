(* Type d'exec *)

let do_main_alt = false

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

let sep3 = String.make 30 '&'

(* Alphabets et longueurs *)

let alpha_lengths = [ 2 ]

let word_lengths = [ 1; 2; 3; 4; 5 ]

let subgraph_lengths = [ 1; 2 ]

(* Limites de calculs *)

let excess_limit = false

let max_norm = 32

let max_pairs = 20

let limit_norm, limit_pairs =
  let d = if excess_limit then 10 else 2 in
  (max_norm * d, max_pairs * d)
