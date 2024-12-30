open Def

(* Exception quand on échoue dans le calcul *)

exception Abort of string

let abort s = raise @@ Abort s

(* Some utilities functions *)

let make rules : rs = rules
