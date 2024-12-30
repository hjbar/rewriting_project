open Def

(* Exception quand on échoue dans le calcul *)

exception Abort of string

let abort s = raise @@ Abort s

(* Renvoie anticipé *)

exception Return of rs

let return res = raise @@ Return res

(* Some utilities functions *)

let make rules : rs = rules
