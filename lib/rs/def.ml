(* Définition d'un système de ré-écriture *)

type orient =
  | Left
  | Right

type rs =
  { orient : orient
  ; rules : Rule.rule list
  }
