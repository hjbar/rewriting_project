type word = string

type name = string option

type rule = name * word * word

type rs = rule list

type program = (rs, rs) Hashtbl.t
