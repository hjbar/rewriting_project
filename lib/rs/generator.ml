(* Donne le premier générateur disponible *)

let get_gen_left w =
  let len = String.length w in
  let res = ref [] in

  for i = 0 to len - 1 do
    res := String.sub w i (len - i) :: !res
  done;

  !res

(* Donne la liste des mots possibles pour aller avec un générateur *)

let get_gen_right rs =
  let max_char = ref 'a' in

  List.iter
    begin
      fun (_, w1, w2) ->
        String.iter (fun c -> max_char := max !max_char c) w1;
        String.iter (fun c -> max_char := max !max_char c) w2
    end
    rs;

  Char.code !max_char + 1 |> Char.chr |> Char.escaped
