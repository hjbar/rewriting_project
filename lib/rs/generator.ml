(* Donne le premier générateur disponible *)

let get_gen_left w =
  let len = String.length w in
  let res = ref [] in

  for i = 0 to len - 1 do
    res := String.sub w i (len - i) :: !res
  done;

  List.rev !res

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

(* Ajoute tous les générateurs au système de réécriture *)

let add_all_gen rs =
  let cache = Hashtbl.create 16 in
  let res = ref [] in

  let new_gen =
    let gen = ref @@ get_gen_right rs in
    fun () ->
      let tmp = !gen in
      gen := Char.code (String.get tmp 0) + 1 |> Char.chr |> Char.escaped;
      tmp
  in

  let update w =
    List.iter
      begin
        fun s ->
          if not @@ Hashtbl.mem cache s then begin
            Hashtbl.replace cache s ();
            res := Rule.make s (new_gen ()) :: !res
          end
      end
      (get_gen_left w)
  in

  List.iter
    begin
      fun (_, w1, w2) ->
        update w1;
        update w2
    end
    rs;

  rs @ List.rev !res
