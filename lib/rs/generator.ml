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

(* Utils functions to get generators *)

let compare_list l1 l2 = match List.compare_lengths l1 l2 with 0 -> compare l1 l2 | c -> c

let rec subsets = function
  | [] -> [ [] ]
  | x :: l ->
    let subsets = subsets l in
    let new_subsets = List.map (fun subset -> x :: subset) subsets in
    subsets @ new_subsets

let get_words_gen f word_list rs =
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

  List.iter update word_list;
  f @@ List.rev !res

(* Get different sets of generators *)

let all_gen rs =
  let res = ref [] in

  List.iter
    begin
      fun (_, w1, w2) ->
        let rw1 =
          w1 |> String.to_seq |> List.of_seq |> List.rev |> List.to_seq |> String.of_seq
        in
        let rw2 =
          w2 |> String.to_seq |> List.of_seq |> List.rev |> List.to_seq |> String.of_seq
        in

        List.iter
          (fun subset -> res := get_words_gen Fun.id subset rs :: !res)
          (subsets [ w1; w2; rw1; rw2 ])
    end
    rs;

  !res |> List.sort_uniq compare_list

let all_sub_gen rs =
  let res = ref [] in

  List.iter
    begin
      fun (_, w1, w2) ->
        let rw1 =
          w1 |> String.to_seq |> List.of_seq |> List.rev |> List.to_seq |> String.of_seq
        in
        let rw2 =
          w2 |> String.to_seq |> List.of_seq |> List.rev |> List.to_seq |> String.of_seq
        in

        List.iter
          (fun subset -> res := get_words_gen subsets subset rs @ !res)
          (subsets [ w1; w2; rw1; rw2 ])
    end
    rs;

  !res |> List.sort_uniq compare_list
