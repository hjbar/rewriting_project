open Rewriting
open Utils

(* Testing functions *)

let test_orient_rule () =
  let res = Rule.make "aaa" "aa" |> orient_rule in
  let obj = Rule.make "aaa" "aa" in
  assert (res = obj);

  let res = Rule.make "aa" "aaa" |> orient_rule in
  let obj = Rule.make "aaa" "aa" in
  assert (res = obj);

  let res = Rule.make "aaa" "aaa" |> orient_rule in
  let obj = Rule.make "aaa" "aaa" in
  assert (res = obj);

  let res = Rule.make "aa" "ba" |> orient_rule in
  let obj = Rule.make "ba" "aa" in
  assert (res = obj);

  Print.println_ok "test_orient_rule : OK"

let test_orient_rs () =
  let res =
    make Rule.[ make "aaa" "aa"; make "aab" "bbb"; make "acc" "c"; make "bxx" "ccc" ]
    |> orient_rs
  in
  let obj =
    make Rule.[ make "aaa" "aa"; make "bbb" "aab"; make "acc" "c"; make "ccc" "bxx" ]
  in
  assert (res = obj);

  let res =
    make Rule.[ make "aa" "aa"; make "aab" "bbbb"; make "a" "caa"; make "bxx" "ccc" ]
    |> orient_rs
  in
  let obj =
    make Rule.[ make "aa" "aa"; make "bbbb" "aab"; make "caa" "a"; make "ccc" "bxx" ]
  in
  assert (res = obj);

  Print.println_ok "test_orient_rs : OK"

let test_normalize () =
  let rs = make [ Rule.make "a" "bbb"; Rule.make "c" "ddd" ] in
  let w = "" in
  let res = normalize rs w in
  let obj = "" in
  assert (res = obj);

  let rs = make [ Rule.make "a" "bbb" ] in
  let w = "aaa" in
  let res = normalize rs w in
  let obj = "bbbbbbbbb" in
  assert (res = obj);

  let rs = make [ Rule.make "bbb" "a" ] in
  let w = "aaa" in
  let res = normalize rs w in
  let obj = "aaa" in
  assert (res = obj);

  let rs = make [ Rule.make "a" "bbb" ] in
  let w = "aca" in
  let res = normalize rs w in
  let obj = "bbbcbbb" in
  assert (res = obj);

  let rs = make [ Rule.make "a" "bbb"; Rule.make "b" "ccc"; Rule.make "c" "ddd" ] in
  let w = "aaa" in
  let res = normalize rs w in
  let obj =
    "ddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd"
  in
  assert (res = obj);

  let rs = make [ Rule.make "bbb" "a"; Rule.make "ccc" "b"; Rule.make "ddd" "c" ] in
  let w = "aaa" in
  let res = normalize rs w in
  let obj = "aaa" in
  assert (res = obj);

  let rs = make [ Rule.make "bbb" "a"; Rule.make "ccc" "b"; Rule.make "ddd" "c" ] in
  let w =
    "ddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd"
  in
  let res = normalize rs w in
  let obj = "aaa" in
  assert (res = obj);

  let rs = make [ Rule.make "c" "ddd"; Rule.make "b" "ccc"; Rule.make "a" "bbb" ] in
  let w = "aaa" in
  let res = normalize rs w in
  let obj =
    "ddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd"
  in
  assert (res = obj);

  let rs =
    [ Rule.make "a" "b"
    ; Rule.make "b" "d"
    ; Rule.make "c" "f"
    ; Rule.make "d" "e"
    ; Rule.make "e" "g"
    ; Rule.make "f" "h"
    ]
  in
  let w = "abcdef" in
  let res = normalize rs w in
  let obj = "gghggh" in
  assert (res = obj);

  Print.println_ok "test_normalize : OK"

let test_normalize_all () =
  let rs = make [ Rule.make "babab" "c"; Rule.make "bab" "d" ] in
  let w = "bababab" in
  let res = normalize_all rs w in
  let obj = [ "cab"; "bac"; "dad"; "badab" ] in
  assert (List.length res = List.length obj && List.for_all (fun w -> List.mem w obj) res);

  Print.println_ok "test_normalize_all : OK"

let test_critical_pairs () =
  let res = critical_rules (Rule.make "ab" "c") (Rule.make "ba" "d") in
  let obj = [ ("bc", "db"); ("ca", "ad") ] in
  assert (List.for_all (fun p -> List.mem p res) obj && List.length res = List.length obj);

  let res = critical_rules (Rule.make "c" "ab") (Rule.make "d" "ba") in
  let obj = [] in
  assert (List.for_all (fun p -> List.mem p res) obj && List.length res = List.length obj);

  let res = critical_rules (Rule.make "ba" "d") (Rule.make "ab" "c") in
  let obj = [ ("db", "bc"); ("ad", "ca") ] in
  assert (List.for_all (fun p -> List.mem p res) obj && List.length res = List.length obj);

  let res = critical_rules (Rule.make "babab" "c") (Rule.make "bab" "d") in
  let obj =
    [ ("c", "dab")
    ; ("c", "bad")
    ; ("cab", "dad")
    ; ("cab", "badab")
    ; ("bac", "dad")
    ; ("bac", "badab")
    ]
  in
  assert (List.for_all (fun p -> List.mem p res) obj && List.length res = List.length obj);

  Print.println_ok "test_critical_pairs : OK"

(* All tests *)

let test () =
  test_orient_rule ();
  test_orient_rs ();
  test_normalize ();
  test_normalize_all ();
  test_critical_pairs ()
