open Def
open Rewriting
open Utils

(* Testing functions *)

let test_orient () =
  let rs =
    make ~orient:Right
      Rule.[ make "aaa" "aa"; make "aab" "bbb"; make "acc" "c"; make "bxx" "ccc" ]
  in
  let res = orient rs in
  let obj = Some rs in
  assert (res = obj);

  let rs =
    make ~orient:Left
      Rule.[ make "aaa" "aa"; make "aab" "bbb"; make "acc" "c"; make "bxx" "ccc" ]
  in
  let res = orient rs in
  let obj = Some { rs with orient = Right } in
  assert (res = obj);

  let rs =
    make ~orient:Right
      Rule.[ make "aa" "aa"; make "aab" "bbbb"; make "a" "caa"; make "bxx" "ccc" ]
  in
  let res = orient rs in
  let obj = Some { rs with orient = Left } in
  assert (res = obj);

  let rs =
    make ~orient:Right
      Rule.[ make "aaa" "aa"; make "aab" "bbbb"; make "a" "caa"; make "bxax" "ccc" ]
  in
  let res = orient rs in
  let obj = None in
  assert (res = obj);

  Print.println_ok "test_orient : OK"

let test_orient_all () =
  let r1 = Rule.make "aaa" "aa" in
  let r2 = Rule.make "bbb" "bb" in
  let res = orient_all r1 r2 in
  let obj =
    ({ orient = Right; rules = [ r1 ] }, { orient = Right; rules = [ r2 ] }, "aaa", "bbb")
  in
  assert (res = obj);

  let r1 = Rule.make "aa" "bbb" in
  let r2 = Rule.make "bb" "aaa" in
  let res = orient_all r1 r2 in
  let obj =
    ({ orient = Left; rules = [ r1 ] }, { orient = Left; rules = [ r2 ] }, "bbb", "aaa")
  in
  assert (res = obj);

  let r1 = Rule.make "aaa" "cc" in
  let r2 = Rule.make "cc" "bbb" in
  let res = orient_all r1 r2 in
  let obj =
    ({ orient = Right; rules = [ r1 ] }, { orient = Right; rules = [ r2 ] }, "aaa", "cc")
  in
  assert (res = obj);

  Print.println_ok "test_orient_all : OK"

let test_normalize () =
  let rs = make ~orient:Right [ Rule.make "a" "bbb"; Rule.make "c" "ddd" ] in
  let w = "" in
  let res = normalize rs w in
  let obj = "" in
  assert (res = obj);

  let rs = make ~orient:Right [ Rule.make "a" "bbb" ] in
  let w = "aaa" in
  let res = normalize rs w in
  let obj = "bbbbbbbbb" in
  assert (res = obj);

  let rs = make ~orient:Left [ Rule.make "a" "bbb" ] in
  let w = "aaa" in
  let res = normalize rs w in
  let obj = "aaa" in
  assert (res = obj);

  let rs = make ~orient:Right [ Rule.make "a" "bbb" ] in
  let w = "aca" in
  let res = normalize rs w in
  let obj = "bbbcbbb" in
  assert (res = obj);

  let rs =
    make ~orient:Right [ Rule.make "a" "bbb"; Rule.make "b" "ccc"; Rule.make "c" "ddd" ]
  in
  let w = "aaa" in
  let res = normalize rs w in
  let obj =
    "ddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd"
  in
  assert (res = obj);

  let rs =
    make ~orient:Left [ Rule.make "a" "bbb"; Rule.make "b" "ccc"; Rule.make "c" "ddd" ]
  in
  let w = "aaa" in
  let res = normalize rs w in
  let obj = "aaa" in
  assert (res = obj);

  let rs =
    make ~orient:Left [ Rule.make "a" "bbb"; Rule.make "b" "ccc"; Rule.make "c" "ddd" ]
  in
  let w =
    "ddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd"
  in
  let res = normalize rs w in
  let obj = "aaa" in
  assert (res = obj);

  let rs =
    make ~orient:Left [ Rule.make "ddd" "c"; Rule.make "ccc" "b"; Rule.make "bbb" "a" ]
  in
  let w = "aaa" in
  let res = normalize rs w in
  let obj =
    "ddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd"
  in
  assert (res = obj);

  let rs =
    make ~orient:Right
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
  let rs = make ~orient:Right [ Rule.make "babab" "c"; Rule.make "bab" "d" ] in
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
  let obj = [ ("bc", "db"); ("ca", "ad") ] in
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
  test_orient ();
  test_orient_all ();
  test_normalize ();
  test_normalize_all ();
  test_critical_pairs ()
