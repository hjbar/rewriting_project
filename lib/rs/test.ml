open Rewriting
open Utils

(* Testing functions *)

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

(* All tests *)

let test () = test_normalize ()
