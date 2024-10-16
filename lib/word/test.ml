open Utils

(* Testing functions *)

let test_get_words () =
  let res = get_words ~alpha_len:16 ~word_len:0 in
  let obj = [] in
  assert (res = obj);

  let res = get_words ~alpha_len:0 ~word_len:16 in
  let obj = [] in
  assert (res = obj);

  let res = get_words ~alpha_len:1 ~word_len:1 in
  let obj = [ "a" ] in
  assert (res = obj);

  let res = get_words ~alpha_len:1 ~word_len:5 in
  let obj = [ "a"; "aa"; "aaa"; "aaaa"; "aaaaa" ] in
  assert (res = obj);

  let res = get_words ~alpha_len:2 ~word_len:2 in
  let obj = [ "a"; "aa"; "ab"; "b"; "ba"; "bb" ] in
  assert (res = obj);

  let res = get_words ~alpha_len:3 ~word_len:1 in
  let obj = [ "a"; "b"; "c" ] in
  assert (res = obj);

  let res = get_words ~alpha_len:3 ~word_len:2 in
  let obj = [ "a"; "aa"; "ab"; "ac"; "b"; "ba"; "bb"; "bc"; "c"; "ca"; "cb"; "cc" ] in
  assert (res = obj);

  let res = get_words ~alpha_len:3 ~word_len:3 in
  let obj =
    [ "a"
    ; "aa"
    ; "aaa"
    ; "aab"
    ; "aac"
    ; "ab"
    ; "aba"
    ; "abb"
    ; "abc"
    ; "ac"
    ; "aca"
    ; "acb"
    ; "acc"
    ; "b"
    ; "ba"
    ; "baa"
    ; "bab"
    ; "bac"
    ; "bb"
    ; "bba"
    ; "bbb"
    ; "bbc"
    ; "bc"
    ; "bca"
    ; "bcb"
    ; "bcc"
    ; "c"
    ; "ca"
    ; "caa"
    ; "cab"
    ; "cac"
    ; "cb"
    ; "cba"
    ; "cbb"
    ; "cbc"
    ; "cc"
    ; "cca"
    ; "ccb"
    ; "ccc"
    ]
  in
  assert (res = obj);

  let () =
    try
      get_words ~alpha_len:3 ~word_len:~-1 |> ignore;
      assert false
    with _ -> assert true
  in

  let () =
    try
      get_words ~alpha_len:~-1 ~word_len:5 |> ignore;
      assert false
    with _ -> assert true
  in

  let () =
    try
      get_words ~alpha_len:27 ~word_len:5 |> ignore;
      assert false
    with _ -> assert true
  in

  Print.println_ok "test_get_words : OK"

(* All tests *)

let test () = test_get_words ()
