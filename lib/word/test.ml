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

let test_search_and_replace () =
  let res = search_and_replace "" "" "" in
  let obj = Some "" in
  assert (res = obj);

  let res = search_and_replace "a" "bab" "ccc" in
  let obj = Some "bcccb" in
  assert (res = obj);

  let res = search_and_replace "aa" "aaa" "bbbb" in
  let obj = Some "bbbba" in
  assert (res = obj);

  let res = search_and_replace "aab" "aaab" "c" in
  let obj = Some "ac" in
  assert (res = obj);

  let res = search_and_replace "aa" "aa" "cc" in
  let obj = Some "cc" in
  assert (res = obj);

  let res = search_and_replace "aaaaa" "baaaaac" "" in
  let obj = Some "bc" in
  assert (res = obj);

  let res = search_and_replace "aaa" "aaa" "" in
  let obj = Some "" in
  assert (res = obj);

  let res = search_and_replace "ccc" "aa" "bbb" in
  let obj = None in
  assert (res = obj);

  let res = search_and_replace "aaa" "aa" "bbb" in
  let obj = None in
  assert (res = obj);

  Print.println_ok "test_search_and_replace : OK"

let test_comp_per_char () =
  let res = comp_per_char "aa" "aa" in
  let obj = true in
  assert (res = obj);

  let res = comp_per_char "ba" "aa" in
  let obj = false in
  assert (res = obj);

  let res = comp_per_char "ab" "ba" in
  let obj = false in
  assert (res = obj);

  let res = comp_per_char "ab" "$$ab$" in
  let obj = true in
  assert (res = obj);

  let res = comp_per_char "aab" "$$aac$$" in
  let obj = false in
  assert (res = obj);

  let res = comp_per_char "aa" "aab" in
  let obj = true in
  assert (res = obj);

  let res = comp_per_char "$baa" "$$aa" in
  let obj = true in
  assert (res = obj);

  let res = comp_per_char "$$ab" "$aa$" in
  let obj = true in
  assert (res = obj);

  let res = comp_per_char "$$ba" "$aa$" in
  let obj = false in
  assert (res = obj);

  let res = comp_per_char "$$$ba" "$aab$" in
  let obj = true in
  assert (res = obj);

  let res = comp_per_char "babab" "$$$$bab$$$$" in
  let obj = true in
  assert (res = obj);

  let res = comp_per_char "$babab" "$$$$bab$$$$" in
  let obj = false in
  assert (res = obj);

  let res = comp_per_char "$$babab" "$$$$bab$$$$" in
  let obj = true in
  assert (res = obj);

  let res = comp_per_char "$$$babab" "$$$$bab$$$$" in
  let obj = false in
  assert (res = obj);

  let res = comp_per_char "$$$$babab" "$$$$bab$$$$" in
  let obj = true in
  assert (res = obj);

  let res = comp_per_char "$$$$$babab" "$$$$bab$$$$" in
  let obj = false in
  assert (res = obj);

  let res = comp_per_char "$$$$$$babab" "$$$$bab$$$$" in
  let obj = true in
  assert (res = obj);

  let () =
    try
      comp_per_char "aaa" "aa" |> ignore;
      assert false
    with _ -> assert true
  in

  Print.println_ok "test_comp_per_char : OK"

let test_unify () =
  let res = unify "ab" "$ba$" in
  let obj = "aba" in
  assert (res = obj);

  let res = unify "$$ab" "$ba$" in
  let obj = "bab" in
  assert (res = obj);

  let res = unify "$$ab" "$aa$" in
  let obj = "aab" in
  assert (res = obj);

  let res = unify "aaaa" "$$$aa$$$" in
  let obj = "aaaaa" in
  assert (res = obj);

  let res = unify "aa" "aa" in
  let obj = "aa" in
  assert (res = obj);

  let res = unify "ab" "$$ab$" in
  let obj = "abab" in
  assert (res = obj);

  let res = unify "aa" "aab" in
  let obj = "aab" in
  assert (res = obj);

  let res = unify "$baa" "$$aa" in
  let obj = "baa" in
  assert (res = obj);

  let res = unify "$$ab" "$aa$" in
  let obj = "aab" in
  assert (res = obj);

  let res = unify "$$$ba" "$aab$" in
  let obj = "aaba" in
  assert (res = obj);

  let res = unify "babab" "$$$$bab$$$$" in
  let obj = "bababab" in
  assert (res = obj);

  let res = unify "$$babab" "$$$$bab$$$$" in
  let obj = "babab" in
  assert (res = obj);

  let res = unify "$$$$babab" "$$$$bab$$$$" in
  let obj = "babab" in
  assert (res = obj);

  let res = unify "$$$$$$babab" "$$$$bab$$$$" in
  let obj = "bababab" in
  assert (res = obj);

  let () =
    try
      unify "aaa" "aa" |> ignore;
      assert false
    with _ -> assert true
  in

  let () =
    try
      unify "$ab" "$ab$" |> ignore;
      assert false
    with _ -> assert true
  in

  let () =
    try
      unify "ab" "aa" |> ignore;
      assert false
    with _ -> assert true
  in

  let () =
    try
      unify "$ab" "$aa" |> ignore;
      assert false
    with _ -> assert true
  in

  Print.println_ok "test_unify : OK"

(* All tests *)

let test () =
  test_get_words ();
  test_search_and_replace ();
  test_comp_per_char ();
  test_unify ()
