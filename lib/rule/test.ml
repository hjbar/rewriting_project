open Utils

(* Testing functions *)

let test_get_rules () =
  let res = get_rules ~alpha_len:2 ~word_len:2 in
  let obj =
    [ (Some "R0", "a", "aa")
    ; (Some "R1", "a", "ab")
    ; (Some "R2", "a", "b")
    ; (Some "R3", "a", "ba")
    ; (Some "R4", "a", "bb")
    ; (Some "R5", "aa", "a")
    ; (Some "R6", "aa", "ab")
    ; (Some "R7", "aa", "b")
    ; (Some "R8", "aa", "ba")
    ; (Some "R9", "aa", "bb")
    ; (Some "R10", "ab", "a")
    ; (Some "R11", "ab", "aa")
    ; (Some "R12", "ab", "b")
    ; (Some "R13", "ab", "ba")
    ; (Some "R14", "ab", "bb")
    ; (Some "R15", "b", "a")
    ; (Some "R16", "b", "aa")
    ; (Some "R17", "b", "ab")
    ; (Some "R18", "b", "ba")
    ; (Some "R19", "b", "bb")
    ; (Some "R20", "ba", "a")
    ; (Some "R21", "ba", "aa")
    ; (Some "R22", "ba", "ab")
    ; (Some "R23", "ba", "b")
    ; (Some "R24", "ba", "bb")
    ; (Some "R25", "bb", "a")
    ; (Some "R26", "bb", "aa")
    ; (Some "R27", "bb", "ab")
    ; (Some "R28", "bb", "b")
    ; (Some "R29", "bb", "ba")
    ]
  in
  assert (res = obj);

  Print.println_ok "test_get_rules : OK"

(* All tests *)

let test () = test_get_rules ()
