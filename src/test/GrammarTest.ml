open Core
open Monty

let tree = Alcotest.testable
             (Fmt.of_to_string Grammar.to_string)
             Grammar.equal

let lexicon =
  let open LC in
  [("Jan", symb "JAN");
   ("baseball", symb "BASEBALL");
   ("played", abs "x" @@ abs "y" @@ app [symb "PLAYED"; symb "y"; symb "x"]);
  ]
  |> String.Map.of_alist_exn

let jan_played_baseball =
  let open Grammar in
  phrase (np "Jan") (vp "played" ~comp:(np "baseball"))

let ian_did_too =
  let open Grammar in
  phrase (np "Ian") (vp "did" ~comp:vp_e ~adjunct:(advp "too"))

let baseball_example () =
    Alcotest.(check @@ string)
    "can construct an NP"
    "[[Jan]NP [[played]V [baseball]NP]VP]S"
    (Grammar.phrase_to_string jan_played_baseball)

let vp_ellipsis () =
  Alcotest.(check @@ string)
    "can construct a VP"
    "[[Ian]NP [[[did]V [E ?]VP]V' [too]AP]VP]S"
    (Grammar.phrase_to_string ian_did_too)


let jan_played_baseball_semantics () =
  let open Grammar in
  phrase (np "Jan") (vp "played" ~comp:(np "baseball"))
  |> phrase_semantics lexicon
  |> LC.to_string
  |> Alcotest.(check string)
       "correct denotation"
       "(PLAYED JAN BASEBALL)"

let tests = [
  Alcotest.test_case "Jan played baseball" `Quick baseball_example;
  Alcotest.test_case "Ian did [E ...] too" `Quick vp_ellipsis;
  Alcotest.test_case "[|Jan played baseball|]" `Quick jan_played_baseball_semantics;
]
