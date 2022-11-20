open Monty

let tree = Alcotest.testable
             (Fmt.of_to_string Grammar.to_string)
             Grammar.equal


let jan_played_baseball =
  let open Grammar in
  phrase (np "Jan") (vp "played" ~comp:(np "baseball"))

let ian_did_too =
  let open Grammar in
  phrase (np "Ian") (vp "did" ~comp:vp_e ~adjunct:(advp "too"))

let baseball_example () =
    Alcotest.(check @@ string)
    "can construct an NP"
    (* "[[[{}]spec [[[Jan]N]N']N']NP [[{}]spec [[[played]V [[{}]spec [[[baseball]N]N']N']NP]V']V']VP]S" *)
    "[[Jan]NP [[played]V [baseball]NP]VP]S"
    (Grammar.phrase_to_string jan_played_baseball)

let vp_ellipsis () =
  Alcotest.(check @@ string)
    "can construct a VP"
    (* "[[[{}]spec [[[Ian]N]N']N']NP [[{}]spec [[[did]V [[?]E]VP]V' [[{}]spec [[[too]A]A']A']AP]V']VP]S" *)
    "[[Ian]NP [[[did]V [E ?]VP]V' [too]AP]VP]S"
    (Grammar.phrase_to_string ian_did_too)


let tests = [
  Alcotest.test_case "Jan played baseball" `Quick baseball_example;
  Alcotest.test_case "Ian did [E ...] too" `Quick vp_ellipsis;
]
