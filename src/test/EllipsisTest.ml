open Monty
open Grammar

let tree = Alcotest.testable
             (Fmt.of_to_string to_string)
             equal

let sentence = Alcotest.testable
    (Fmt.of_to_string phrase_to_string)
    (equal_s)

let played_baseball =
  vp "played" ~comp:(np "baseball")

let jan_played_baseball =
  phrase (np "Jan_0") played_baseball

let ate_peanut_butter =
  vp "ate" ~comp:(np "peanut_butter")

let ron_ate_peanut_butter =
  phrase (np "Ron_3") ate_peanut_butter

let ian_did_too =
  phrase (np "Ian_2") (vp "did" ~comp:vp_e ~adjunct:(advp "too"))

let ian_did_E_x_too x =
  vp "did" ~comp:(ewrap x) ~adjunct:(advp "too")
  |> phrase (np "Ian_2")

let baseball_example_vp () =
  Ellipsis.vp_ellipsis_resolution [jan_played_baseball] ian_did_too
  |> Alcotest.(check @@ neg @@ list sentence)
    "equivalent lists"
    [ian_did_E_x_too played_baseball]

let baseballs_or_peanut_butter () =
  Ellipsis.vp_ellipsis_resolution
    [jan_played_baseball;
     ron_ate_peanut_butter ]
    ian_did_too
  |> Alcotest.(check @@ neg @@ list sentence)
    "ambiguous discourse leads to ambiguous resolution"
    [ian_did_E_x_too played_baseball;
     ian_did_E_x_too ate_peanut_butter ]

let sam_scratched_his_arm =
  phrase (np "Sam_1") (vp "scratched" ~comp:(dp "his_1" ~comp:(np "arm")))

let scratched_sams_arm =
  vp "scratched" ~comp:(dp "his_1" ~comp:(np "arm"))

let scratched_ians_arm =
  vp "scratched" ~comp:(dp "his_2" ~comp:(np "arm"))


let ambiguous_pronouns () =
  Ellipsis.vp_ellipsis_resolution
    [sam_scratched_his_arm]
    ian_did_too
  |> Alcotest.(check @@ list sentence)
    "sloppy vs strict readings lead to ambiguous resolution"
    [ian_did_E_x_too scratched_sams_arm;
     ian_did_E_x_too scratched_ians_arm ]


let tests = [
  Alcotest.test_case "resolution for `Ian did [E ?] too`" `Quick baseball_example_vp;
  Alcotest.test_case "ambiguity for `Ian did [E ?] too`" `Quick baseballs_or_peanut_butter;
  Alcotest.test_case "pronoun ambiguity for `Ian did [E ?] too`" `Quick ambiguous_pronouns;
]
