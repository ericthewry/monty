open Monty
open Grammar

let tree = Alcotest.testable
             (Fmt.of_to_string to_string)
             equal


let played_baseball =
  vp "played" ~comp:(np "baseball")

let jan_played_baseball =
  phrase (np "Jan") played_baseball

let ian_did_too =
  phrase (np "Ian") (vp "did" ~comp:vp_e ~adjunct:(advp "too"))

let ian_did_play_baseball_too =
  phrase (np "Ian") (vp "did" ~comp:played_baseball ~adjunct:(advp "too"))

let baseball_example_vp () =
  Ellipsis.vp_ellipsis_resolution [jan_played_baseball] ian_did_too.vp
  |> Alcotest.(check @@ list tree)
    "equivalent lists"
    [(vp "did" ~comp:played_baseball ~adjunct:(advp "too"))]


let tests = [
  Alcotest.test_case "Ellipsis candidates for `Ian did [E ?] too`" `Quick baseball_example_vp;
]
