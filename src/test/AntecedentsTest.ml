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
  [jan_played_baseball]
  |> Antecedents.get_antecedents
  |> Alcotest.(check @@ neg @@ list tree)
    "equivalent lists"
    []


let tests = [
  Alcotest.test_case "Antecedents of Jan played baseball" `Quick baseball_example;
]
