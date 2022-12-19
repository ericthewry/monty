
let test_suites : unit Alcotest.test list = [
  "Grammar", GrammarTest.tests;
  "Antecedents", AntecedentsTest.tests;
  "Ellipsis", EllipsisTest.tests;
  "Lambda Calculus", LCTest.tests;
]

(* let%test_unit "identity" = SmartConstructors.identity () *)

let () =
  Alcotest.run "Monty" test_suites
