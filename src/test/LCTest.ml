open Core
open Monty
open LC


let played_baseball =
  app [abs "y" @@ abs "x" @@ app [symb "PLAYED"; symb "x"; symb "y"];
       symb "BASEBALL"]

let jim_played_jim =
  app [abs "x" @@ abs "x" @@ app [symb "PLAYED"; symb "x"; symb "x"];
       symb "BASEBALL"; symb "JIM"]


let played_baseball_string () =
  Alcotest.(check string) "serialized correctly"
    "(\\x. (PLAYED x BASEBALL))"
    (to_string @@ beta played_baseball)

let shadow_jim_played_jim () =
  Alcotest.(check string) "serialied correctly"
    "(PLAYED JIM JIM)"
    (to_string @@ beta jim_played_jim)

let lift_jan_played_baseball () =
  let jan_played_baseball = app [played_baseball; symb "JAN"] in
  Tuple3.get2 @@ lifting String.Map.empty jan_played_baseball
  |> List.map ~f:to_string
  |> Alcotest.(check @@ list string)
       "equal list"
       [
         "(BASEBALL 1)";
         "(JAN 2)";
       ]


let tests = [
  Alcotest.test_case "check beta reduction" `Quick played_baseball_string;
  Alcotest.test_case "check capture avoided" `Quick shadow_jim_played_jim;
  Alcotest.test_case "lifted assertions correct" `Quick lift_jan_played_baseball;
]
