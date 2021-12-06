(*
  Testing the Graph module
  Author: Conner Delahanty
  Date: 12/6/21
*)

open Core;;
open OUnit2;;
open Graph;;

let instance = Broql.empty;;
let n = Node.empty;;
let e = Edge.empty;;
let r = Relation.empty;;

let test_creation _ =
  assert_equal 0 @@ Broql.n_queries instance;;
  assert_equal "" @@ Node.to_string n;;
;;


let instantiation_tests =
  "Instantiation Tests" >: test_list [
    "Test Create" >:: test_creation;
  ]


let series =
  "Broql Tests" >::: [
    instantiation_tests;
  ]

let () =
  run_test_tt_main series