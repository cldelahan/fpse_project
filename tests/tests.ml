(*
  Testing the Graph module
  Author: Conner Delahanty
  Date: 12/6/21
*)

open Core;;
open OUnit2;;
open Graph;;

let i = Broql.empty;;
let d = Database.empty;;
let n = Node.empty;;
let e = Edge.empty;;
let r = Relation.empty;;


(* 
  Creation tests
*)

let test_creation _ =
  assert_equal 0 @@ Broql.n_queries i;;
  assert_equal "" @@ Node.to_string n;;
  assert_equal "" @@ Edge.get_id e;;
  assert_equal "" @@ Relation.get_id r;;
;;

let instantiation_tests =
  "Instantiation Tests" >: test_list [
    "Test Create" >:: test_creation;
  ]

(*
  Node tests
*)

let n2 = Node.add_attr n "name" "conner";;
let n3 = Node.set_from_json n "{name: \"vini\", id: \"45\"}";;

let test_node_attr _ =
  assert_equal None @@ Node.get_attr n "name";;
  assert_equal (Some "conner") @@ Node.get_attr n2 "name";;
  assert_equal (Some "45") @@ Node.get_attr n3 "id";;
  assert_equal None @@ Node.get_attr n3 "Id";;
;;

let test_node_tostring _ = 
  assert_equal "" @@ Node.to_string n;;
  assert_equal "name: conner" @@ Node.to_string n2;;
  assert_equal "id: 45 name: vini" @@ Node.to_string n3;;
;;

let node_tests =
  "Node Tests" >: test_list [
    "Test Attributes" >:: test_node_attr;
    "Test ToString" >:: test_node_tostring;
  ]

(* 
  Database Tests
*)

let d2 = Database.add_node d "n" n;;
let d2 = Database.add_node d2 "n2" n2;;

let node_equal n1_o n2_o = 
  match (n1_o, n2_o) with
  | (Some n1, Some n2) -> String_Map.equal String.equal n1 n2
  | (None, None) -> true
  | (_, _) -> false

let test_database_io _ =
  assert_equal true @@ (node_equal None (Database.get_node d2 "n3"));;
  assert_equal true @@ (node_equal (Some n) (Database.get_node d2 "n"));;
  assert_equal false @@ (node_equal (Some n2) (Database.get_node d2 "n"));;
;;


Broql.add_node i ~json:"{name: \"vini\", id: \"45\"}" "node1";;
Broql.add_node i ~json:"{name: \"conner\", id: \"67\"}" "node2";;


let database_tests =
  "Database Tests" >: test_list [
    "Test IO" >:: test_database_io;
  ]

(*
  Broql Tests
*)

let test_broql_attr _ = 
  assert_equal (Some "vini") @@ Broql.get_attr i ~name:"name" "node1";;
  assert_equal (Some "67") @@ Broql.get_attr i ~name:"id" "node2";;
  assert_equal None @@ Broql.get_attr i ~name:"Id" "node2";;
;;

(* Write and read from file *)
let path = "out.broql";;
Broql.save i path;;
let i_recovered = Broql.load path;;

let test_broql_fileio _ = 
  assert_equal (Some "vini") @@ Broql.get_attr i_recovered ~name:"name" "node1";;
  assert_equal (Some "67") @@ Broql.get_attr i_recovered ~name:"id" "node2";;
  assert_equal None @@ Broql.get_attr i_recovered ~name:"Id" "node2";;
;;
  
let broql_tests =
  "Broql Tests" >: test_list [
    "Test Broql Attributes" >:: test_broql_attr;
    "Test Broql File IO" >:: test_broql_fileio;
  ]

let series =
  "Broql Tests" >::: [
    instantiation_tests;
    node_tests;
    database_tests;
    broql_tests;
  ]

let () =
  run_test_tt_main series