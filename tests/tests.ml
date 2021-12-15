(*
  Testing the Graph module
  Author: Conner Delahanty
  Date: 12/6/21
*)

open Core;;
open OUnit2;;
open Graph;;
open Debugutils;;

let b = Broql.empty;;
let d = Database.empty;;
let n = Node.empty;;
let e = Edge.empty;;
let r = Relation.empty;;


(* 
  Creation tests
*)

let test_creation _ =
  assert_equal 0 @@ Broql.n_queries b;
  assert_equal "{}" @@ Node.to_string n;
  assert_equal "" @@ Edge.get_id e;
  assert_equal "" @@ Relation.get_id r

let instantiation_tests =
  "Instantiation Tests" >: test_list [
    "Test Create" >:: test_creation
  ]

(*
  Node tests
*)

let n2 = Node.add_attr n "name" "conner";;
let n3 = Node.set_from_json n "{name: \"vini\", id: \"45\"}";;
let n4 = Node.set_from_json n "{name: \"tom\", id: \"44\"}";;
let n5 = Node.set_from_json n "{name: \"bob\", id: \"41\"}";;
let n6 = Node.set_from_json n "{name: \"bill\", id: \"40\"}";;

let test_node_attr _ =
  assert_equal None @@ Node.get_attr n "name";
  assert_equal (Some "conner") @@ Node.get_attr n2 "name";
  assert_equal (Some "45") @@ Node.get_attr n3 "id";
  assert_equal None @@ Node.get_attr n3 "Id"

let test_node_tostring _ = 
  assert_equal "{}" @@ Node.to_string n;
  assert_equal "{name: conner}" @@ Node.to_string n2;
  assert_equal "{id: 45, name: vini}" @@ Node.to_string n3

let node_tests =
  "Node Tests" >: test_list [
    "Test Attributes" >:: test_node_attr;
    "Test ToString" >:: test_node_tostring
  ]

(* 
  Database Tests
*)

let d2 = Database.add_node_exn d "n" n;;
let d2 = Database.add_node_exn d2 "n2" n2;;
let d2 = Database.add_node_exn d2 "n3" n3;;
let d2 = Database.add_node_exn d2 "n4" n4;;
let d2 = Database.add_node_exn d2 "n5" n4;;

let d3 = Database.create_relation d2 "friends" false;;
let d3 = Database.create_relation d3 "loves" true;;

let d3 = Database.add_edge d3 "friends" ["n";"n2"];;
let d3 = Database.add_edge d3 "loves" ["n2"; "n"];;
let d3 = Database.add_edge d3 "loves" ["n3"; "n"];;
let d3 = Database.add_edge d3 "loves" ["n3"; "n2"];;
let d3 = Database.add_edge d3 "loves" ["n5"; "n3"];;


let opt_equal ~f o1_o o2_o = 
  match (o1_o, o2_o) with
  | (Some o1, Some o2) -> f o1 o2
  | (None, None) -> true
  | (_, _) -> false


let test_database_nodes _ =
  assert_equal true @@ (opt_equal ~f:Node.equal None (Database.get_node d2 "n10"));
  assert_equal true @@ (opt_equal ~f:Node.equal (Some n2) (Database.get_node d2 "n2"));
  assert_equal false @@ (opt_equal  ~f: Node.equal (Some n3) (Database.get_node d2 "n2"))


let test_database_relations _ = 
  assert_equal true @@ (Database.has_relation d3 "friends");
  assert_equal false @@ (Database.has_relation d3 "not_friends")


let test_database_neighbors _ = 
  assert_equal ["n"] @@ (Database.neighbors d3 "loves" "n2");
  assert_equal ["n2"] @@ (Database.neighbors d3 "friends" "n");;


Broql.add_node b ~json:"{name: \"vini\", id: \"45\"}" "node1";;
Broql.add_node b ~json:"{name: \"conner\", id: \"67\"}" "node2";;
Broql.add_node b ~json:"{name: \"steven\", id: \"11\"}" "node3";;
Broql.add_node b ~json:"{name: \"dexter\", height: \"tall\", id: \"11\"}" "node4";;

let database_tests =
  "Database Tests" >: test_list [
    "Test Node IO" >:: test_database_nodes;
    "Test Relation IO" >:: test_database_relations;
    "Test WHO IO" >:: test_database_neighbors
  ]

(*
  Broql Tests
*)

let test_broql_attr _ = 
  assert_equal (Some "vini") @@ Broql.get_attr b ~name:"name" "node1";
  assert_equal (Some "67") @@ Broql.get_attr b ~name:"id" "node2";
  assert_equal None @@ Broql.get_attr b ~name:"Id" "node2";;


Broql.create_relation_pair b "manages" "is_managed_by";
Broql.create_relation b "roomates" false;
Broql.add_edge b "roomates" ["node1"; "node2"; "node4"]

(* Create BROQL with some relations *)
let test_broql_paired_relations _ = 
  assert_equal None @@ Database.get_relation b.db "mannnages";
  assert_equal true @@ Database.has_relation b.db "manages";
  assert_equal true @@ Database.has_relation b.db "is_managed_by";;


Broql.add_edge b "manages" ["node1"; "node2"];; (* node1 manages node2 *)
Broql.add_edge b "is_managed_by" ["node2"; "node3"];; (* node2 is managed by node3*)
Broql.create_relation b "loves" true;;
Broql.add_edge b "loves" ["node3"; "node2"];; (* node3 loves node2 *)
Broql.add_edge b "loves" ["node2"; "node1"];; (* node2 loves node1 *)

(* Assert that creating a relation that is paired creates the pair *)
let test_broql_paired_relations_inserts _ = 
  assert_equal ["node2"] @@ (Database.neighbors b.db "manages" "node1"); (* who node1 manages? *)
  assert_equal ["node2"] @@ (Database.neighbors b.db "manages" "node3"); (* who node3 manages? *)
  assert_equal ["node3"; "node1"] @@ (Database.neighbors b.db "is_managed_by" "node2")


(* List.map (Broql.who b "loves" ~recurse:true ~times:2 "node3") ~f:(fun x-> Printf.printf "%s\n" x);; *)


let test_broql_who_rec _ = 
  assert_equal ["node2"] @@ (Broql.who b "manages" 1 "node1");
  assert_equal ["node3"; "node1"] @@ (Broql.who b "is_managed_by" 1 "node2");
  assert_equal ["node1"] @@ (Broql.who b "loves" 2 "node3");
  assert_equal ["node2"] @@ (Broql.who b "loves" 1 "node3");
  assert_equal ["node1"] @@ (Broql.who b "loves" 1 "node2")
(* assert_equal true @@ true;; *)



let test_broql_search _ = 
  assert_equal ["node1"] @@ (Broql.search b "{name: \"vini\"}");
  assert_equal ["node4"] @@ (Broql.search b "{height: \"tall\"}");
  assert_equal ["node3"; "node4"] @@ (Broql.search b "{id: \"11\"}");
  assert_equal ["node1"; "node2"; "node3"; "node4"] @@ (Broql.search b "{}")


(* Write and read from file *)
let path = "out.broql";;
Broql.save b path;;
let i_recovered = Broql.load path;;

let test_broql_fileio _ = 
  assert_equal (Some "vini") @@ Broql.get_attr i_recovered ~name:"name" "node1";
  assert_equal (Some "67") @@ Broql.get_attr i_recovered ~name:"id" "node2";
  assert_equal None @@ Broql.get_attr i_recovered ~name:"Id" "node2"


(* Test miscellaneous functionality *)
let test_broql_misc _ = 
  assert_equal ["node1"; "node2"; "node3"; "node4"] @@ Broql.show_nodes b;
  assert_equal ["is_managed_by"; "loves"; "manages"; "roomates"] @@ Broql.show_relations b


let broql_tests =
  "Broql Tests" >: test_list [
    "Test Broql Attributes" >:: test_broql_attr;
    "Test Broql Paired Relations" >:: test_broql_paired_relations;
    "Test Broql Paired Relations Inserts" >:: test_broql_paired_relations_inserts;
    "Test Broql Search" >:: test_broql_search;
    "Test Broql File IO" >:: test_broql_fileio;
    "Test Broql WHO Rec" >:: test_broql_who_rec;
    "Test Broql Misc" >:: test_broql_misc
  ]

(* 
  Lexing Tests
*)

open Parser;;
open Ast;;
let test_lexer _ = 
  let obtained = debug_lex "NODE n1 = {name: \"Vini\", id: \"45\"};" in
  let expected = [NODE; IDENT "n1"; EQUAL; STRING "{name: \"Vini\", id: \"45\"}"; EOEX] in
  assert_equal expected obtained;
  let obtained = debug_lex "RELATION roommates FOR s1, s2, s3;" in
  let expected = [RELATION; IDENT "roommates"; FOR; IDENT "s1"; COMMA; IDENT "s2"; COMMA; IDENT "s3"; EOEX] in
  assert_equal expected obtained;
  let obtained = debug_lex "WHO roommates FOR s2;" in
  let expected = [WHO; IDENT "roommates"; FOR; IDENT "s2"; EOEX] in
  assert_equal expected obtained;
  let obtained = debug_lex "SIZE WHO roommates FOR s2;" in
  let expected = [SIZE; WHO; IDENT "roommates"; FOR; IDENT "s2"; EOEX] in
  assert_equal expected obtained;
  let obtained = debug_lex "CREATE RELATION UNDIR rel_name;" in
  let expected = [CREATE; RELATION; UNDIR; IDENT "rel_name"; EOEX] in
  assert_equal expected obtained;
  let obtained = debug_lex "CREATE RELATION loves is_loved;" in
  let expected = [CREATE; RELATION; IDENT "loves"; IDENT "is_loved"; EOEX] in
  assert_equal expected obtained;
  let obtained = debug_lex "ATTR name n2;" in
  let expected = [ATTR; IDENT "name"; IDENT "n2"; EOEX] in
  assert_equal expected obtained

let lexing_tests = 
  "Lexing Tests" >: test_list [
    "Lexer"    >:: test_lexer
  ]

(*
  Parsing Tests
*)
let test_parser_create_node _ =
  assert_equal (CreateNode (Ident "x", "{name: \"Conner\", age: \"22\"}")) @@ parse "NODE x = {name: \"Conner\", age: \"22\"};"

let test_parser_node _ =
  assert_equal (Attr (None, Node (Ident "x"))) @@ parse "NODE x;"

let test_parser_create_relation _ =
  assert_equal (CreateRelation (Ident "roommates", None, false)) @@ parse "CREATE RELATION UNDIR roommates;";
  assert_equal (CreateRelation (Ident "visited", None, true)) @@ parse "CREATE RELATION visited;";
  assert_equal (CreateRelation (Ident "likes", Some (Ident "is_liked"), true)) @@ parse "CREATE RELATION likes is_liked;"

let test_parser_create_edge _ =
  assert_equal (CreateEdge (Ident "roommates", NodeList [Node (Ident "n1"); Node (Ident "n2"); Node (Ident "n3")])) @@ parse "RELATION roommates FOR n1, n2, n3;"

let test_parser_who _ =
  assert_equal (Who (Relation (Ident "loves"), Node (Ident "n1"), 1)) @@ parse "WHO loves n1;";
  assert_equal (Who (Relation (Ident "is_loved"), Node (Ident "n1"), 1)) @@ parse "WHO is_loved BY n1;";
  assert_equal (parse "WHO is_liked n") @@ parse "WHO is_liked BY n;"; (* Test optional BY keyword *)
  assert_equal (Who (Relation (Ident "loves"), Node (Ident "n2"), 3)) @@ parse "WHO loves n2 REC 3;";
  assert_equal (Who (Relation (Ident "is_loved"), Node (Ident "n2"), 3)) @@ parse "WHO is_loved BY n2 REC 3;"

let test_parser_attr _ =
  assert_equal (Attr (Some (Ident "name"), Node (Ident "n1"))) @@ parse "ATTR name n1;"

let test_parser_size _ =
  assert_equal (Size (NodeList [Node (Ident "n1"); Node (Ident "n2")])) @@ parse "SIZE n1, n2;"

let test_parser_search _ =
  assert_equal (Search "{name: \"Vini\"}") @@ parse "SEARCH {name: \"Vini\"};"

let test_parser_show_nodes _ =
  assert_equal ShowNodes @@ parse "SHOW NODES;"

let test_parser_show_relations _ =
  assert_equal ShowRelations @@ parse "SHOW RELATIONS;"

let test_parser_load _ =
  assert_equal (Load "test.broql") @@ parse "LOAD test.broql;"

let test_parser_save _ =
  assert_equal (Save "test.broql") @@ parse "SAVE test.broql;"


let parsing_tests =
  "Parsing Tests" >: test_list [
    "CREATE NODE"     >:: test_parser_create_node;
    "NODE"            >:: test_parser_node;
    "CREATE RELATION" >:: test_parser_create_relation;
    "RELATION"        >:: test_parser_create_edge;
    "WHO"             >:: test_parser_who;
    "ATTR"            >:: test_parser_attr;
    "SIZE"            >:: test_parser_size;
    "SEARCH"          >:: test_parser_search;
    "SHOW NODES"      >:: test_parser_show_nodes;
    "SHOW RELATIONS"  >:: test_parser_show_relations;
    "LOAD"            >:: test_parser_load;
    "SAVE"            >:: test_parser_save
  ]

let series =
  "Broql Tests" >::: [
    instantiation_tests;
    node_tests;
    database_tests;
    broql_tests;
    lexing_tests;
    parsing_tests
  ]

let () =
  run_test_tt_main series