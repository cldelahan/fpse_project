open OUnit2;;
open Parser;;
(* open Ast;; *)
open Debugutils;;

let test_lexer _ = 
  let obtained = lex "NODE n1 = {name: \"Vini\", id: \"45\"};" in
  let expected = [NODE; IDENT "n1"; EQUAL; STRING "{name: \"Vini\", id: \"45\"}"; EOEX] in
  assert_equal expected obtained;
  let obtained = lex "RELATION roommates FOR s1, s2, s3;" in
  let expected = [RELATION; IDENT "roommates"; FOR; IDENT "s1"; COMMA; IDENT "s2"; COMMA; IDENT "s3"; EOEX] in
  assert_equal expected obtained;
  let obtained = lex "WHO roommates FOR s2;" in
  let expected = [WHO; IDENT "roommates"; FOR; IDENT "s2"; EOEX] in
  assert_equal expected obtained;
  let obtained = lex "SIZE WHO roommates FOR s2;" in
  let expected = [SIZE; WHO; IDENT "roommates"; FOR; IDENT "s2"; EOEX] in
  assert_equal expected obtained;
  let obtained = lex "CREATE RELATION UNDIR rel_name;" in
  let expected = [CREATE; RELATION; UNDIR; IDENT "rel_name"; EOEX] in
  assert_equal expected obtained;
  let obtained = lex "CREATE RELATION loves is_loved;" in
  let expected = [CREATE; RELATION; IDENT "loves"; IDENT "is_loved"; EOEX] in
  assert_equal expected obtained;
  let obtained = lex "ATTR name n2;" in
  let expected = [ATTR; IDENT "name"; IDENT "n2"; EOEX] in
  assert_equal expected obtained

(* let test_parser_create_node _ =
   assert_equal (CreateNode (Ident "x", "{name: \"Conner\", age: \"22\"}")) @@ parse "NODE x = {name: \"Conner\", age: \"22\"};"

   let test_parser_node _ =
   assert_equal (Node(Ident "x")) @@ parse "NODE x;"

   let test_parser_create_relation _ =
   assert_equal (CreateRelation (Ident "roommates", None, false)) @@ parse "CREATE RELATION UNDIR roommates;";
   assert_equal (CreateRelation (Ident "visited", None, true)) @@ parse "CREATE RELATION visited;";
   assert_equal (CreateRelation (Ident "likes", Some (Ident "is_liked"), true)) @@ parse "CREATE RELATION likes is_liked;"

   let test_parser_create_edge _ =
   assert_equal (CreateEdge (Ident "roommates", [Node (Ident "n1"); Node (Ident "n2"); Node (Ident "n3")])) @@ parse "RELATION roommates FOR n1, n2, n3;"

   let test_parser_who _ =
   assert_equal (Who (Relation (Ident "loves"), Node (Ident "n1"), 1)) @@ parse "WHO loves n1;";
   assert_equal (Who (Relation (Ident "is_loved"), Node (Ident "n1"), 1)) @@ parse "WHO is_loved BY n1;";
   assert_equal (parse "WHO is_liked n") @@ parse "WHO is_liked BY n;"; (* Test optional BY keyword *)
   assert_equal (Who (Relation (Ident "loves"), Node (Ident "n2"), 3)) @@ parse "WHO loves n2 REC 3;";
   assert_equal (Who (Relation (Ident "is_loved"), Node (Ident "n2"), 3)) @@ parse "WHO is_loved BY n2 REC 3;"

   let test_parser_attr _ =
   assert_equal (Attr (Ident "name", Node (Ident "n1"))) @@ parse "ATTR name n1;"

   let test_parser_size _ =
   assert_equal (Size ([Node (Ident "n1"); Node (Ident "n2")])) @@ parse "SIZE n1, n2;"

   let test_parser_search _ =
   assert_equal (Search "{name: \"Vini\"}") @@ parse "SEARCH {name: \"Vini\"};"

   let test_parser_show_nodes _ =
   assert_equal ShowNodes @@ parse "SHOW NODES;"

   let test_parser_show_relations _ =
   assert_equal ShowRelations @@ parse "SHOW RELATIONS;" *)

let lexing_tests = 
  "Lexing Tests" >: test_list [
    "Lexer"    >:: test_lexer
  ]

(* let parsing_tests =
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
    "SHOW RELATIONS"  >:: test_parser_show_relations
   ] *)

let series = 
  "Lexing/Parsing Tests" >::: [
    lexing_tests
    (* parsing_tests *)
  ]

let () = 
  run_test_tt_main series