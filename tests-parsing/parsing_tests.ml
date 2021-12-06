open Core;;
open OUnit2;;
open Parser;;

(* Helper function to extract all tokens from lexbuf *)
let list_of_lexbuf (lexbuf: Lexing.lexbuf): token list =
  let rec helper lexbuf cur_list =
    match Lexer.token lexbuf with
    | EOEX as t -> t :: cur_list
    | _ as t -> helper lexbuf (t :: cur_list)
  in List.rev @@ helper lexbuf []

let test_lexer _ = 
  let lexbuf = Lexing.from_string "NODE n1 = {name: \"Vini\", id: \"45\"};" in
  let expected = [NODE; IDENT "n1"; EQUAL; STRING "{name: \"Vini\", id: \"45\"}"; EOEX] in
  assert_equal expected @@ list_of_lexbuf lexbuf;
  let lexbuf = Lexing.from_string "RELATION roommates FOR s1, s2, s3;" in
  let expected = [RELATION; IDENT "roommates"; FOR; IDENT "s1"; COMMA; IDENT "s2"; COMMA; IDENT "s3"; EOEX] in
  assert_equal expected @@ list_of_lexbuf lexbuf;
  let lexbuf = Lexing.from_string "WHO roommates FOR s2;" in
  let expected = [WHO; IDENT "roommates"; FOR; IDENT "s2"; EOEX] in
  assert_equal expected @@ list_of_lexbuf lexbuf;
  let lexbuf = Lexing.from_string "SIZE WHO roommates FOR s2;" in
  let expected = [SIZE; WHO; IDENT "roommates"; FOR; IDENT "s2"; EOEX] in
  assert_equal expected @@ list_of_lexbuf lexbuf

(* let test_parser _ =
   let  *)

let parsing_tests = 
  "Parsing Tests" >: test_list [
    "Lexer"    >:: test_lexer
  ]

let series = 
  "Parsing Tests" >::: [
    parsing_tests
  ]

let () = 
  run_test_tt_main series