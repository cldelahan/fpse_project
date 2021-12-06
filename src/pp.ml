open Broql_ast;;

let ff = Format.printf

let rec pp_expr fmt =
  function
  | Node(Ident(x)) -> ff fmt "%s" x
  | Relation(Ident(x)) -> ff fmt "%s" x
  | Object(x) -> ff fmt "%s" x
  | NodeList(l) -> ff fmt "TODO"

  | _ -> ff fmt "TODO"