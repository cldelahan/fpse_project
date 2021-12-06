open Ast;;

let ff = Format.fprintf

let pp_expr fmt =
  function
  | Node(Ident(x)) -> ff fmt "%s" x
  | Relation(Ident(x)) -> ff fmt "%s" x
  | Object(x) -> ff fmt "%s" x

  | _ -> ff fmt "TODO"