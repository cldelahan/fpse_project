open Ast;;

let ff = Format.fprintf

let rec pp_expr fmt =
  let rec pp_expr_list fmt =
    function
    | [] -> ff fmt ""
    | hd :: [] -> ff fmt "%a" pp_expr hd
    | hd :: tl -> ff fmt "%a, %a" pp_expr hd pp_expr_list tl
  in
  function
  | Node(Ident x) -> ff fmt "%s" x
  | Relation(Ident x) -> ff fmt "%s" x
  | Object x -> ff fmt "%s" x

  | CreateNode (Ident x, s) -> ff fmt "NODE %s = %s" x s
  | CreateRelation (Ident x, l) -> ff fmt "RELATION %s FOR %a" x pp_expr_list l

  | Who (e1, e2) -> ff fmt "WHO %a FOR %a" pp_expr e1 pp_expr e2
  | Size e -> ff fmt "SIZE %a" pp_expr e

  | Load s -> ff fmt "LOAD %s" s
  | Save s -> ff fmt "SAVE %s" s