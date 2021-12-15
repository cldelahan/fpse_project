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
  | Msg s -> ff fmt "%s" s
  | Node(Ident x) -> ff fmt "%s" x
  | Relation(Ident x) -> ff fmt "%s" x
  | RelationPair(Ident x, Ident y) -> ff fmt "%s %s" x y
  | Object x -> ff fmt "%s" x
  | NodeList l -> ff fmt "%a" pp_expr_list l

  | CreateNode (Ident x, s) -> ff fmt "NODE %s = %s" x s
  | CreateRelation (Ident x, Some (Ident y), _) ->
    ff fmt "CREATE RELATION %s %s" x y
  | CreateRelation (Ident x, None, is_dir) ->
    if is_dir then
      ff fmt "CREATE RELATION %s" x
    else
      ff fmt "CREATE RELATION UNDIR %s" x
  | CreateEdge (Ident x, l) -> ff fmt "RELATION %s FOR %a" x pp_expr_list l

  | Attr (Ident x, e) -> ff fmt "ATTR %s %a" x pp_expr e
  | Who (e1, e2) -> ff fmt "WHO %a FOR %a" pp_expr e1 pp_expr e2
  | Size l -> ff fmt "SIZE %a" pp_expr_list l

  | Load s -> ff fmt "LOAD %s" s
  | Save s -> ff fmt "SAVE %s" s