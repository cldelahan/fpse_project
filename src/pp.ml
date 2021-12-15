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
  | RelationList l -> ff fmt "%a" pp_expr_list l

  | CreateNode (Ident x, s) -> ff fmt "NODE %s = %s" x s
  | CreateRelation (Ident x, Some (Ident y), _) ->
    ff fmt "CREATE RELATION %s %s" x y
  | CreateRelation (Ident x, None, is_dir) ->
    if is_dir then
      ff fmt "CREATE RELATION %s" x
    else
      ff fmt "CREATE RELATION UNDIR %s" x
  | CreateEdge (Ident x, e) -> ff fmt "RELATION %s FOR %a" x pp_expr e

  | Attr (Some (Ident x), e) -> ff fmt "ATTR %s %a" x pp_expr e
  | Attr (None, e) -> ff fmt "NODE %a" pp_expr e
  | Who (e1, e2, num_rec) -> 
    ff fmt "WHO %a %a %s" pp_expr e1 pp_expr e2 (if num_rec >= 1 then ("REC" ^ string_of_int num_rec) else "")
  | Size e -> ff fmt "SIZE %a" pp_expr e
  | Search s -> ff fmt "SEARCH %s" s

  | ShowNodes -> ff fmt "SHOW NODES"
  | ShowRelations -> ff fmt "SHOW RELATIONS"

  | Load s -> ff fmt "LOAD %s" s
  | Save s -> ff fmt "SAVE %s" s