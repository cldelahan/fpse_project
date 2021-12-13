open Ast;;
open Graph;;

let instance = ref Broql.empty

let rec eval (exp: expr) : expr = 
  match exp with
  | Msg s -> exp
  | Node _ -> exp
  | Relation _ -> exp
  | Object _ -> exp

  | CreateNode (i, json) -> (match i with Ident node_name ->
      Broql.add_node !instance node_name ~json;
      Node i
    )
  | CreateRelation (i, l) -> (match i with Ident relation_name ->
      let node_list = List.map l ~f:(fun x -> eval !instance x) in
      Broql.add_relation !instance relation_name node_list;
      Relation i
    )

  | Who (e1, e2) -> (match (eval !instance e1, eval !instance e2) with
      | (Relation rel_ident, Node node_ident) -> failwith "TODO"
      | _ -> failwith "incorrect usage"
    )
  | Size l -> 
    let node_list = List.map l ~f:(fun x -> eval !instance x) in List.length node_list

  | Load s -> instance := Broql.load s; Msg "Database Loaded"
  | Save s -> Broql.save !instance s; Msg "Database Saved"
