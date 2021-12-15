open Core;;
open Ast;;
include Graph;;
open Graph;;


exception Exception of string

let instance = ref Broql.empty

let rec eval (exp: expr) : expr = 
  match exp with
  | Msg _ -> exp
  | Node _ -> exp
  | Relation _ -> exp
  | Object _ -> exp
  | NodeList _ -> exp

  | CreateNode (i, json) -> (match i with Ident node_name ->
      Broql.add_node !instance node_name ~json;
      Node i
    )
  | CreateRelation (i, l, is_dir) -> (match i with Ident relation_name ->
      let f n = (
        match eval n with
        | Node (Ident i) -> i
        | _ -> failwith "Incorrect usage"
      ) in
      let node_identifier_list = List.map l ~f in
      Broql.create_relation !instance relation_name is_dir;
      Broql.add_edge !instance relation_name node_identifier_list;
      Relation i
    )

  | Attr (Ident attribute, e) -> (
      match e with
      | Node i -> (match i with Ident node_name ->
        match Broql.get_attr !instance node_name ~name:attribute with
        | Some attr_val -> Msg attr_val
        | None -> raise @@ Exception "Missing attribute"
        )
      | _ -> failwith "Incorrect usage"
    )
  | Who (e1, e2) -> (match (eval e1, eval e2) with
      | (Relation (Ident rel_ident), Node (Ident node_ident)) -> 
        let nodes = Broql.who !instance rel_ident 0 node_ident in
        NodeList (List.map nodes ~f:(fun s -> Node (Ident s)))
      | _ -> failwith "Incorrect usage"
    )
  | Size l -> 
    let node_list = List.map l ~f:(fun x -> eval x) in Msg ("Size: " ^ (string_of_int @@ List.length node_list))

  | Load s -> instance := Broql.load s; Msg "Database Loaded"
  | Save s -> Broql.save !instance s; Msg "Database Saved"
