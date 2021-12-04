(*
  Main graph module with useful implementations.
  Author: Conner Delahanty
  Date: 12/1/21
*)
[@@@ocaml.warning "-33"]
[@@@ocaml.warning "-6"]
open Core

module String_Map = Map.Make(String)

module Node = struct
  type t = (String.t) String_Map.t
  let empty = String_Map.empty
  let add_attr (a: t) (k: string) (v: string) =
    String_Map.add_exn a k v 
  let get_attr (a: t) (k: string) : string option = 
    String_Map.find a k
end


module Edge = struct
  type t = {id: string; node_ids: string list}
  let empty = {id = ""; node_ids = []}
  let get_id (a: t) = a.id
  let get_nodes (a: t) = a.node_ids
  let set_nodes (a: t) (nodes: string list) = 
    {id = a.id; node_ids = nodes}
end

module Relation = struct
  (* Map from node id to edge *)
  type e = (Edge.t) String_Map.t
  type t = {id: string; participants: e}
  let empty = {id =  ""; participants = String_Map.empty}
end

module Database = struct
  type n = (Node.t) String_Map.t
  type r = (Relation.t) String_Map.t
  type t = {relations: r; nodes: n}
  let empty = {relations = String_Map.empty; nodes = String_Map.empty}
  let add_node (db: t) (id: string) (node: Node.t) = 
    let new_nodes = String_Map.add_exn db.nodes id node in
    {relations = db.relations; nodes = new_nodes}

  let add_relation (db: t) (id: string) (relation: Relation.t) = 
    let new_relations = String_Map.add_exn db.relations id relation in
    {relations = new_relations; nodes = db.nodes}
end

module Broql = struct 
  type t = {n_queries: int; db: Database.t; out_path: string}
  
  (* Put broql operations here *)
end
