(*
  Main graph module with useful implementations.
  Author: Conner Delahanty
  Date: 12/1/21
*)
(* [@@@ocaml.warning "-33"] *)

open Core

exception Exception of string
module String_Map = Map.Make(String)

(* Lib contains a collection of helper library functions *)
module Lib = struct
  (* 
    "Explode" a string to a character list 
    Implementation provided by Pierre Weis
    https://caml.inria.fr/pub/old_caml_site/Examples/oc/basics/explode.ml
  *)
  let explode s =
    let rec expl i l =
      if i < 0 then l else
      expl (i - 1) (s.[i] :: l) in
    expl (String.length s - 1) [];;

  (* 
    "Implode" a character list back to a string 
  *)
  let implode l = 
    let acc str c = 
      String.concat [str; Char.to_string c] in
    List.fold l ~init:"" ~f:acc

  (*
      Flatten a list of lists
  *)
  let flatten (a: 'a list list) = 
    let rec flatten' (res: 'a list) (arr: 'a list list) = 
      match arr with 
      | [] -> res
      | v_l :: r_l -> (
        match v_l with 
          | v :: r -> flatten' (v::res) (r :: r_l)
          | _ -> flatten' res r_l) in
    List.rev @@ flatten' [] a

end

module Node = struct
  type t = (String.t) String_Map.t
  let empty = String_Map.empty
  (* Add a specific key-value attribute *)
  (* To-do -- need to check for duplicates *)
  let add_attr (a: t) (k: string) (v: string) =
    String_Map.add_exn a ~key:k ~data:v 
  (* Get a specific attribute by key *)
  let get_attr (a: t) (k: string) : string option = 
    String_Map.find a k
  (* Crudely parses json string for key-value attributes *)
  (*
  let rec set_from_json (a: t) (json: string) =
    match Lib.explode json with 
    | k::':'::v::','::rest -> set_from_json (add_attr a (Lib.implode k) (Lib.implode v)) rest
    | _ -> a
  *)
  let set_from_json (a: t) (json: string) =
    let acc a pair = 
      match pair with
      | (k, v) -> add_attr a k (Yojson.Basic.Util.to_string v) in
    try
      let l = Yojson.Basic.from_string json in
        let v = Yojson.Basic.Util.values l in
        let k = Yojson.Basic.Util.keys l in
        List.fold (List.zip_exn k v) ~init:a ~f:acc
    with Yojson.Json_error "Blank input data" -> a

  let to_string (a: t) = 
    let rec acc keys s = 
      match keys with
      | k :: rest ->  if String.(=) s "" then acc rest (String.concat [k; ": "; String_Map.find_exn a k])
                      else acc rest (String.concat [s;" "; k; ": "; String_Map.find_exn a k])
      | [] -> s in
    acc (String_Map.keys a) ""

  let sexp_of_t (a: t) = 
    String_Map.sexp_of_t String.sexp_of_t a

  let t_of_sexp (sexp: Sexp.t) = 
    String_Map.t_of_sexp String.t_of_sexp sexp

end


module Edge = struct
  type t = {id: string; node_ids: string list; is_dir: bool} [@@deriving sexp]
  let empty = {id = ""; node_ids = []; is_dir = false}
  let get_id (a: t) = a.id
  let create (eid: string) (nodes: string list) (dir: bool) = 
    {id = eid; node_ids = nodes; is_dir = dir}
  let get_nodes (a: t) = a.node_ids
  let set_nodes (a: t) (nodes: string list) = 
    {id = a.id; node_ids = nodes; is_dir = a.is_dir}
  let get_neighbors (node: string) (a: t) = 
    if a.is_dir then match a.node_ids with
      | n :: v when String.(=) n node -> v
      | _ -> []
    else List.filter a.node_ids ~f:(fun n -> not @@ String.(=) node n)
end

module Relation = struct
  (* Map from node id to list of edges that node is invloved in *)
  type t = {id: string; participants: (Edge.t list) String_Map.t} [@@deriving sexp]
  let empty = {id =  ""; participants = String_Map.empty}
  let get_id (a: t) = a.id
  let _add_edge_to_node (a: t) (node_id: string) (edge: Edge.t) = 
    match String_Map.find a.participants node_id with
    | Some(v) -> let p' = String_Map.remove a.participants node_id in {id = a.id; participants = String_Map.add_exn p' ~key:node_id ~data:(edge::v)} (* Cons if exists *)
    | None -> {id = a.id; participants = String_Map.add_exn a.participants ~key:node_id ~data:[edge]}

  let add_edge (a: t) (edge: Edge.t) = 
    (* Define fold accumulator *)
    let acc rel node = _add_edge_to_node rel node edge in
    (* Set map from node to edge *)
    List.fold (Edge.get_nodes edge) ~init:a ~f:acc
  (* 
    ID for the relation is the same as the node.
    Can change this so we can target specific edges
  *)
  let add_edge (a: t) (id: string) (nodes: string list) (is_dir: bool)=
    let e = Edge.create id nodes is_dir in
    add_edge a e

  let get_neighbors (a: t) (node_id: string) =
    match String_Map.find a.participants node_id with
    | Some v -> List.map v ~f:(Edge.get_neighbors node_id) |> Lib.flatten
    | None -> []

end

module Database = struct
  type t = {relations: (Relation.t) String_Map.t; 
            nodes: (Node.t) String_Map.t} [@@deriving sexp]
  let empty = {relations = String_Map.empty; nodes = String_Map.empty}
  let add_node_exn (db: t) (id: string) (node: Node.t) = 
    let new_nodes = String_Map.add_exn db.nodes ~key:id ~data:node in
    {relations = db.relations; nodes = new_nodes}

  let has_node (db: t) (id: string) = 
    match String_Map.find db.nodes id with
    | Some _ -> true
    | None -> false
  
  let has_relation (db: t) (id: string) = 
    match String_Map.find db.relations id with
    | Some _ -> true
    | None -> false

  (* Add a relation to the database without exception *)
  let _add_relation (db: t) (id: string) (relation: Relation.t) = 
    if has_relation db id then 
      let m = String_Map.remove db.relations id in
      let relations' = String_Map.add_exn m ~key:id ~data:relation in
      {relations = relations'; nodes = db.nodes}
    else
      let relations' = String_Map.add_exn db.relations ~key:id ~data:relation in
      {relations = relations'; nodes = db.nodes}

  let get_node (db: t) (id: string) = 
    String_Map.find db.nodes id

  let get_node_exn (db: t) (id: string) = 
    String_Map.find_exn db.nodes id

  let get_relation (db: t) (id: string) = 
    String_Map.find db.relations id

  let get_relation_exn (db: t) (id: string) = 
    String_Map.find_exn db.relations id

  let get_nodes (db: t) = 
    List.map (String_Map.keys db.nodes) ~f:(get_node db)

  let get_node_ids (db: t) = 
    String_Map.keys db.nodes

  (* Add a relation + edge to the database*)
  let add_relation (db: t) (rel_id: string) (nodes: string list) (is_dir: bool) = 
    if has_relation db rel_id then 
      let r = get_relation_exn db rel_id in
      let r' = Relation.add_edge r rel_id nodes is_dir in
      _add_relation db rel_id r'
    else 
      let r = Relation.add_edge Relation.empty rel_id nodes is_dir in
      _add_relation db rel_id r


  (* Get all related nodes to n *)
  (*  let neighbors (db: t) (rel_id: string) (node_id: string) *)
  let neighbors (db: t) (rel_id: string) (node_id: string) = 
    let r = get_relation_exn db rel_id in
    Relation.get_neighbors r node_id
  
end

module Broql = struct 
  (* TODO: Figure out best way to interact *)
  type t = {mutable n_queries: int; 
            mutable db: Database.t; 
            mutable out_path: string} [@@deriving sexp]
    
  let empty = {n_queries = 0; db = Database.empty; out_path = "db.broql"}
  let do_query (a: t) = a.n_queries <- a.n_queries + 1
  let n_queries (a: t) = a.n_queries

  let _exist_nodes (db: Database.t) (nodes: string list) = 
    let acc b n_id = b && Database.has_node db n_id in
    List.fold nodes ~init:true ~f:acc

  (* Interpreter-level functions. Correspond to command-line commands *)
  let add_node (a: t) ?(json = "") (id: string) = 
    let n = Node.set_from_json Node.empty json in
      a.db <- Database.add_node_exn a.db id n

  let add_relation (a: t) (rel_id: string) (nodes: string list) (is_dir: bool) = 
    if not @@ _exist_nodes a.db nodes then raise @@ Exception "Nodes do not exist"
    else 
      let db' = Database.add_relation a.db rel_id nodes is_dir in a.db <- db'

  let get_attr (a: t) ?(name) (node_id: string) = 
    let n_opt = Database.get_node (a.db) node_id in
      match (n_opt, name) with 
      | (Some (n), Some (attr_name)) -> Node.get_attr n attr_name
      | (Some (n), None) -> Some(Node.to_string n)
      | (None, _) -> None

  let show_nodes (a: t) = 
    Database.get_node_ids a.db

  let show_relations (a: t) = 
    String_Map.keys a.db.relations

  let who (a: t) (rel_id: string) (node_id: string) = 
    Database.neighbors a.db rel_id node_id

  let save (a: t) (path: string) = 
    let str = Sexp.to_string (sexp_of_t a) in
      let oc = Out_channel.create path in
        Printf.fprintf oc "%s\n" str;
        Out_channel.close oc

  let load (path: string) = 
    let ic = In_channel.create path in
    try
      match In_channel.input_line ic with 
      | None -> In_channel.close ic; empty
      | Some (str) -> In_channel.close ic; let sexp = Sexp.of_string str in
                      t_of_sexp sexp
    with _ -> 
      empty

  (* Put broql operations here *)
end
