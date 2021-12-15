(*
  Main graph module with useful implementations.
  Author: Conner Delahanty
  Date: 12/1/21
*)

open Core

exception Exception of string
module String_Map = Map.Make(String)
module String_Set = Set.Make(String)

module Node = struct
  type t = (String.t) String_Map.t [@@deriving sexp]
  let empty = String_Map.empty
  let equal (n1: t) (n2: t) = String_Map.equal String.equal n1 n2
  (* Add a specific key-value attribute *)
  let get_attr (a: t) (k: string) : string option = 
    String_Map.find a k
  let add_attr (a: t) (k: string) (v: string) =
    match get_attr a k with
    | Some (v) -> let a' = String_Map.remove a k in String_Map.add_exn a' ~key:k ~data:v
    | None -> String_Map.add_exn a ~key:k ~data:v 
  let matches ~(query: t) (a: t) = 
    let acc s v = s && (Option.equal String.equal (String_Map.find query v) (String_Map.find a v)) in
    List.fold (String_Map.keys query) ~init:true ~f:acc 
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
    let rec acc keys s = (
      match keys with 
      | [] -> s ^ "}"
      | hd :: [] -> acc [] (String.concat [s; hd; ": "; String_Map.find_exn a hd])
      | hd :: tl -> acc tl (String.concat [s; hd; ": "; String_Map.find_exn a hd; ", "])
    ) in acc (String_Map.keys a) "{"
end


module Edge = struct
  type t = {id: string; node_ids: string list} [@@deriving sexp]
  let equal (e1: t) (e2: t) = 
    (List.equal String.(=) e1.node_ids e2.node_ids) &&
    (String.(=) e1.id e2.id)
  let empty = {id = ""; node_ids = []}
  let get_id (a: t) = a.id
  let create (eid: string) (nodes: string list) = 
    {id = eid; node_ids = nodes}
  let get_nodes (a: t) = a.node_ids
  let set_nodes (a: t) (nodes: string list) = 
    {id = a.id; node_ids = nodes}
  let get_neighbors (node: string) (is_dir: bool) (a: t) = 
    if is_dir then match a.node_ids with
      | n :: v :: _ when String.(=) v node -> [n]
      | _ -> []
    else List.filter a.node_ids ~f:(fun n -> not @@ String.(=) node n)
end

module Relation = struct
  (* Map from node id to list of edges that node is invloved in *)
  type t = {id: string; participants: (Edge.t list) String_Map.t; is_dir: bool} [@@deriving sexp]
  let empty = {id =  ""; participants = String_Map.empty; is_dir = true}
  let equal (r1: t) (r2: t) = 
    (String_Map.equal (List.equal Edge.equal) r1.participants r2.participants) &&
    (String.(=) r1.id r2.id) &&
    (Bool.(=) r1.is_dir r2.is_dir)
  let create (id_s: string) (is_dir_s: bool) = {id = id_s; participants = String_Map.empty; is_dir = is_dir_s}
  let get_id (a: t) = a.id
  let _add_edge_to_node (a: t) (node_id: string) (edge: Edge.t) = 
    match String_Map.find a.participants node_id with
    | Some(v) -> let p' = String_Map.remove a.participants node_id in {id = a.id; participants = String_Map.add_exn p' ~key:node_id ~data:(edge::v); is_dir = a.is_dir} (* Cons if exists *)
    | None -> {id = a.id; participants = String_Map.add_exn a.participants ~key:node_id ~data:[edge]; is_dir = a.is_dir}

  let add_edge_obj (a: t) (edge: Edge.t) = 
    (* Define fold accumulator *)
    let acc rel node = _add_edge_to_node rel node edge in
    (* Set map from node to edge *)
    List.fold (Edge.get_nodes edge) ~init:a ~f:acc
  (* 
    ID for the relation is the same as the node.
    Can change this so we can target specific edges
  *)
  let add_edge (a: t) (id: string) (nodes: string list)=
    if a.is_dir && not (List.length nodes = 2) then raise @@ Exception "Directed relations only accept two participants"
    else
    let e = Edge.create id nodes in
    add_edge_obj a e

  let get_neighbors (a: t) (node_id: string) =
    match String_Map.find a.participants node_id with
    | Some v -> List.map v ~f:(Edge.get_neighbors node_id a.is_dir) |> List.concat
    | None -> []

end

module Database = struct
  type t = {relations: (Relation.t) String_Map.t; 
            nodes: (Node.t) String_Map.t;
            paired_relation: (string) String_Map.t } [@@deriving sexp]
  let empty = {relations = String_Map.empty; nodes = String_Map.empty; paired_relation = String_Map.empty}
  let equal (d1: t) (d2: t) = 
    (String_Map.equal Relation.equal d1.relations d2.relations) &&
    (String_Map.equal Node.equal d1.nodes d2.nodes) &&
    (String_Map.equal String.(=) d1.paired_relation d2.paired_relation)
  let has_node (db: t) (id: string) = 
    match String_Map.find db.nodes id with
    | Some _ -> true
    | None -> false
  let add_node_exn (db: t) (id: string) (node: Node.t) = 
    if has_node db id then raise @@ Exception "Node already exists"
    else
    let new_nodes = String_Map.add_exn db.nodes ~key:id ~data:node in
    {relations = db.relations; nodes = new_nodes; paired_relation = db.paired_relation}


  let has_relation (db: t) (id: string) = 
    match String_Map.find db.relations id with
    | Some _ -> true
    | None -> false

  let has_relation_pair (db: t) (id: string) = 
    match String_Map.find db.paired_relation id with
    | Some _ -> true
    | None -> false

  (* Add a relation to the database without exception *)
  let add_relation_obj (db: t) (id: string) (relation: Relation.t) = 
    if has_relation db id then 
      let m = String_Map.remove db.relations id in
      let relations' = String_Map.add_exn m ~key:id ~data:relation in
      {relations = relations'; nodes = db.nodes; paired_relation = db.paired_relation}
    else
      let relations' = String_Map.add_exn db.relations ~key:id ~data:relation in
      {relations = relations'; nodes = db.nodes; paired_relation = db.paired_relation}

  let get_node (db: t) (id: string) = String_Map.find db.nodes id
  let get_node_exn (db: t) (id: string) = String_Map.find_exn db.nodes id
  let get_relation (db: t) (id: string) = String_Map.find db.relations id
  let get_relation_exn (db: t) (id: string) = String_Map.find_exn db.relations id
  let get_nodes (db: t) = List.map (String_Map.keys db.nodes) ~f:(get_node_exn db)
  let get_node_ids (db: t) = String_Map.keys db.nodes


  let create_relation (db: t) (rel_id: string) (is_dir: bool) = 
    if has_relation db rel_id then 
      raise @@ Exception "Relation ID already exists"
    else 
      let r = Relation.create rel_id is_dir in
      add_relation_obj db rel_id r


  (* Add a edge to the database*)
  let add_edge (db: t) (rel_id: string) (nodes: string list) = 
    if has_relation db rel_id then 
      let r = get_relation_exn db rel_id in
      let r' = Relation.add_edge r rel_id nodes in
      let db' = add_relation_obj db rel_id r' in
      (* Assumes the bi-directed relations are already created *)
      if has_relation_pair db rel_id then
        let r2 = get_relation_exn db (String_Map.find_exn db.paired_relation rel_id) in
        let r2' = Relation.add_edge r2 rel_id (List.rev nodes) in
        add_relation_obj db' (String_Map.find_exn db.paired_relation rel_id) r2'
      else
        db'
    else 
      raise @@ Exception "Relation ID does not exist"

  (* Pair two relation names *)
  let pair_relations (db: t) (rel_id_fwd: string) (rel_id_bkw: string) = 
    let p = db.paired_relation in
    let p = String_Map.add_exn p ~key:rel_id_fwd ~data:rel_id_bkw in
    let p = String_Map.add_exn p ~key:rel_id_bkw ~data:rel_id_fwd in
    {relations = db.relations; nodes = db.nodes; paired_relation = p}

  (* Get all related nodes to n *)
  (*  let neighbors (db: t) (rel_id: string) (node_id: string) *)
  let neighbors (db: t) (rel_id: string) (node_id: string) = 
    let r = get_relation_exn db rel_id in
    Relation.get_neighbors r node_id

  (* Query a node based off a query node map *)
  let query (db: t) (query_node: Node.t) = 
    let node_ids = get_node_ids db in 
    List.filter node_ids ~f:(fun v -> Node.matches ~query:(query_node) (get_node_exn db v))

end

module Broql = struct 
  (* Underling BROQL interface *)
  type t = {mutable n_queries: int; 
            mutable db: Database.t; 
            mutable out_path: string} [@@deriving sexp]

  let create (a: Database.t) = {n_queries = 0; db = a; out_path = "db.broql"}
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

  let create_relation (a: t) (rel_id: string) (is_dir: bool) = 
    let db' = Database.create_relation a.db rel_id is_dir in a.db <- db'

  (* Add a bi-labeled relation + edge to the database *)
  let create_relation_pair (a: t) (rel_id_fwd: string) (rel_id_bkw: string) = 
    let db' = Database.create_relation a.db rel_id_fwd true in
    let db' = Database.create_relation db' rel_id_bkw true in
    let db' = Database.pair_relations db' rel_id_fwd rel_id_bkw in
    a.db <- db'

  let add_edge (a: t) (rel_id: string) (nodes: string list) = 
    if not @@ _exist_nodes a.db nodes then raise @@ Exception "Nodes do not exist"
    else 
      let db' = Database.add_edge a.db rel_id nodes in a.db <- db'

  let get_attr (a: t) ?(name) (node_id: string) = 
    let n_opt = Database.get_node (a.db) node_id in
    match (n_opt, name) with 
    | (Some (n), Some (attr_name)) -> Node.get_attr n attr_name
    | (Some (n), None) -> Some(Node.to_string n)
    | (None, _) -> None

  let search (a: t) (json: string) = 
    let query_node = Node.set_from_json Node.empty json in
    Database.query a.db query_node

  let show_nodes (a: t) = 
    Database.get_node_ids a.db

  let show_relations (a: t) = 
    String_Map.keys a.db.relations

  let rec who' (a: t) (rel_id: string) (node_ids: string list) (times: int) = 
    if times < 1 then node_ids
    else
      let neighbors = String_Set.empty in
      (* Define neighbor-getting function *)
      let expl n = Database.neighbors a.db rel_id n in
      (* Define set-putting function *)
      let insert s v = String_Set.add s v in
      (* Get all neighbors *)
      let new_set = List.map node_ids ~f:expl |> List.concat |> List.fold ~init:neighbors ~f:insert in
      (* Recurse *)
      who' a rel_id (String_Set.to_list new_set) (times - 1)


  let who (a: t) (rel_id: string) (times: int) (node_id: string) = 
    if times=0 then [node_id] 
    else if times=1 then Database.neighbors a.db rel_id node_id
    else who' a rel_id [node_id] times

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

end