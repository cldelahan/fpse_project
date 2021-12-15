(*
  Main signature for the graph module
  Author: Conner Delahanty
  Date: 12/14/21
*)

open Core

module String_Map : Map.S 

exception Exception of string

module Node : sig
  type t = (String.t) String_Map.t
  (* Get an empty node *)
  val empty : t
  (* Determine if two nodes are equal *)
  val equal : t -> t -> bool
  (* Get attribute (if exists) from node *)
  val get_attr : t -> string -> string option
  (* Update an attribute for a node. Will replace existing attribute *)
  val add_attr : t -> string -> string -> t
  (* Sets the node's attributes from a JSON string *)
  val set_from_json : t -> string -> t
  (* Converts the node to a printable string *)
  val to_string : t -> string
end

module Edge : sig
    type t = {id: string; node_ids: string list}
    (* Get an empty edge *)
    val empty : t
    (* Tell if two edges are equal *)
    val equal : t -> t -> bool
    (* Get the identifier for an edge *)
    val get_id : t -> string
    (* Create an edge from its identifier and the node ids that exist. Will not check if nodes are valid (done at higher level) *)
    val create : string -> string list -> t
    (* Gets the nodes in the edge *)
    val get_nodes : t -> string list
    (* Sets the nodes in the edge, replacing the existing nodes *)
    val set_nodes : t -> string list -> t
    (* 
    Gets the neighbors of the nodes in the edge. If directed, looks that the node in question 
    is the first node and the second is where the edge points to. If not directed, will return the rest of the list, 
    not including the original edge
    *)
    val get_neighbors : string -> bool -> t -> string list
end

module Relation : sig
    type t = {id: string; participants: (Edge.t list) String_Map.t; is_dir: bool}
    (* Returns an empty relation *)
    val empty : t
    (* Determines if two relations are equal *)
    val equal : t -> t -> bool
    (* Get the id of a relation *)
    val get_id : t -> string
    (* Add an edge (object) to the relation *)
    val add_edge_obj : t -> Edge.t -> t
    (* Add an edge (using what it takes to create an edge) to the relation *)
    val add_edge : t -> string -> string list -> t
    (* Get the neighbors for a node, using all the available edges *)
    val get_neighbors : t -> string -> string list
end

module Database : sig
    type t = {relations: Relation.t String_Map.t; 
            nodes: Node.t String_Map.t;
            paired_relation: (string) String_Map.t }
    (* Return an empty database *)
    val empty : t
    (* Return if two databases are equal *)
    val equal : t -> t -> bool
    (* Add a node, and throw exception if it already exists *)
    val add_node_exn : t -> string -> Node.t -> t
    (* Determine if the database has a node by name / id *)
    val has_node : t -> string -> bool
    (* Determine if the database has a relation by name / id *)
    val has_relation : t -> string -> bool
    (* Determine if the relation id is involved in a relation-pair *)
    val has_relation_pair : t -> string -> bool
    (* Add a relation (by object) to the database *)
    val add_relation_obj : t -> string -> Relation.t -> t
    (* Get a node if it exists *)
    val get_node : t -> string -> Node.t option
    (* Get a node or throw exception *)
    val get_node_exn : t -> string -> Node.t
    (* Get a relation if it exists *)
    val get_relation : t -> string -> Relation.t option
    (* Get a relation or throw exception *)
    val get_relation_exn : t -> string -> Relation.t
    (* Get a list of node objects in the database *)
    val get_nodes : t -> Node.t list
    (* Get a list of the node ids in the database *)
    val get_node_ids : t -> string list
    (* Add a relation to the database using what is needed to create a relation *)
    val create_relation : t -> string -> bool -> t 
    (* Add an edge to the database, passing a relation id and the string of nodes involved *)
    val add_edge : t -> string -> string list -> t
    (* Associate a pair between two relations *)
    val pair_relations : t -> string -> string -> t
    (* Get the neighbors of a node across a relation *)
    val neighbors : t -> string -> string -> string list
    (* Query the database for nodes that match a reference node *)
    val query : t -> Node.t -> string list
end

module Broql : sig
    type t = {mutable n_queries: int; 
            mutable db: Database.t; 
            mutable out_path: string }
    (* Create a broql session from a database *)
    val create : Database.t -> t
    (* Create an empty broql session *)
    val empty : t
    (* Increment the query count *)
    val do_query : t -> unit
    (* Get how many querys have been used *)
    val n_queries : t -> int
    (* 
    If the JSON string is supplied, parse it to get the attributes for a 
    node, which is added to the database. If the json string is empty, 
    then assign no attributes 
    *)
    val add_node : t -> ?json:string -> string -> unit
    (* 
    Add a relation for a set of nodes, and denote whether 
    it is directed or not 
    *)
    val create_relation : t -> string -> bool -> unit
    (*
    Add two relations, by name, that are automatically inverses of each other.
    Meaning that if at any time an edge is added of one's name, then the 
    reverse edge is added under the other's name
    *)
    val create_relation_pair : t -> string -> string -> unit
    (*
    Add an edge to an existing relation and specify the nodes that
    particiate in that relation
    *)
    val add_edge : t -> string -> string list -> unit
    (* 
    Get the named attribute of a node if it exists. If name is not specified
    then return the to-stirng / pretty print of its attributes
    *)
    val get_attr : t -> ?name:string-> string -> string option
    (* Search for all nodes in the datbase that match the json passed *)
    val search : t -> string -> string list
    (* List the names of all the nodes in the database *)
    val show_nodes : t -> string list
    (* List the names of all the relations in the database *)
    val show_relations : t -> string list
    (*
    Query all nodes that are connected to a source node across an edge. If 
    recurse is specified, then recursively query the number of times mentioned.
    Returns a list of all nodes that are connected
    *)
    val who : t -> string -> int -> string -> string list
    (* Save the state of broql to a out file *)
    val save : t -> string -> unit
    (* Loat the state of broql from an infile *)
    val load : string -> t
end