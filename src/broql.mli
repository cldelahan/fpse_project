(* This module defines the abstract syntax tree that will be used to parse the BROQL commands *)
module Ast: sig
  type ident = Broqlast.ident = 
    | Ident of string
  
  type expr = Broqlast.expr =
    (* basic and literal *)
    | Node of ident
    | Relation of ident
    | Object of string (* JSON formatted *)
    | NodeList of Node list

    (* Creating nodes and relations *)
    | CreateNode of ident * expr (* expr should evaluate to Object *)
    | CreateRelation of ident * expr (* expr should evaluate to NodeList *)
    | CreateRelationDir of ident * expr * expr (* expr * expr should evaluate to Node * NodeList *)

    | Attr of ident * expr (* expr should evaluate to Node *)

    | Search expr (* expr should evaluate to Object *)
    
    | Who of expr * expr (* expr * expr should evaluate to Relation * Node *)

    | Size of expr (* expr should evaluate to NodeList *)

    | Extend of expr * expr (* expr * expr should evaluate to NodeList * NodeList *)

    | Load of string
    | Save of string

  val show_expr: expr -> string
  val pp_expr: Format.formatter -> expr -> unit
    [@@ocaml.toplevel_printer]
end

(* A parser that converts tokens to an AST expression *)
module Parser: sig
  type token
  val main:
    (Lexing.lexbuf -> token) -> Lexing.lexbuf -> Ast.expr
end

(* A lexer that converts the text input into tokens *)
module Lexer: sig
  val token: Lexing.lexbuf -> Parser.token
end

(* Used to pretty-print expressions *)
module Pp: sig
  val show_expr: Ast.expr -> string
  val pp_expr: Format.formatter -> Ast.expr -> unit
end

(* The actual interpreter, it takes in a parsed command and returns the output expression *)
module Interpreter: sig
  val eval: Ast.expr -> Ast.expr
end

(* Nodes in the graph*)
module Node: sig
  (* The identifier for the node *)
  val ident: string
  (* The json data for the node*)
  type t
  (* JSON Key Values *)
  val attributes: t
  (* Get the attribute from a node*)
  val get_attr: t -> t option
end

(* Collection of nodes in a relation *)
module Relation_directed: sig
  (* The identifier for the relation *)
  val ident: string
  (* Who did the relation to what list of Nodes *)
  val actions: Node Map.t
end

(* Collection of nodes in a non-directed relation *)
module Relation_nondirected: sig
  (* The identifier for the relation *)
  val ident: string
  (* All nodes participating in relation *)
  val participants: Node list
end


(* The underlying graph structure for the Database*)
module Graph: sig
  (* List of nodes *)
  val nodes: Node list
  (* List of relations *)
  val relations_directed: Relation_directed list
  (* List of relations *)
  val relations_nondirected: Relation_nondirected list

  (* Create node *)
  val add_node: string -> string -> Node
  (* Create relation *)
  val add_relation: string -> Node list -> Relation

  (* Get edges from node *)
  val get_edges: Node -> Relation list
  (* Get neighbors from node *)
  val get_neighbors: Node -> Node list
  (* Get all neighbors using the relation *)
  val get_relation_neighbors: Node -> Relation -> Node list
end

module DB (module Graph): sig
  module Graph = Graph
  (* Graph that stores data*)
  val g: Graph
  (* Type of the operation *)
  type operation
  (* Type of the response *)
  type response

  (* IO for the database *)
  val load: string -> Graph;
  val write: string -> ();

  (* Perform an operation on the graph*)
  val act: operation -> response


(* module Options: sig
  val options: (Arg.key * Arg.spec * Arg.doc) list
end

module Version: sig
  val version: string
  val build_date: string
end *)