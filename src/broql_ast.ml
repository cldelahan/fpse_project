open Graph

type ident = Ident of string [@@deriving show { with_path = false }]

type expr =
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