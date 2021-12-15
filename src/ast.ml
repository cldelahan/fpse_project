type ident = Ident of string [@@deriving show { with_path = false }]

type expr =
  (* basic and literal *)
  | Msg of string (* to return messages *)
  | Node of ident
  | Relation of ident
  | RelationPair of ident * ident
  | Object of string (* JSON formatted *)
  | NodeList of expr list
  | RelationList of expr list

  (* Creating nodes and relations *)
  | CreateNode of ident * string (* string is JSON formatted *)
  | CreateRelation of ident * (ident option) * bool 
  | CreateEdge of ident * (expr list) (* expr list should evaluate to Node list *)

  | Attr of (ident option) * expr (* expr should evaluate to Node *)

  | Search of string (* TODO: swich to Search of expr and make recursive; expr should evaluate to Object *)

  | Who of expr * expr * int (* expr * expr should evaluate to Relation * Node *)

  | Size of expr list (* expr list should evaluate to Node list *)

  | ShowNodes
  | ShowRelations

  | Load of string
  | Save of string