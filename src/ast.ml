type ident = Ident of string [@@deriving show { with_path = false }]

type expr =
  (* basic and literal *)
  | Msg of string (* to return messages *)
  | Node of ident
  | Relation of ident
  | Object of string (* JSON formatted *)
  | NodeList of expr list

  (* Creating nodes and relations *)
  | CreateNode of ident * string (* string is JSON formatted *)
  | CreateRelation of ident * (expr list) * bool (* expr list should evaluate to Node list *)

  | Attr of ident * expr (* expr should evaluate to Node *)

  (* | Search of expr (* expr should evaluate to Object *) *)

  | Who of expr * expr (* expr * expr should evaluate to Relation * Node *)

  | Size of expr list (* expr list should evaluate to Node list *)

  (* | Extend of expr * expr (* expr * expr should evaluate to NodeList * NodeList *) *)

  | Load of string
  | Save of string