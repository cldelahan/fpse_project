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

(* module Options: sig
  val options: (Arg.key * Arg.spec * Arg.doc) list
end

module Version: sig
  val version: string
  val build_date: string
end *)