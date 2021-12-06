{
    open Broql_parser;;
}

let blank = [' ' '\t' '\n' '\r']
let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']
let identchar =
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']

rule token = parse
| blank+                { token lexbuf }
| "Node"                { NODE }
| "Relation"            { RELATION }
| "Who"                 { WHO }
| "For"                 { FOR }
| "Size"                { SIZE }
| "Rec"                 { REC }
| 
