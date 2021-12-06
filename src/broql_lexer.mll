{
    open Broql_parser;;
}

let blank = [' ' '\t' '\n' '\r']
let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']
let identchar =
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']

let object = '{' _* '}'
let file = _+ ".broql" 

rule token = parse
| blank+                { token lexbuf }
| "NODE"                { NODE }
| "RELATION"            { RELATION }
| "WHO"                 { WHO }
| "FOR"                 { FOR }
| "SIZE"                { SIZE }
| "REC"                 { REC }
| "DIR"                 { DIR }
| "LOAD"                { LOAD }
| "SAVE"                { SAVE }
| "="                   { EQUAL }
| ","                   { COMMA }
| ";"                   { EOEX }
| lowercase identchar*  { IDENT (Lexing.lexeme lexbuf) }
| object | file         { STRING (Lexing.lexeme lexbuf) }
| eof                   { raise Exit }

{}