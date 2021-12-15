{
    open Parser;;
}

let blank = [' ' '\t' '\n' '\r']
let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']
let identchar =
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']

let object = '{' ([^'}'])* '}'
let file = ['a'-'z' 'A'-'Z' '0'-'9' '_' '-']+ ".broql"

rule token = parse
| blank+                { token lexbuf }
| "NODE"                { NODE }
| "CREATE"              { CREATE }
| "RELATION"            { RELATION }
| "WHO"                 { WHO }
| "FOR"                 { FOR }
| "SIZE"                { SIZE }
| "REC"                 { REC }
| "UNDIR"               { UNDIR }
| "LOAD"                { LOAD }
| "SAVE"                { SAVE }
| "ATTR"                { ATTR }
| "="                   { EQUAL }
| ","                   { COMMA }
| ";"                   { EOEX }
| lowercase identchar*  { IDENT (Lexing.lexeme lexbuf) }
| object                { STRING (Lexing.lexeme lexbuf) }
| file                  { STRING (Lexing.lexeme lexbuf) }
| eof                   { raise Exit }

{}