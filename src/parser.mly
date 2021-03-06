%{
    open Ast;;

    let broqli_text_art = "
        /$$$$$$$  /$$$$$$$   /$$$$$$   /$$$$$$  /$$       /$$$$$$
        | $$__  $$| $$__  $$ /$$__  $$ /$$__  $$| $$      |_  $$_/
        | $$  \\ $$| $$  \\ $$| $$  \\ $$| $$  \\ $$| $$        | $$  
        | $$$$$$$ | $$$$$$$/| $$  | $$| $$  | $$| $$        | $$  
        | $$__  $$| $$__  $$| $$  | $$| $$  | $$| $$        | $$  
        | $$  \\ $$| $$  \\ $$| $$  | $$| $$/$$ $$| $$        | $$  
        | $$$$$$$/| $$  | $$|  $$$$$$/|  $$$$$$/| $$$$$$$$ /$$$$$$
        |_______/ |__/  |__/ \\______/  \\____ $$$|________/|______/
                                            \\__/                  


        "
%}

/*
 * Tokens
 */
%token NODE
%token CREATE
%token RELATION
%token WHO
%token BY
%token OF
%token FOR
%token SIZE
%token REC
%token UNDIR
%token LOAD
%token SAVE
%token ATTR
%token SHOW
%token NODES
%token RELATIONS
%token SEARCH
%token EQUAL
%token QUIT
%token BROQL

%token COMMA
%token LPAREN
%token RPAREN
%token EOEX

%token <string> STRING
%token <string> IDENT
%token <int> INT

%start <Ast.expr> main

%%

main:
    expr EOEX { $1 }
;

expr:
    | NODE ident_decl EQUAL STRING { CreateNode($2, $4) }
    | NODE node_usage { Attr(None, $2) }
    | CREATE RELATION ident_decl { CreateRelation($3, None, true) }
    | CREATE RELATION UNDIR ident_decl { CreateRelation($4, None, false) }
    | CREATE RELATION ident_decl ident_decl { CreateRelation($3, Some $4, true) }
    | RELATION ident_decl FOR node_list { CreateEdge($2, $4) }
    | WHO relation_usage node_usage { Who($2, $3, 1) }
    | WHO relation_usage BY node_usage { Who($2, $4, 1) } (* Allow BY as syntactic sugar *)
    | WHO relation_usage node_usage REC INT { Who($2, $3, $5) }
    | WHO relation_usage BY node_usage REC INT { Who($2, $4, $6) } (* Allow BY as syntactic sugar *)
    | ATTR ident_decl node_usage { Attr(Some $2, $3) }
    | ATTR ident_decl OF node_usage { Attr(Some $2, $4) }
    | SIZE node_list { Size $2 }
    | SIZE LPAREN expr RPAREN { Size $3 }
    | SEARCH STRING { Search(Object $2) }
    | SEARCH LPAREN expr RPAREN { Search $3 }
    | SHOW NODES { ShowNodes }
    | SHOW RELATIONS { ShowRelations }
    | LOAD STRING { Load $2 }
    | SAVE STRING { Save $2 }
    | QUIT { exit 0 }
    | BROQL { Msg broqli_text_art }
;

node_usage:
    ident_decl { Node $1 }
;

relation_usage:
    ident_decl { Relation $1 }
;

ident_decl:
    IDENT { Ident $1 }
;

node_list:
    separated_list(COMMA, node_usage) { NodeList $1 }
;

%%
