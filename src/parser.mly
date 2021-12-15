%{
    open Ast;;
%}

/*
 * Tokens
 */
%token NODE
%token CREATE
%token RELATION
%token WHO
%token BY
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
%token COMMA
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
    | CREATE RELATION ident_decl { CreateRelation($3, None, true) }
    | CREATE RELATION UNDIR ident_decl { CreateRelation($4, None, false) }
    | CREATE RELATION ident_decl ident_decl { CreateRelation($3, Some $4, true) }
    | RELATION ident_decl FOR node_list { CreateEdge($2, $4) }
    | WHO relation_usage node_usage { Who($2, $3, 1) }
    | WHO relation_usage BY node_usage { Who($2, $4, 1) } (* Allow BY as syntactic sugar *)
    | WHO relation_usage node_usage REC INT { Who($2, $3, $5) }
    | WHO relation_usage BY node_usage REC INT { Who($2, $4, $6) } (* Allow BY as syntactic sugar *)
    | ATTR ident_decl node_usage { Attr($2, $3) }
    | SIZE node_list { Size $2 }
    | SEARCH STRING { Search($2) }
    | SHOW NODES { ShowNodes }
    | SHOW RELATIONS { ShowRelations }
    | LOAD STRING { Load $2 }
    | SAVE STRING { Save $2 }
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
    separated_list(COMMA, node_usage) { $1 }
;

%%
