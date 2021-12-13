%{
    open Ast;;
%}

/*
 * Tokens
 */
%token NODE
%token RELATION
%token WHO
%token FOR
%token SIZE
%token REC
%token DIR
%token LOAD
%token SAVE
%token ATTR
%token EQUAL
%token COMMA
%token EOEX

%token <string> STRING
%token <string> IDENT

%start <Ast.expr> main

%%

main:
    expr EOEX { $1 }
;

expr:
    | NODE ident_decl EQUAL STRING { CreateNode($2, $4) }
    | RELATION ident_decl FOR node_list { CreateRelation($2, $4) }
    | WHO relation_usage FOR node_usage { Who($2, $4) }
    | ATTR ident_decl node_usage { Attr($2, $3) }
    | SIZE node_list { Size $2 }
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
