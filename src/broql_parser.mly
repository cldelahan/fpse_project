%{
    open Broql_ast;;
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
%token EQUAL
%token COMMA
%token EOEX

%token <string> OBJECT
%token <string> RELATION_NAME
%token <string> NODE_NAME

%start <Broql_ast.expr> main

%%

main:
    expr EOEX { $1 }
;

expr:
    | NODE NODE_NAME EQUAL OBJECT { CreateNode($2, $4) }
    | RELATION RELATION_NAME FOR node_list { CreateRelation($2, $4) }
    | WHO RELATION_NAME FOR NODE_NAME { Who($2, $4) }
    | SIZE expr { Size($2) }
;

node_list:
    separated_list(COMMA, NODE_NAME) { $1 }

