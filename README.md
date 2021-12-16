# BROQL
### Authors: Conner Delahanty, Vinicius Lepca

## Introduction

BROQL is the BRoql Ocaml Query Language, with its accompanying interpreter: BROQLI (BROQL Interpreter).
It is a DBMS (Database Management System) that combines a graph database with key-value pairs. It was built using OCaml to take advantage of the
language's functional characteristics, which render it efficient for recursive queries and pattern matching.

BROQL has a useful instruction set, allowing for the creation of nodes that hold key-value data, as well as directed or undirected relationships.
With BROQLI, the user has access to an interactive command-line interface.

## Running BROQLI
In order to run BROQLI, run `dune exec src/broqli.exe` from the project root

You can also run `dune exec src/broqli.exe -- --verbose` to activate verbose logs (useful for debugging)
or `dune exec src/broqli.exe -- --help` for a help message detailing all the available commands

## Available Operators

1. `NODE` (creation): Create a node with key-value data pairs.  
Ex: `NODE n1 = {name: "Conner", age: "22"};`

2. `NODE` (lookup): Get the details of an existing node.  
Ex: `NODE n1;`

3. `CREATE RELATION`: Create a new relation, which can be directed (default) or undirected (add keyword `UNDIR`). A directed relation can also be created in pairs.  
Ex: `CREATE RELATION loves is_loved;`   
`CREATE RELATION visited;`  
`CREATE RELATION UNDIR roommates;`  

4. `RELATION`: Add an edge to an existing relation. Directed relations receive only two nodes at a time, while undirected can receive many.  
Ex: `RELATION loves FOR FOR n1, n2;`  
`RELATION roommates FOR n1, n2, n3, n4;`

5. `ATTR`: Get a specific attribute from a node. Keyword `OF` is optional.  
Ex: `ATTR name n1;`  
`ATTR name OF n1;`

6. `SEARCH`: Get all nodes that meet a certain search criterion. Can take other expressions as argument by using parenthesis.  
Ex: `SEARCH {name: "Conner"};`  
 `SEARCH (NODE n1);`

6. `WHO`: Get nodes that satisfy a certain relation for a node. Can be used to traverse relations recursively. Keyword `BY` is optional for readability.   
Ex: `WHO roommates n1`;  
`WHO loves n3;`  
`WHO is_loved BY n2 REC 2;`

7. `SIZE`: Get size of a list of nodes or relations. Can take other expressions as argument by using parenthesis.  
Ex: `SIZE (WHO roommates n1);`  
 `SIZE (SHOW RELATIONS);`

8. `SHOW NODES`: Get all the nodes in the database  
Ex: `SHOW NODES;`

9. `SHOW RELATIONS`: Get all the relations in the database  
Ex: `SHOW RELATIONS;`

10. `LOAD`: Load a stored database file (.broql).  
Ex: `LOAD db.broql;`

11. `SAVE`: Save the state of the database to a file (.broql).  
Ex: `SAVE db.broql;`

12. `BROQL` or `BROQLI`: Try it out!  
Ex: `BROQL;`  
`BROQLI;`

**COMMAND LINE OPTIONS:**  
  `--verbose` Output debug information  
  `-help`  Display list of options  
  `--help`  Display list of options

## Basic Project Structure
- Parsing Functionality
    - Built lexer (using `ocamllex`) and parser (using `menhir`) to interpret the live commands
    - Lexer: `src/lexer.mll`
    - Parser: `src/parser.mly`
- Live interpreter (BROQLI)
    - Command-line interface to interface with BROQL
    - `src/broqli.ml`
- Evaluation function
    - A recursive function that takes in any BROQL expression, runs the command, and returns the appropriate expression
    - `src/eval.ml`
- Pretty-printer
    - Used to print the executed commands
    - `src/pp.ml`
- Abstract Syntax Tree
    - File where we define all the possible expression types for our interpreter
    - `src/ast.ml`
- BROQL
    - The library functions used to interface with the database
    - Contains several helper internal modules, such as `Node`, `Relation`, and `Database`
    - Module `Broql` is the only one needed to interface with the database, since it wraps around all the functionalities
    - `src/graph.ml` and `src/graph.mli`
- Debugging utility functions
    - Simple functions to make it easier to debug by steps
    - `src/debugutils.ml`
- Test suite
    - Complete test suite for most of our files, using OUnit2
    - `tests/tests.ml`

## Testing
To execute our test suite, run `dune test` from the project root. Our current code coverage is as follows:
- 68% src/debugutils.ml (not as relevant for testing)
- 100% src/eval.ml
- 79% src/graph.ml
- 92% src/lexer.ml
- 51% src/parser.ml (harder to test since the file is auto generated)
- 24% src/pp.ml (not as relevant for testing, only a few of the cases are actually used)

These numbers are also available by running `bisect-ppx-report html` after executing the tests, then opening `_coverage/index.html` with the browser

There are two pre-populated BROQL files that can be used for exploration with the command line: `test.broql` and `family.broql` 

## Acknowledgements
Interpreter structure inspired by the FbDK distribution from Prof. Smith's Principles of Programming Languages class.

We would like to thank Dr. Scott Smith, as well as our CA's Zach and Alison, for their support.
