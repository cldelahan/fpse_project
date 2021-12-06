# FPSE Final Project

Project structure inspired by the FbDK distribution from Prof. Smith's Principles of Programming Languages class.

Implemented for code checkpoint:
1. Parsing functionality
- Built lexer and parser for a few of the basic functionalities: node creation, relation creation, `WHO` operator, `SIZE` operator
- Used `ocamllex` in `lexer.mll` and `menhir` in `parser.mly`

2. Live interpreter
- Added file `broqli.ml` to live interpret the commands given
- To test it, run `dune exec src/broqli.exe`
- Available commands:
    - Node creation: `NODE n1 = {name: "John", id: 4};`
    - Relation creation: `RELATION roommates FOR n1, n2, n3;`
    - `WHO` operator: `WHO roommates FOR n1;`
    - `SIZE` operator: `SIZE WHO roommates FOR n1`
- Note that, at this point, we have not implemented the evaluation function yet, so the interpreter just returns the same input it was given (as long as it is syntatically correct)

3. Pretty-printer
- Simple function to print any of the implemented expressions
- Found in `pp.ml`

4. Test suite for parsing
- Simple tests, at this stage just for the lexing part of the process
- Found in `tests-parsing/parsing_tests.ml`

5. Database Structure
- Created the underlying graphical structure of the database, which is a set of nodes, contained in edges, which participate in relations. 
- Database supports serialization, and thus has the ability to read and load to a file using `save` and `load`.

6. Broql Wrapper
- Interface for parser to run commands against, using the Broql module that wraps the underlying database
- Has a one-to-one correspondance with the Broqli interpreter.

6. Database test suite
- Simple tests that test core functionality of the database data structure modules. 
- Found in `tests/tests.ml` 