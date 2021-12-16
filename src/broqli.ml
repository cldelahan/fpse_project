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

let toplevel_loop verbose =
  (* Prints exceptions and associated stack traces *)
  let print_exception ex =
    Format.printf "Exception: %s\n" (Printexc.to_string ex);
    Format.print_flush ();
    if verbose then Printexc.print_backtrace stdout else ();
    flush stdout
  in 
  (* Parse stdin *)
  let safe_parse () =
    try
      let lexbuf = Lexing.from_channel stdin in
      Some ( Parser.main Lexer.token lexbuf )
    with Exit -> exit 0
       | ex -> print_exception ex; None
  in
  (* Interpret and print. Exceptions are caught and reported. But the toploop is not aborted *)
  let safe_interpret_and_print ast =
    try
      let result = Eval.eval ast in
      Format.printf "==> %a\n" Pp.pp_expr result
    with ex ->
      print_exception ex
  in
  Format.print_flush ();
  Format.printf "%s" broqli_text_art;
  while true do
    Format.printf "# ";
    Format.print_flush ();
    let parse_result = safe_parse () in
    match parse_result with
      None -> ()
    | Some ast ->
      safe_interpret_and_print ast;
      Format.print_flush ()
  done


(* let run_file filename =
   let fin = open_in filename in
   let lexbuf = Lexing.from_channel fin in
   let ast = Fbdk.Parser.main Fbdk.Lexer.token lexbuf in
   let result = Fbdk.Interpreter.eval ast in
   Format.printf "%a\n" Fbdk.Pp.pp_expr result;
   Format.print_flush () *)

type operation_usage = {name: string; description: string; example: string}
let operations = [
  {
    name = "NODE (creation)";
    description = "Create a node with key-value data pairs.";
    example = "NODE n1 = {name: \"Conner\", age: \"22\"};"
  };
  {
    name = "NODE (lookup)";
    description = "Get the details of an existing node.";
    example = "NODE n1;"
  };
  {
    name = "CREATE RELATION";
    description = "Create a new relation, which can be directed (default) or undirected (add keyword UNDIR). A directed relation can also be created in pairs.";
    example = "CREATE RELATION loves is_loved;\nCREATE RELATION visited;\nCREATE RELATION UNDIR roommates;"
  };
  {
    name = "RELATION";
    description = "Add an edge to an existing relation. Directed relations receive only two nodes at a time, while undirected can receive many.";
    example = "RELATION loves FOR FOR n1, n2;\nRELATION roommates FOR n1, n2, n3, n4;"
  };
  {
    name = "ATTR";
    description = "Get a specific attribute from a node. Keyword OF is optional.";
    example = "ATTR name n1;\nATTR name OF n1;"
  };
  {
    name = "SEARCH";
    description = "Get all nodes that meet a certain search criterion. Can take other expressions as argument by using parenthesis.";
    example = "SEARCH {name: \"Conner\"};\n SEARCH (NODE n1);"
  };
  {
    name = "WHO";
    description = "Get nodes that satisfy a certain relation for a node. Can be used to traverse relations recursively. Keyword BY is optional for readability. ";
    example = "WHO roommates n1;\nWHO loves n3;\nWHO is_loved BY n2 REC 2;"
  };
  {
    name = "SIZE";
    description = "Get size of a list of nodes or relations. Can take other expressions as argument by using parenthesis.";
    example = "SIZE (WHO roommates n1);\n SIZE (SHOW RELATIONS);"
  };
  {
    name = "SHOW NODES";
    description = "Get all the nodes in the database";
    example = "SHOW NODES;"
  };
  {
    name = "SHOW RELATIONS";
    description = "Get all the relations in the database";
    example = "SHOW RELATIONS;"
  };
  {
    name = "LOAD";
    description = "Load a stored database file (.broql).";
    example = "LOAD db.broql;"
  };
  {
    name = "SAVE";
    description = "Save the state of the database to a file (.broql).";
    example = "SAVE db.broql;"
  };
  {
    name = "BROQL or BROQLI";
    description = "Try it out!";
    example = "BROQL;\nBROQLI;"
  }
]

let operations_usage_string (operations: operation_usage list): string =
  Core.List.fold operations ~init:"" ~f:(fun accum op -> accum ^ op.name ^ ": " ^ op.description ^ "\nEx: " ^ op.example ^ "\n\n")

let usage_str =
  let basic_usage = "Usage: broqli.exe [--verbose]" in
  let operations_usage = operations_usage_string operations in
  basic_usage ^ "\n\nOPERATIONS:\n" ^ operations_usage ^ "COMMAND LINE OPTIONS:"

let main () = 
  let verbose = ref false in
  Arg.parse
    ([("--verbose",
       Arg.Set verbose,
       "Output debug information")
     ])
    (fun _ -> ())
    usage_str;
  toplevel_loop !verbose

let () = 
  main ()