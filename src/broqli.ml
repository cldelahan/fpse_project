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
    name = "NODE";
    description = "Create a node with key-value data pairs.";
    example = "NODE n1 = {name: \"Conner\", age: \"22\"};"
  };
  {
    name = "RELATION";
    description = "Add a relation for certain nodes. Relations can be undirected or directed.";
    example = "RELATION roommates FOR n1, n2, n3;\nRELATION manages DIR FOR n1, n2, n3;"
  };
  {
    name = "ATTR";
    description = "Get a specific attribute from a node.";
    example = "ATTR name n1;"
  };
  {
    name = "SEARCH";
    description = "Get all nodes that meet a certain search criterion.";
    example = "SEARCH {name: \"Conner\"};"
  };
  {
    name = "WHO";
    description = "Get nodes that satisfy a certain relation for a node.";
    example = "WHO roommates FOR n1;"
  };
  {
    name = "SIZE";
    description = "Get size of a list of nodes.";
    example = "SIZE WHO roommates FOR n1;"
  };
  {
    name = "LOAD";
    description = "Load a stored database file (.broql).";
    example = "LOAD db.broql"
  };
  {
    name = "SAVE";
    description = "Save the state of the database to a file (.broql).";
    example = "SAVE db.broql"
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