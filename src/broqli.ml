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

let toplevel_loop () =
  (* Prints exceptions and associated stack traces *)
  let print_exception ex =
    Format.printf "Exception: %s\n" (Printexc.to_string ex);
    Format.print_flush ();
    Printexc.print_backtrace stdout;
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
    Format.printf "Here 1\n";
    let parse_result = safe_parse () in
    Format.printf "Here 2\n";
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


let () = 
  toplevel_loop ()