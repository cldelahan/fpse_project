let eval = Interpreter.eval

let parse s =
  let lexbuf = Lexing.from_string (s^";") in
  Broql_parser.main Broql_lexer.token lexbuf

(* let unparse e =
   Format.asprintf "%a" Fbdk.Pp.pp_expr e *)

let parse_eval s =
  Interpreter.eval (parse s)

(* let parse_eval_unparse s =
   unparse @@ Fbdk.Interpreter.eval (parse s) *)

(* let peu = parse_eval_unparse *)

(* let parse_eval_print s =
   Format.printf "==> %a\n" Fbdk.Pp.pp_expr 
    (Fbdk.Interpreter.eval @@ parse s) *)

(* let pp s = s |> parse |> unparse |> print_string |> print_newline;; *)
