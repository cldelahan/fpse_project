let eval = Eval.eval

let parse s =
  let lexbuf = Lexing.from_string (s^";") in
  Broql_parser.main Broql_lexer.token lexbuf

let unparse e =
  Format.asprintf "%a" Pp.pp_expr e

let parse_eval s =
  Eval.eval (parse s)

let parse_eval_unparse s =
  unparse @@ Eval.eval (parse s)

let peu = parse_eval_unparse

let parse_eval_print s =
  Format.printf "==> %a\n" Pp.pp_expr 
    (Eval.eval @@ parse s)

let pp s = s |> parse |> unparse |> print_string |> print_newline;;
