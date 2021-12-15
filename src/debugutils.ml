let eval = Eval.eval

let list_of_lexbuf (lexbuf: Lexing.lexbuf): Parser.token list =
  let rec helper lexbuf cur_list =
    match Lexer.token lexbuf with
    | EOEX as t -> t :: cur_list
    | _ as t -> helper lexbuf (t :: cur_list)
  in List.rev @@ helper lexbuf []

let debug_lex s =
  let lexbuf = Lexing.from_string (s^";") in
  list_of_lexbuf lexbuf

let parse s =
  let lexbuf = Lexing.from_string (s^";") in
  Parser.main Lexer.token lexbuf

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
