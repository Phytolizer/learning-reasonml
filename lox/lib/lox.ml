module State = State

let rec print_tokens (tokens : Token.t list) : unit =
  match tokens with
  | [] -> ()
  | tok :: toks ->
      print_endline (Token.print tok);
      print_tokens toks

let run (state : State.t) (source : string) : State.t =
  let scanner = Scanner.create source state in
  let tokens, scanner = Scanner.scan scanner in
  print_tokens tokens;
  Scanner.state scanner
