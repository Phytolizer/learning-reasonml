type t = { kind : TokenKind.t; lexeme : string; literal : Object.t; line : int }

let create (kind : TokenKind.t) (lexeme : string) (literal : Object.t)
    (line : int) : t =
  { kind; lexeme; literal; line }

let print (tok : t) : string =
  Printf.sprintf "%d %s %s" tok.line tok.lexeme (Object.print tok.literal)
