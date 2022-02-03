type t = { input : char List.t; position : int; read_position : int; ch : char }

let peek_char (l : t) =
  if l.read_position < List.length l.input then List.nth l.input l.read_position
  else '\x00'

let read_char (l : t) =
  let c = peek_char l in
  {
    l with
    position = l.read_position;
    read_position = l.read_position + 1;
    ch = c;
  }

let explode (s : string) = List.init (String.length s) (String.get s)

let create (input : string) =
  let l =
    { input = explode input; position = 0; read_position = 0; ch = '\x00' }
  in
  read_char l

let new_token (t : Token.kind) (c : char) = Token.create t (String.make 1 c)

let next_token (l : t) =
  match l.ch with
  | '\x00' -> (new_token Token.TkEof l.ch, l)
  | '=' -> (Token.TkAssign, read_char l)
  | '+' -> (Token.TkPlus, read_char l)
  | '(' -> (Token.TkLParen, read_char l)
  | ')' -> (Token.TkRParen, read_char l)
  | '{' -> (Token.TkLBrace, read_char l)
  | '}' -> (Token.TkRBrace, read_char l)
  | ',' -> (Token.TkComma, read_char l)
  | ';' -> (Token.TkSemicolon, read_char l)
  | x -> (Token.TkIllegal, read_char l)
