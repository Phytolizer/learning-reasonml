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
  let open Token in
  match l.ch with
  | '\x00' -> (new_token TkEof l.ch, l)
  | '=' -> (new_token TkAssign l.ch, read_char l)
  | '+' -> (new_token TkPlus l.ch, read_char l)
  | '(' -> (new_token TkLParen l.ch, read_char l)
  | ')' -> (new_token TkRParen l.ch, read_char l)
  | '{' -> (new_token TkLBrace l.ch, read_char l)
  | '}' -> (new_token TkRBrace l.ch, read_char l)
  | ',' -> (new_token TkComma l.ch, read_char l)
  | ';' -> (new_token TkSemicolon l.ch, read_char l)
  | x -> (new_token TkIllegal x, read_char l)
