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

let is_letter (ch : char) =
  (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || ch == '_'

let is_digit (ch : char) = ch >= '0' && ch <= '9'

let rec read_identifier (l : t) =
  let ch = l.ch in
  if is_letter ch then
    let rest, l = read_identifier (read_char l) in
    (ch :: rest, l)
  else ([], l)

let rec read_number (l : t) =
  let ch = l.ch in
  if is_digit ch then
    let rest, l = read_number (read_char l) in
    (ch :: rest, l)
  else ([], l)

let rec skip_whitespace (l : t) =
  let ch = l.ch in
  if List.exists (fun x -> x = ch) [ ' '; '\t'; '\n'; '\r' ] then
    let l = read_char l in
    skip_whitespace l
  else l

let implode (chars : char List.t) = String.of_seq (List.to_seq chars)

let next_token (l : t) =
  let l = skip_whitespace l in
  let open Token in
  match l.ch with
  | '\x00' -> (Token.create TkEof "", l)
  | '=' -> (new_token TkAssign l.ch, read_char l)
  | '+' -> (new_token TkPlus l.ch, read_char l)
  | '-' -> (new_token TkMinus l.ch, read_char l)
  | '*' -> (new_token TkAsterisk l.ch, read_char l)
  | '/' -> (new_token TkSlash l.ch, read_char l)
  | '!' -> (new_token TkBang l.ch, read_char l)
  | '<' -> (new_token TkLt l.ch, read_char l)
  | '>' -> (new_token TkGt l.ch, read_char l)
  | '(' -> (new_token TkLParen l.ch, read_char l)
  | ')' -> (new_token TkRParen l.ch, read_char l)
  | '{' -> (new_token TkLBrace l.ch, read_char l)
  | '}' -> (new_token TkRBrace l.ch, read_char l)
  | ',' -> (new_token TkComma l.ch, read_char l)
  | ';' -> (new_token TkSemicolon l.ch, read_char l)
  | x when is_letter x ->
      let s, l = read_identifier l in
      let s = implode s in
      (Token.create (Token.lookup_ident s) s, l)
  | x when is_digit x ->
      let n, l = read_number l in
      (Token.create TkInt (implode n), l)
  | x -> (new_token TkIllegal x, read_char l)

let rec all_tokens (l : t) =
  let t, l' = next_token l in
  if t.kind = TkEof then [ t ] else t :: all_tokens l'
