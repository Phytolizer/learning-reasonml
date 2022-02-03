type t = {
  state : State.t;
  source : string;
  start : int;
  current : int;
  line : int;
}

let create (source : string) (state : State.t) : t =
  { state; source; start = 0; current = 0; line = 1 }

let is_at_end (s : t) : bool = s.current >= String.length s.source

let advance (s : t) : char * t =
  (s.source.[s.current], { s with current = s.current + 1 })

let add_token (s : t) (kind : TokenKind.t) (literal : Object.t) : Token.t =
  let text = String.sub s.source s.start s.current in
  Token.create kind text literal s.line

let scan_token (s : t) : Token.t option * t =
  let c, s = advance s in
  let n = Object.null in
  match c with
  | '(' -> (Some (add_token s TokenKind.LeftParen n), s)
  | ')' -> (Some (add_token s TokenKind.RightParen n), s)
  | '{' -> (Some (add_token s TokenKind.LeftBrace n), s)
  | '}' -> (Some (add_token s TokenKind.RightBrace n), s)
  | ',' -> (Some (add_token s TokenKind.Comma n), s)
  | '.' -> (Some (add_token s TokenKind.Dot n), s)
  | '-' -> (Some (add_token s TokenKind.Minus n), s)
  | '+' -> (Some (add_token s TokenKind.Plus n), s)
  | ';' -> (Some (add_token s TokenKind.Semicolon n), s)
  | '*' -> (Some (add_token s TokenKind.Star n), s)
  | _ ->
      let state = State.error s.state s.line "Unexpected character." in
      (None, { s with state })

let rec scan (s : t) : Token.t list * t =
  if is_at_end s then ([ Token.create TokenKind.Eof "" Object.null s.line ], s)
  else
    let s = { s with start = s.current } in
    let token, s = scan_token s in
    match token with
    | None -> scan s
    | Some token -> ( match scan s with tokens, s -> (token :: tokens, s))

let state (s : t) : State.t = s.state
