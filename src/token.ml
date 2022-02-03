type kind =
  | TkIllegal
  | TkEof
  | TkIdent
  | TkInt
  | TkAssign
  | TkPlus
  | TkComma
  | TkSemicolon
  | TkLParen
  | TkRParen
  | TkLBrace
  | TkRBrace
  | TkFunction
  | TkLet

let kind_name kind =
  match kind with
  | TkIllegal -> "ILLEGAL"
  | TkEof -> "EOF"
  | TkIdent -> "IDENT"
  | TkInt -> "INT"
  | TkAssign -> "="
  | TkPlus -> "+"
  | TkComma -> ","
  | TkSemicolon -> ";"
  | TkLParen -> "("
  | TkRParen -> ")"
  | TkLBrace -> "{"
  | TkRBrace -> "}"
  | TkFunction -> "FUNCTION"
  | TkLet -> "LET"

type t = { kind : kind; literal : string }

let to_string { kind; literal } =
  Printf.sprintf "{Type:%s Literal:%s}" (kind_name kind) literal

let create kind literal = { kind; literal }
