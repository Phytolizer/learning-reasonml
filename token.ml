open Printf

type token_kind =
  | TkIllegal
  | TkEof
  | TkIdent
  | TkInt
  | TkAssign
  | TkPlus
  | TkLParen
  | TkRParen
  | TkLBrace
  | TkRBrace
  | TkFunction
  | TkLet

let token_kind_name kind =
  match kind with
  | TkIllegal -> "ILLEGAL"
  | TkEof -> "EOF"
  | TkIdent -> "IDENT"
  | TkInt -> "INT"
  | TkAssign -> "="
  | TkPlus -> "+"
  | TkLParen -> "("
  | TkRParen -> ")"
  | TkLBrace -> "{"
  | TkRBrace -> "}"
  | TkFunction -> "FUNCTION"
  | TkLet -> "LET"

type token = { kind : token_kind; literal : string }

let token_to_string { kind; literal } =
  sprintf "{Type:%s Literal:%s}" (token_kind_name kind) literal
