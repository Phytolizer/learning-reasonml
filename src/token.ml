type kind =
  | TkIllegal
  | TkEof
  | TkIdent
  | TkInt
  | TkAssign
  | TkPlus
  | TkMinus
  | TkAsterisk
  | TkSlash
  | TkBang
  | TkLt
  | TkGt
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
  | TkMinus -> "-"
  | TkAsterisk -> "*"
  | TkSlash -> "/"
  | TkBang -> "!"
  | TkLt -> "<"
  | TkGt -> ">"
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

let cmp { kind = k1; literal = l1 } { kind = k2; literal = l2 } =
  if k1 = k2 then String.equal l1 l2 else false

let ( = ) = cmp

let lookup_ident id =
  match id with "fn" -> TkFunction | "let" -> TkLet | _ -> TkIdent
