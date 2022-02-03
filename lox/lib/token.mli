type t

val create : TokenKind.t -> string -> Object.t -> int -> t
val print : t -> string
