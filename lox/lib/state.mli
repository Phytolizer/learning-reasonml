type t

val create : t
val had_error : t -> bool
val error : t -> int -> string -> t
