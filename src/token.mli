type kind
type t

val kind_name : kind -> string
val to_string : t -> string
val create : kind -> string -> t
