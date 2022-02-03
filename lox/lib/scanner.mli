type t

val create : string -> State.t -> t
val scan : t -> Token.t list * t
val state : t -> State.t
