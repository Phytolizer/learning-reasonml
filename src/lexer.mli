type t

val create : string -> t
(** Initialize a new lexer. *)

val next_token : t -> Token.t * t
(** Iterate one step forward in the lexing process. *)

val all_tokens : t -> Token.t list
(** Collect all tokens into a list. This is equivalent
    to calling next_token repeatedly. *)
