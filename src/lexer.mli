type lexer

val create : string -> lexer
val next_token : lexer -> Token.t * lexer
val all_tokens : lexer -> Token.t list
