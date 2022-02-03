type t = Integer of int | String of string | Null

let integer (i : int) : t = Integer i
let string (s : string) : t = String s
let null : t = Null

let print (obj : t) : string =
  match obj with Integer i -> string_of_int i | String s -> s | Null -> "null"
