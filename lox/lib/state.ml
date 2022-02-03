type t = { had_error : bool; had_runtime_error : bool }

let create : t = { had_error = false; had_runtime_error = false }
let had_error (state : t) : bool = state.had_error

let report (state : t) (line : int) (where : string) (msg : string) : t =
  Printf.eprintf "[line %d] Error%s: %s\n" line where msg;
  { state with had_error = true }

let error (state : t) (line : int) (msg : string) : t = report state line "" msg
