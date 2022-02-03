let maybe_read_line : string option =
  try
    let result = read_line () in
    Some result
  with End_of_file -> None

let rec run_prompt () : unit =
  print_string "> ";
  let line = maybe_read_line in
  match line with
  | None -> ()
  | Some line ->
      let state = Lox.State.create in
      ignore (Lox.run state line);
      run_prompt ()

let read_file (path : string) : string =
  let ic = open_in path in
  let buf = Buffer.create 4096 in
  let rec loop () =
    let line = input_line ic in
    Buffer.add_string buf line;
    Buffer.add_char buf '\n';
    loop ()
  in
  try loop () with End_of_file -> Buffer.contents buf

let run_file path =
  let content = read_file path in
  let state = Lox.State.create in
  let state = Lox.run state content in
  if Lox.State.had_error state then exit 65

let () =
  match Array.length Sys.argv with
  | 1 -> run_prompt ()
  | 2 -> run_file Sys.argv.(1)
  | _ ->
      Printf.eprintf "usage: %s [script]\n" Sys.argv.(0);
      exit 64
