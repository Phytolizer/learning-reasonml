let rec run_prompt () =
  print_string "> ";
  let line = read_line () in
  print_endline ("You typed: " ^ line);
  run_prompt ()

let read_file path =
  let ic = open_in path in
  let buf = Buffer.create 4096 in
  let rec loop () =
    let line = input_line ic in
    Buffer.add_string buf line;
    Buffer.add_char buf '\n';
    loop ()
  in
  try loop () with End_of_file -> Buffer.contents buf

let runFile path =
  let content = read_file path in
  print_endline ("File contents: " ^ content)

let () =
  match Array.length Sys.argv with
  | 1 -> run_prompt ()
  | 2 -> runFile Sys.argv.(1)
  | _ ->
      Printf.eprintf "usage: %s [script]\n" Sys.argv.(0);
      exit 64
