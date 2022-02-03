let next_token _ =
  let input = "=+(){},;" in
  let lexer = Monkey.Lexer.create input in
  let tokens = Monkey.Lexer.all_tokens lexer in
  let open Monkey.Token in
  let rec check_tokens expected actual =
    let open OUnit2 in
    match expected with
    | [] -> (
        match actual with
        | [] -> ()
        | _ -> assert_failure "actual token list too long.")
    | x :: xs -> (
        match actual with
        | y :: ys ->
            assert_equal ~printer:Monkey.Token.to_string x y;
            check_tokens xs ys
        | [] -> assert_failure "actual token list too short.")
  in
  let module T = Monkey.Token in
  let expected =
    [
      T.create TkAssign "=";
      T.create TkPlus "+";
      T.create TkLParen "(";
      T.create TkRParen ")";
      T.create TkLBrace "{";
      T.create TkRBrace "}";
      T.create TkComma ",";
      T.create TkSemicolon ";";
      T.create TkEof "";
    ]
  in
  check_tokens expected tokens
