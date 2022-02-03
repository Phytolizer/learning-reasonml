let next_token _ =
  let input =
    {|
    let five = 5;
    let ten = 10;

    let add = fn(x, y) {
        x + y;
    };

    let result = add(five, ten);
    |}
  in
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
      T.create TkLet "let";
      T.create TkIdent "five";
      T.create TkAssign "=";
      T.create TkInt "5";
      T.create TkSemicolon ";";
      T.create TkLet "let";
      T.create TkIdent "ten";
      T.create TkAssign "=";
      T.create TkInt "10";
      T.create TkSemicolon ";";
      T.create TkLet "let";
      T.create TkIdent "add";
      T.create TkAssign "=";
      T.create TkFunction "fn";
      T.create TkLParen "(";
      T.create TkIdent "x";
      T.create TkComma ",";
      T.create TkIdent "y";
      T.create TkRParen ")";
      T.create TkLBrace "{";
      T.create TkIdent "x";
      T.create TkPlus "+";
      T.create TkIdent "y";
      T.create TkSemicolon ";";
      T.create TkRBrace "}";
      T.create TkSemicolon ";";
      T.create TkLet "let";
      T.create TkIdent "result";
      T.create TkAssign "=";
      T.create TkIdent "add";
      T.create TkLParen "(";
      T.create TkIdent "five";
      T.create TkComma ",";
      T.create TkIdent "ten";
      T.create TkRParen ")";
      T.create TkSemicolon ";";
      T.create TkEof "";
    ]
  in
  check_tokens expected tokens
