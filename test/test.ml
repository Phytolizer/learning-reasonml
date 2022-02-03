open OUnit2
module Lexer = Monkey.Lexer
module Token = Monkey.Token

let tests =
  "lexer tests"
  >::: [
         ( "next token" >:: fun _ ->
           let input = "=+(){},;" in
           let lexer = Lexer.create input in
           let tokens = Lexer.all_tokens lexer in
           let open Token in
           let rec check_tokens expected actual =
             match expected with
             | [] -> ( match actual with [] -> true | _ -> false)
             | x :: xs -> (
                 match actual with
                 | y :: ys ->
                     assert_equal ~cmp:Token.cmp ~printer:Token.to_string x y;
                     check_tokens xs ys
                 | [] -> false)
           in
           let expected =
             [
               Token.create TkAssign "=";
               Token.create TkPlus "+";
               Token.create TkLParen "(";
               Token.create TkRParen ")";
               Token.create TkLBrace "{";
               Token.create TkRBrace "}";
               Token.create TkComma ",";
               Token.create TkSemicolon ";";
               Token.create TkEof "";
             ]
           in
           assert (check_tokens expected tokens) );
       ]

let _ = run_test_tt_main tests
