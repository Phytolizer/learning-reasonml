open OUnit2
module Lexer = Monkey.Lexer
module Token = Monkey.Token

let tests =
  "lexer tests"
  >::: [
         ( "next token" >:: LexerTest.next_token );
       ]

let _ = run_test_tt_main tests
