open OUnit2
open Monkey.Token

let tests =
  "test suite"
  >::: [
         ( "mytest" >:: fun test_ctxt ->
           assert_equal ~msg:"int value" ~printer:string_of_int 0 1 );
       ]

let _ = run_test_tt_main tests
