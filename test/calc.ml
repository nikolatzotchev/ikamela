open OUnit2
open Calc.Registers

let calc expression = Calc.Execution.calculate expression

let math_operations =
  "basic math operation"
  >::: [
         ( "sum" >:: fun _ ->
           assert_equal [ Integer { value = 2 } ] (calc "1 1+") );
         ( "mul" >:: fun _ ->
           assert_equal [ Integer { value = 15 } ] (calc "3 5*") );
         ( "copy" >:: fun _ ->
           assert_equal
             [
               Integer { value = 9 };
               Integer { value = 0 };
               Integer { value = 9 };
             ]
             (calc "9 0 0!") );
         ( "null_check_empty_string" >:: fun _ ->
           assert_equal [ Integer { value = 1 } ] (calc "()_") );
         ( "null_check_string" >:: fun _ ->
           assert_equal [ Integer { value = 0 } ] (calc "(asd)_") );
         ( "null_check_int_zero" >:: fun _ ->
           assert_equal [ Integer { value = 1 } ] (calc "0_") );
         ( "null_check_int_non_zero" >:: fun _ ->
           assert_equal [ Integer { value = 0 } ] (calc "1_") );
         ( "null_check_float_zero" >:: fun _ ->
           assert_equal [ Integer { value = 1 } ] (calc "0.000000000000000001_")
         );
         ( "null_check_float_zero" >:: fun _ ->
           assert_equal [ Integer { value = 0 } ] (calc "0.1_") );
         ( "negation_int" >:: fun _ ->
           assert_equal [ Integer { value = -1 } ] (calc "1~") );
         ( "negation_float" >:: fun _ ->
           assert_equal [ Float { value = -0.1 } ] (calc "0.1~") );
         ( "negation_string" >:: fun _ ->
           assert_equal [ String { value = "()" } ] (calc "a~") );
       ]

let registers_test =
  "registers"
  >::: [
         ( "reg1" >:: fun _ ->
           assert_equal
             [ Calc.Registers.Integer { value = 2 } ]
             (calc "(1 1+)Aa@") );
       ]

let () =
  run_test_tt_main math_operations;
  run_test_tt_main registers_test
