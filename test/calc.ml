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
               Integer { value = 9};
               Integer { value = 0 };
               Integer { value = 9 };
             ]
             (calc "9 0 3!") );
         ( "copy1" >:: fun _ ->
           assert_equal
             [
               Integer { value = 1 };
               String { value = "(A)" };
               Integer { value = 1 };
             ]
             (calc "1(A)3!") );
         ( "copy - no effect if wrong value n" >:: fun _ ->
           assert_equal
             [
               String { value = "(A)" };
               Integer { value = 1 };
             ]
             (calc "1(A)4!") );
         ( "copy_bigger" >:: fun _ ->
           assert_equal
             [
               Integer { value = 1 };
               String { value = "(9~)" };
               String { value = "(8)" };
               Integer { value = 1 };
             ]
             (calc "1(8)(9~)4!") );
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
let example_from_pdf_test = 
  "pdf_example"
  >::: [
  ( "exam1" >:: fun _ ->
           assert_equal
             [ Integer { value = 8 } ]
             (calc "1(8)(9~)(4!4$_1+$@)@") );
  ]
let () =
  run_test_tt_main math_operations;
  run_test_tt_main registers_test;
  run_test_tt_main example_from_pdf_test; 
