open Calc.Execution
open Calc.Registers
open Calc.Calc_stack

(* read input and execute the commands, rec function the calculator ends ones we stop the program*)
let rec read_input () =
  let input = read_line () in
  let output = calculate input in
  CustomStack.print_stack output;
  print_endline "";
  read_input ()

let () =
  set_value "a" (String { value = "(Welcome! Let's calculate stuff! Woohoo!)" });
  (* calculate median *)
  set_value "m" (String { value = "((#1+! #! > (#1+! #$) (#! #1-$) c@ 4! 4$ 4! 1- 4$  m@) () (4!_1+$@)@)" });
  set_value "t" (String { value = "((1-#2-m@#$1$t@)()(4!_1+$@)@)" });
  set_value "i" (String { value = "((#1+! #! > (#! #1-$) (#1+! #$) c@ 4! 4$ 4! 1- 4$  i@) () (4!_1+$@)@)" });
  set_value "g" (String { value = "((1-#2-i@#$1$g@)()(4!_1+$@)@)" });
  set_value "h" (String { value = "(2! 2% (2/g@1$y@)(2/1-g@1$#m@1$+2/y@) c@ )" });
  (* this is the median if you want to test use j@ in the 'console' and then input the array like this (1.5 1.8 1.0 1.2 2.5) *)
  set_value "j" (String { value = "('@ # h@)" });
  
  (* help function for the average*)
  (* this calculates the average (arithmetic mean) the main code is in register n *)
  set_value "v" (String { value = "(# 1 > (#$v@) () c@)"}); 
  set_value "n" (String { value = "((#3!-2+!4!4$+3!3$1-n@)(1$#1-/v@)(4!_1+$@)@)"}); 
  (* this is the mean *)
  set_value "q" (String { value = "('@ 0 #1- n@)"});

  (* find the variance*)
  set_value "e" (String { value = "((1-e@#$)(1$)(4!_1+$@)@)"}); 
  set_value "o" (String { value = "((#3!-2+!5!-2!*4!4$+3!3$1-o@)(1$2$#2-/#1-e@)(4!_1+$@)@)"}); 
  set_value "p" (String { value = "((#3!-2+!4!4$+3!3$1-p@)(1$#1-/0#2-o@)(4!_1+$@)@)"}); 
  (* this is the register for variance*)
  set_value "w" (String { value = "('@ 0 #1- p@)"}); 

  set_value "x" (String { value = "(9 7 8 #! (#-1)@! > (0) (1) (4!4$_1+$@)@)"});
  set_value "c" (String { value = "(4!4$_1+$@)"});

  set_value "z" (String { value = "((3! 3! >)@ (1$) (2$) c@)"}); (*compare  2 value and remove lower one*)
  set_value "x" (String { value = "((3! 3! >)@ () (3! 3$) c@)"}); (*order 2 values*)
  set_value "x" (String { value = "((3! 3! >)@ () (3! 3$) c@)"}); (*order 2 values*)
  set_value "y" (String { value = "(# 1 > (z@ y@) () c@)"}); (*get max value of list*)

  (*print register output `a`, which contains initial program*)
  let output = get_register "a" in
  CustomStack.print_stack_element output;
  print_endline "";
  read_input ()
