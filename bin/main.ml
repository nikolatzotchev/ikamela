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
  set_value "i" (String { value = "((#1+! #! > (#! #1-$) (#1+! #$) c@ 4! 4$ 4! 1- 4$  i@) () (4!_1+$@)@)" });
  set_value "g" (String { value = "((1-#2-i@#$1$g@)(1$y@)(4!_1+$@)@)" });
  set_value "h" (String { value = "(2! 2% (2/g@)() c@ )" });
  set_value "j" (String { value = "('@ # h@)" });
  
  (* help function for the average*)
  set_value "n" (String { value = "(('+3!3$1-3!3$n@)(2$3!3$/)(5!_1+$@)@)" });
  (* this calculates the average (arithmetic mean) the main code is in register n *)
  set_value "q" (String { value = "(' 2! 0 n@)"});
  
  set_value "x" (String { value = "(9 7 8 #! (#-1)@! > (0) (1) (4!4$_1+$@)@)"});
  set_value "n" (String { value = "(no)" });
  set_value "b" (String { value = "(3! 3! >)" }); (*is the first bigger than the second*)
  set_value "c" (String { value = "(4!4$_1+$@)"});
  set_value "d" (String { value = "(9 7 8)"});
  set_value "d" (String { value = "(9 8 7)"});
  set_value "x" (String { value = "(9 8 7 (3! 3! >)@ (1$) (2$) c@)"});

  set_value "z" (String { value = "((3! 3! >)@ (1$) (2$) c@)"}); (*compare  2 value and remove lower one*)
  set_value "x" (String { value = "((3! 3! >)@ () (3! 3$) c@)"}); (*order 2 values*)
  set_value "x" (String { value = "((3! 3! >)@ () (3! 3$) c@)"}); (*order 2 values*)
  set_value "y" (String { value = "(# 1 > (z@ y@) () c@)"}); (*get max value of list*)
  (* ------------------ this is a try at the mean*)
  (* set_value "l" (String { value = "((' 1$2!2$1-2!2$l@)()(5!_1+$@)@)"});  *)
  (* set_value "l" (String { value = "((' 4!4$4!4$ 1-l@)(1$1+2/!)(4!_1+$@)@)"});  *)
  (* set_value "c" (String { value = "(l@)"}); *)
  (* set_value "m" (String { value = "(' 2! 2! 1+ 2 %_ (c@)(9~)(4!4$_1+$@)@)"});  *)
  (*print register output `a`, which contains initial program*)
  let output = get_register "a" in
  CustomStack.print_stack_element output;
  print_endline "";
  read_input ()
