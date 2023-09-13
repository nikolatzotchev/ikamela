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
  set_value "a" (String { value = "Welcome! Let's calculate stuff! Woohoo!" });
  set_value "b" (Integer { value = 21 });
  set_value "c" (Float { value = 3.14 });

  (*registers, user register char  `c` like this to evaluate: `c@` *)
  set_value "d" (String { value = "(15 2 3 4+*-)" });
  set_value "e" (String { value = "(15 2+)" });
  set_value "f" (String { value = "(15)" });
  set_value "g" (String { value = "(15 1*)" });
  set_value "h" (String { value = "(15 2*)" });
  set_value "i" (String { value = "(15 3*)" });
  set_value "j" (String { value = "(9 2+)" });
  set_value "k" (String { value = "(0 9 0 0 0 0 1!)" });
  set_value "n" (String { value = "(('+3!3$1-3!3$n@)(2$3!3$/)(5!_1+$@)@)" });
  (* this calculates the average (arithmetic mean) the main code is in register n *)
  set_value "q" (String { value = "(' 2! 0 n@)"});
  (* ------------------ this is a try at the mean*)
  (* set_value "l" (String { value = "((' 1$2!2$1-2!2$l@)()(5!_1+$@)@)"});  *)
  (* set_value "l" (String { value = "((' 4!4$4!4$ 1-l@)(1$1+2/!)(4!_1+$@)@)"});  *)
  (* set_value "c" (String { value = "(l@)"}); *)
  (* set_value "m" (String { value = "(' 2! 2! 1+ 2 %_ (c@)(9~)(4!4$_1+$@)@)"});  *)
  (**)
  (*print register output `a`, which contains initial program*)
  let output = get_register "a" in
  CustomStack.print_stack_element output;
  print_endline "";
  read_input ()
