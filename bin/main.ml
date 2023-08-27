open Calc.Execution
open Calc.Registers

let rec read_input () =
  let input = read_line () in
  let output = calculate input in
  Stack.print_stack output;
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
  (*stack should be 9000090,*)
  (*TODO: should stack begin to count at 0?*)
  set_value "k" (String { value = "(0 9 0 0 0 0 1!)" });

  (*print register output `a`, which contains initial program*)
  let output = get_register "a" in
  print_stack_element output;
  print_endline "";
  read_input ()
