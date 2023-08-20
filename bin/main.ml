open Execution

let rec read_input () =
  let input = read_line () in
  let output = Execution.calculate input in
  Stack.print_stack output;
  print_endline "";
  read_input ()

let () =
  Registers.set_value "a" "15 2 3 4+*-";
  (*Registers.set_value "b"  "15 2 3 4+*-";*)
  (*Registers.set_value "c" "2((2 2*)@*)@";*)
  (*Registers.set_value "d" "(2*)\\2";*)
  (*Registers.set_value "e" "2 2*";*)

  (*Registers.print_registers();*)
  (*note register 'a' contains the initial command (see Registers)*)
  Registers.print_register "a";
  let output = Execution.calculate (Registers.get_register "a") in
  Stack.print_stack output;
  print_endline "";
  read_input ()
