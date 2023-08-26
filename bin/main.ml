open Execution
open Registers

let rec read_input () =
  let input = read_line () in
  let output = Execution.calculate input in
  Stack.print_stack output;
  print_endline "";
  read_input ()

let () =
  Registers.set_value "a" (String { value = "Welcome! Let's calculate stuff! Woohoo!" });
  Registers.set_value "b" (Integer { value = 1 });
  Registers.set_value "c" (Float { value = 3.14 });
  Registers.set_value "d" (String { value = "15 2 3 4+*-" });
  Registers.set_value "e" (String { value = "15 2+" });
  Registers.set_value "e" (String { value = "15 2+" });
  Registers.set_value "f" (String { value = "15" });
  Registers.set_value "g" (String { value = "15 1*" });
  Registers.set_value "h" (String { value = "15 2*" });
  Registers.set_value "i" (String { value = "15 3*" });
  (*Registers.set_value "b"  "15 2 3 4+*-";*)
  (*Registers.set_value "c" "2((2 2*)@*)@";*)
  (*Registers.set_value "d" "(2*)\\2";*)
  (*Registers.set_value "e" "2 2*";*)

  (*Registers.print_registers();*)
  (*note register 'a' contains the initial command (see Registers)*)
  
  let output = Registers.get_register "a" in
  print_stack_element output;
  print_endline "";
  read_input ()
