open Execution

let rec read_input () =
  let input = read_line () in
  let output = Execution.calculate input in
  Stack.print_stack output;
  print_endline "";
  read_input ()

let () =
  let m = Registers.init_regs Registers.RegistersMap.empty in
  Registers.print_registers m;
  (* note register a contains the initial command (see Registers)*)
  let output = Execution.calculate (Registers.RegistersMap.find "a" m) in
  Stack.print_stack output;
  print_endline "";
  read_input ()
