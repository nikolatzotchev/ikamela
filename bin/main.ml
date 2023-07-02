let rec read_input () =
  let input = read_line () in
  print_int (Execution.calculate input);
  print_endline "";
  read_input ()

let () =
  let m = Registers.init_regs Registers.RegistersMap.empty in
  Registers.print_registers m;
  (* note register a contains the initial command (see Registers)*)
  print_int (Execution.calculate (Registers.RegistersMap.find "a" m));
  print_endline "";
  read_input ()
