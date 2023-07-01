(* dummy command for now, but here the command should be parced and then executed*)
let proces_command command = print_endline ("proccessing: " ^ command)

(* this reads the commands from the std in an then process them (after the initial command in register a is executed)
 *)
let rec read_input () =
  let input = read_line () in
  proces_command input;
  read_input ()

let () =
  let m = Registers.init_regs Registers.RegistersMap.empty in
  Registers.print_registers m;
  (* note register a contains the initial command (see Registers)*)
  proces_command (Registers.RegistersMap.find "a" m);
  read_input ()
