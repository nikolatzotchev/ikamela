module RegistersMap = Map.Make (String)

let init_regs m = RegistersMap.add "a" "15 2 3 4+*-" m

let print_registers m =
  let print_reg key value =
    print_endline ("reg: " ^ key ^ " value: " ^ value)
  in
  RegistersMap.iter print_reg m
