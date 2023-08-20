
(*
- 26 registers
- named by lowercase letters (a to z)
- predefined values when switching on the calculator
- a -> contains the inital content of the command stream
*)
module RegistersMap = Map.Make (String)
let global_registers_map: string RegistersMap.t ref = ref RegistersMap.empty

(*init all registers as empty strings*)
(*idk if that is the way it should be done in ocaml*)
let init_register_empty key = global_registers_map := RegistersMap.add (String.make 1 key) "" !global_registers_map
let init_all = String.iter init_register_empty "abcdefghijklmnobqrstuvwxyz"

(*set the value of a register*)
let set_value key value = global_registers_map := RegistersMap.add key value !global_registers_map

(*get the value of a register*)
let get_register key = RegistersMap.find key !global_registers_map

(*printing register values*)
let print_reg key value = print_endline ("reg: " ^ key ^ " value: " ^ value)
let print_registers () = RegistersMap.iter print_reg !global_registers_map
let print_register key = print_reg key (RegistersMap.find key !global_registers_map)

