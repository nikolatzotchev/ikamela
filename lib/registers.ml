open Calc_stack
(*
- 26 registers
- named by lowercase letters (a to z)
- predefined values when switching on the calculator (see main)
- a -> contains the inital content of the command stream
*)

(*init global registers map*)
module RegistersMap = Map.Make (String)

type register_map = stack_element RegistersMap.t

let global_registers_map : register_map ref = ref RegistersMap.empty

(*init all registers as empty strings*)
let init_register_empty key =
  global_registers_map :=
    RegistersMap.add (String.make 1 key)
      (String { value = "" })
      !global_registers_map

let init_all = String.iter init_register_empty "abcdefghijklmnobqrstuvwxyz"

(*set the value of a specific register*)
let set_value key value =
  global_registers_map := RegistersMap.add key value !global_registers_map

(*geth the value of a specific register*)
let get_register key = RegistersMap.find key !global_registers_map
