
(*
- 26 registers
- named by lowercase letters (a to z)
- predefined values when switching on the calculator
- a -> contains the inital content of the command stream
*)

type stack_element =
  | Integer of { value : int }
  | String of { value : string }
  | Float of { value : float }

module RegistersMap = Map.Make (String)
type register_map = stack_element RegistersMap.t
(*let global_registers_map = ref RegistersMap.empty*)
let global_registers_map: register_map ref  = ref RegistersMap.empty


(*init all registers as empty strings*)
(*idk if that is the way it should be done in ocaml*)
let init_register_empty key = 
  global_registers_map := RegistersMap.add (String.make 1 key) (String { value = "" }) !global_registers_map

let init_all = 
  String.iter init_register_empty "abcdefghijklmnobqrstuvwxyz"

let set_value key value = 
  global_registers_map := RegistersMap.add key value !global_registers_map

let get_register key = RegistersMap.find key !global_registers_map

(*(*set the value of a register*)

let insert (key: string) (value: 'a) = 
  RegistersMap.add key value !global_registers_map

(*get the value of a register*)

(*printing register values*)
let print_reg key value = print_endline ("reg: " ^ key ^ " value: " ^ value)
let print_registers () = RegistersMap.iter print_reg !global_registers_map
let print_register key = print_reg key (RegistersMap.find key !global_registers_map)
*)



module MyMap = Map.Make(struct
  type t = string
  let compare = compare
end)

type my_map =  stack_element MyMap.t

let create_map () : my_map = MyMap.empty

let insert (key: string) (value: stack_element) (map: my_map) : my_map =
  MyMap.add key value map

let find (key: string) (map: my_map) : stack_element option =
  try Some (MyMap.find key map)
  with Not_found -> None


