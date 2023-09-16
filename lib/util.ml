open Calc_stack

(* this module contains utility functions needed in our program *)

let length_of_stack_element elem =
  match elem with
  | Integer _ -> 1 (* Integer takes up 1 element *)
  | String str -> String.length str.value - 2 (*because of `(` and `)` *)
  | Float _ -> 1 (* Float takes up 1 element *)

(* this is needed for the ' command *)
let check_well_formed_string str =
  (* this stack is different than the stack for the calculator*)
  let local_stack = Stack.create () in
  let rec check_string_parentessens vec =
    match vec () with
    | Seq.Nil -> if Stack.length local_stack == 0 then true else false
    | Seq.Cons (x, xs) -> (
        match x with
        | '(' ->
            Stack.push 1 local_stack;
            check_string_parentessens xs
        | ')' ->
            let _ = Stack.pop local_stack in
            check_string_parentessens xs
        | _ -> check_string_parentessens xs)
  in
  check_string_parentessens (String.to_seq str)

(* Converts a char to a stack element with an integer value*)
let char_to_stack_int char =
  Integer { value = int_of_char char - int_of_char '0' }

(* Converts a char into an integer*)
let char_to_int char = int_of_char char - int_of_char '0'

let compare_float v u =
  let epsilon = 0.00000000000001 in
  let diff = v -. u in
  let abs_diff = abs_float diff in
  if abs_float v > 1.0 || abs_float u > 1.0 then
    if abs_diff >= epsilon *. max (abs_float v) (abs_float u) then if diff > 0.0 then 1 else -1 else 0
  else if abs_diff <= epsilon then 0 else 1

let compare_values operand1 operand2 =
  match (operand1, operand2) with
  | Integer v1, Integer v2 -> compare v1.value v2.value
  | Float v1, Float v2 -> compare_float v1.value v2.value
  (*| Float v1, Integer v2 -> compare_float v1.value (float_of_int v2.value)*)
  | Float v1, Integer v2 -> (Printf.printf "%d %f\n" v2.value v1.value); 1
  | Integer v1, Float v2 -> compare_float (float_of_int v1.value) v2.value
  | String v1, String v2 -> compare v1.value v2.value
  | String _, _ -> -1
  | _, String _ -> 1

