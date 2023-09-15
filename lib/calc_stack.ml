type stack_element =
  | Integer of { value : int }
  | String of { value : string }
  | Float of { value : float }

(* Signature for the custom stack *)
module type CALC_STACK = sig
  type a = stack_element

  val empty : a list
  val print_stack : a list -> unit
  val print_stack_element : a -> unit
  val is_empty : a list -> bool
  val push : a -> a list -> a list
  val pop : a list -> a * a list
  val remove_nth : int -> a list -> a list
  val get_nth : int -> a list -> a
end

(* Stack module, we need a custom one in order to implement also the other operations*)
(* we do not have only pop and push*)
module CustomStack : CALC_STACK = struct
  type a = stack_element

  let empty = []

  let print_stack_element = function
    | Integer p -> print_int p.value
    | String s -> print_string s.value
    | Float f -> print_float f.value

  let print_stack stack = List.iter (fun elem -> print_stack_element elem; print_char ' ') (List.rev stack)
  let is_empty stack = match stack with [] -> true | _ -> false
  let push x stack = x :: stack

  let pop stack =
    match stack with [] -> failwith "Stack is empty" | x :: xs -> (x, xs)

  let remove_nth n stack =
    if n < 0 then invalid_arg "remove_nth: negative index"
    else
      let rec aux n acc = function
        | [] -> List.rev_append acc []
        | _ :: tl when n = 0 -> List.rev_append acc tl
        | hd :: tl -> aux (n - 1) (hd :: acc) tl
      in
      aux n [] stack

  let rec get_nth n stack =
    match (stack, n) with
    | [], _ -> raise (Failure "get_nth")
    | _, n when n < 0 -> raise (Invalid_argument "get_nth")
    | x :: _, 0 -> x
    | _ :: xs, n -> get_nth (n - 1) xs
end
