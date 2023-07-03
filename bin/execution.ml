(* Stack module, we need a custom one in order to implement also the other operations*)
(* we do not have only pop and push*)
module Stack = struct
  type 'a t = 'a list

  let empty = []
  let is_empty stack = match stack with [] -> true | _ -> false
  let push x stack = x :: stack

  let pop stack =
    match stack with [] -> failwith "Stack is empty" | x :: xs -> (x, xs)
end

(* Calculator function *)
(* right now only basic operation on integers are supported*)
(* and we do not look at the operation mode*)
let calculate expression =
  let stack = ref Stack.empty and operation_mode = ref 0 in

  let apply_operator operator =
    let operand2, stack' = Stack.pop !stack in
    let operand1, stack'' = Stack.pop stack' in
    match operator with
    | '+' -> stack := Stack.push (operand1 + operand2) stack''
    | '-' -> stack := Stack.push (operand1 - operand2) stack''
    | '*' -> stack := Stack.push (operand1 * operand2) stack''
    | '/' -> stack := Stack.push (operand1 / operand2) stack''
    | _ -> failwith "Invalid operator"
  in

  let process_token token =
    match token with
    | '+' | '-' | '*' | '/' -> operation_mode := 0; apply_operator token
    | '0' .. '9' ->
        (match !operation_mode with
        | 0 -> stack := Stack.push (int_of_char token - int_of_char '0') !stack;  operation_mode := -1
        | _ when !operation_mode < 0 ->
            let number, stack' = Stack.pop !stack in
              stack := Stack.push ((int_of_char token - int_of_char '0') + number * 10) stack'
        | _  -> failwith "test")
    | ' ' -> operation_mode := 0
    | _ -> failwith "unsupported"
      
  in

  let tokens = List.of_seq (String.to_seq expression) in

  List.iter process_token tokens;

  let result, _ = Stack.pop !stack in
  (*this is the result of the whole operation*)
  result
