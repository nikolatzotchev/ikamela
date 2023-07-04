type stack_element =
  | Integer of { value : int }
  | String of { value : string }
  | Float of { value : float }

let print_stack_element = function
  | Integer p -> print_int p.value
  | String s -> print_string s.value
  | Float f -> print_float f.value
(* test#derived1_method "daf" "dafkj" *)

(* Stack module, we need a custom one in order to implement also the other operations*)
(* we do not have only pop and push*)
module Stack = struct
  type 'a t = 'a list

  let empty = []
  let is_empty stack = match stack with [] -> true | _ -> false
  let push x stack = x :: stack

  let pop stack =
    match stack with [] -> failwith "Stack is empty" | x :: xs -> (x, xs)

  let print_stack stack = List.iter print_stack_element stack
end

let char_to_int char = Integer { value = int_of_char char - int_of_char '0' }

(* Calculator function *)
(* right now only basic operation on integers are supported*)
(* and we do not look at the operation mode*)
let calculate expression =
  let stack = ref Stack.empty in

  let apply_operator operator =
    let operand2, stack' = Stack.pop !stack in
    let operand1, stack'' = Stack.pop stack' in
    match operator with
    | '+' -> (
        match (operand1, operand2) with
        | Integer v, Integer u ->
            let sum = v.value + u.value in
            let i = Integer { value = sum } in
            stack := Stack.push i stack''
        (*do the rest*)
        | _, _ -> failwith "not implemented")
    | '-' -> (
        match (operand1, operand2) with
        | Integer v, Integer u ->
            let sum = v.value - u.value in
            let i = Integer { value = sum } in
            stack := Stack.push i stack''
        (*do the rest*)
        | _, _ -> failwith "not implemented")
    | '*' -> (
        match (operand1, operand2) with
        | Integer v, Integer u ->
            let mul = v.value * u.value in
            let i = Integer { value = mul } in
            stack := Stack.push i stack''
        (*do the rest*)
        | _, _ -> failwith "not implemented * ")
    | '/' -> (
        match (operand1, operand2) with
        | Integer v, Integer u ->
            let sum = v.value / u.value in
            let i = Integer { value = sum } in
            stack := Stack.push i stack''
        (*do the rest*)
        | _, _ -> failwith "not implemented")
    | _ -> failwith "Invalid operator"
  in

  let rec process_token operation_mode token =
    match operation_mode with
    | 0 -> (
        match token with
        | '+' | '-' | '*' | '/' ->
            apply_operator token;
            0
        | '0' .. '9' ->
            stack := Stack.push (char_to_int token) !stack;
            -1
        | ' ' -> 0
        | '(' ->
            stack := Stack.push (String { value = "(" }) !stack;
            1
        (*This is the apply immediately which should take the string on the stakand evaluate it, if the top element is not a string do nothing*)
        | '@' -> (
            let stack_entry, stack' = Stack.pop !stack in
            match stack_entry with
            (* if string pop and apply *)
            | String str ->
                let len = String.length str.value in
                if len <= 2 then 0
                else
                  let new_str = String.sub str.value 1 (len - 2) in
                  stack := stack';
                  String.fold_left process_token operation_mode new_str
            (* else do nothing *)
            | _ -> 0)
        | _ -> failwith "unsupported")
    (* here we have integer construction mode *)
    (* what is left to be done is switching ot float create mode*)
    | _ when operation_mode = -1 -> (
        match token with
        | '0' .. '9' -> (
            let stack_entry, stack' = Stack.pop !stack in
            match stack_entry with
            | Integer number ->
                stack :=
                  Stack.push
                    (Integer
                       {
                         value =
                           int_of_char token - int_of_char '0'
                           + (number.value * 10);
                       })
                    stack';
                -1
            | Float _ -> failwith "not implemented -1 mode"
            | String _ -> failwith "not implemented -1 mode")
        | ' ' -> 0
        | _ -> process_token 0 token)
    (* string creation mode*)
    | _ when operation_mode > 0 -> (
        let stackentry, stack' = Stack.pop !stack in
        match stackentry with
        | String s -> (
            match token with
            | '(' ->
                stack := Stack.push (String { value = s.value ^ "(" }) stack';
                operation_mode + 1
            | ')' ->
                stack := Stack.push (String { value = s.value ^ ")" }) stack';
                operation_mode - 1
            | c ->
                stack :=
                  Stack.push
                    (String { value = s.value ^ String.make 1 c })
                    stack';
                operation_mode)
        | _ -> failwith "not supported")
    | _ -> failwith "unsoported"
  in

  let tokens = List.of_seq (String.to_seq expression) in
  let _ = List.fold_left process_token 0 tokens in

  !stack
