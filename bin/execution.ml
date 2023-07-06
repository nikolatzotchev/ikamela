type stack_element =
  | Integer of { value : int }
  | String of { value : string }
  | Float of { value : float }

let print_stack_element = function
  | Integer p -> print_int p.value
  | String s -> print_string s.value
  | Float f -> print_float f.value

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

(* Converts a char to a stack element with an integer value*)
let char_to_stack_int char =
  Integer { value = int_of_char char - int_of_char '0' }
(* Converts a char into an integer*)
let char_to_int char = int_of_char char - int_of_char '0'

let epsilon = 0.001

let compare_float x y =
  if abs_float (x -. y) <= epsilon *. max (abs_float x) (abs_float y) then 0
  else if x < y then -1
  else 1

let compare_values operand1 operand2 =
  match (operand1, operand2) with
  | Integer v1, Integer v2 -> compare v1.value v2.value
  | Float v1, Float v2 -> compare_float v1.value v2.value
  | Float v1, Integer v2 -> compare_float v1.value (float_of_int v2.value)
  | Integer v1, Float v2 -> compare_float (float_of_int v1.value) v2.value
  | String v1, String v2 -> compare v1.value v2.value
  | String _, _ -> -1
  | _, String _ -> 1

let rec evaluate_one_step mode expression_list stack_ =
  let stack = ref stack_ in
  let tokens = expression_list in

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
        | Float v, Float u ->
            let sum = v.value +. u.value in
            let result = Float { value = sum } in
            stack := Stack.push result stack''
        | Integer v, Float u | Float u, Integer v ->
            let sum = float_of_int v.value +. u.value in
            let result = Float { value = sum } in
            stack := Stack.push result stack''
        (*do the rest*)
        | _, _ -> failwith "not implemented")
    | '-' -> (
        match (operand1, operand2) with
        | Integer v, Integer u ->
            let sum = v.value - u.value in
            let i = Integer { value = sum } in
            stack := Stack.push i stack''
        | Float v, Float u ->
            let sum = v.value -. u.value in
            let result = Float { value = sum } in
            stack := Stack.push result stack''
        | Integer v, Float u | Float u, Integer v ->
            let sum = float_of_int v.value -. u.value in
            let result = Float { value = sum } in
            stack := Stack.push result stack''
        (*do the rest*)
        | _, _ -> failwith "not implemented")
    | '*' -> (
        match (operand1, operand2) with
        | Integer v, Integer u ->
            let mul = v.value * u.value in
            let i = Integer { value = mul } in
            stack := Stack.push i stack''
        | Float v, Float u ->
            let sum = v.value *. u.value in
            let result = Float { value = sum } in
            stack := Stack.push result stack''
        | Integer v, Float u | Float u, Integer v ->
            let sum = float_of_int v.value *. u.value in
            let result = Float { value = sum } in
            stack := Stack.push result stack''
        (*do the rest*)
        | _, _ -> failwith "not implemented * ")
    | '/' -> (
        match (operand1, operand2) with
        | Integer v, Integer u ->
            let sum = v.value / u.value in
            let i = Integer { value = sum } in
            stack := Stack.push i stack''
        | Float v, Float u ->
            let sum = v.value /. u.value in
            let result = Float { value = sum } in
            stack := Stack.push result stack''
        | Integer v, Float u ->
            let sum = float_of_int v.value /. u.value in
            let result = Float { value = sum } in
            stack := Stack.push result stack''
        | Float v, Integer u ->
            let sum = v.value /. float_of_int u.value in
            let result = Float { value = sum } in
            stack := Stack.push result stack''
        (*do the rest*)
        | _, _ -> failwith "not implemented")
    | '%' -> (
        match (operand1, operand2) with
        | Integer v, Integer u ->
            let sum = v.value mod u.value in
            let i = Integer { value = sum } in
            stack := Stack.push i stack''
        | Float v, Float u ->
            let sum = mod_float v.value u.value in
            let result = Float { value = sum } in
            stack := Stack.push result stack''
        | Integer v, Float u ->
            let sum = mod_float (float_of_int v.value) u.value in
            let result = Float { value = sum } in
            stack := Stack.push result stack''
        | Float v, Integer u ->
            let sum = mod_float v.value (float_of_int u.value) in
            let result = Float { value = sum } in
            stack := Stack.push result stack''
        (*do the rest*)
        | _, _ -> failwith "not implemented")
    | '&' -> (
        match (operand1, operand2) with
        | Integer v, Integer u -> (
            match (v.value, u.value) with
            | 0, _ -> stack := Stack.push (Integer { value = 0 }) stack''
            | _, 0 -> stack := Stack.push (Integer { value = 0 }) stack''
            | _, _ -> stack := Stack.push (Integer { value = 1 }) stack'')
        (*do the rest*)
        | _, _ -> stack := Stack.push (String { value = "()" }) stack'')
    | '|' -> (
        match (operand1, operand2) with
        | Integer v, Integer u -> (
            match (v.value, u.value) with
            | 0, 0 -> stack := Stack.push (Integer { value = 0 }) stack''
            | _, _ -> stack := Stack.push (Integer { value = 1 }) stack'')
        (*do the rest*)
        | _, _ -> stack := Stack.push (String { value = "()" }) stack'')
    | '=' ->
      let comparison = compare_values operand1 operand2 in
      let result = if comparison = 0 then 1 else 0 in
      let i = Integer { value = result } in
      stack := Stack.push i stack''
    | '<' ->
      let comparison = compare_values operand1 operand2 in
      let result = if comparison < 0 then 1 else 0 in
      let i = Integer { value = result } in
      stack := Stack.push i stack''
    | '>' ->
      let comparison = compare_values operand1 operand2 in
      let result = if comparison > 0 then 1 else 0 in
      let i = Integer { value = result } in
      stack := Stack.push i stack''
    (*do the rest*)
    | _ -> failwith "Invalid operator"
  (* Constructs one float digit when the operation_mode is less then -1 *)
  and construct_float_number mode token =
    let operand, stack' = Stack.pop !stack in
    match operand with
    | Float f ->
        let new_value =
          f.value
          +. float_of_int (char_to_int token)
             /. (10.0 ** float_of_int (-mode - 1))
        in
        let new_float = Float { value = new_value } in
        stack := Stack.push new_float stack'
    | _ -> failwith "Invalid operand for constructing float number"
  in

  let rec process_token operation_mode token rest =
    match operation_mode with
    | 0 -> (
        match token with
        | '+' | '-' | '*' | '/' | '&' | '|' | '%' | '=' | '<' | '>' ->
            apply_operator token;
            (0, rest)
        (*start integer creation*)
        | '0' .. '9' ->
            stack := Stack.push (char_to_stack_int token) !stack;
            (-1, rest)
        (*go to next number*)
        | ' ' -> (0, rest)
        (*start string creation*)
        | '(' ->
            stack := Stack.push (String { value = "(" }) !stack;
            (1, rest)
        (*This is the apply immediately which should take the string on the stack and evaluate it, if the top element is not a string do nothing*)
        | '@' -> (
            let stack_entry, stack' = Stack.pop !stack in
            match stack_entry with
            (* if string pop and apply *)
            | String str ->
                let len = String.length str.value in
                if len <= 2 then (0, rest)
                else
                  let new_str = String.sub str.value 1 (len - 2) in
                  stack := stack';
                  (0, List.of_seq (String.to_seq new_str) @ rest)
            (* else do nothing *)
            | _ -> (0, rest))
        (* pop string -> put at the end with @*)
        | '\\' -> (
            let stack_entry, stack' = Stack.pop !stack in
            match stack_entry with
            (* if string pop and apply *)
            | String str ->
                stack := stack';
                (*just put at end of command stream*)
                (0, rest @ List.of_seq (String.to_seq str.value) @ [ '@' ])
            (* else do nothing *)
            | _ -> (0, rest))
        | _ -> failwith "unsupported")
    (* here we have integer construction mode *)
    | _ when operation_mode = -1 -> (
        match token with
        | '0' .. '9' -> (
            let stack_entry, stack' = Stack.pop !stack in
            match stack_entry with
            | Integer number ->
                stack :=
                  Stack.push
                    (Integer { value = char_to_int token + (number.value * 10) })
                    stack';
                (-1, rest)
            | Float _ -> failwith "not implemented -1 mode"
            | String _ -> failwith "not implemented -1 mode")
        (* Convert the current stack entry to a float and push it back then switch to float construction mode *)
        | '.' -> (
            let stack_entry, stack' = Stack.pop !stack in
            match stack_entry with
            | Integer number ->
                let float_value = float_of_int number.value in
                stack := Stack.push (Float { value = float_value }) stack';
                (-2, rest)
            | _ -> failwith "Invalid token after dot")
        | ' ' -> (0, rest)
        | _ -> process_token 0 token rest)
    (* Float construction mode *)
    | _ when operation_mode < -1 -> (
        match token with
        | '0' .. '9' ->
            construct_float_number operation_mode token;
            (operation_mode - 1, rest)
        | '.' ->
            stack := Stack.push (Float { value = 0.0 }) !stack;
            (-2, rest)
        | ' ' -> (0, rest)
        | _ -> process_token 0 token rest)
    (* string creation mode*)
    (* this should be ready*)
    | _ when operation_mode > 0 -> (
        let stackentry, stack' = Stack.pop !stack in
        match stackentry with
        | String s -> (
            match token with
            | '(' ->
                stack := Stack.push (String { value = s.value ^ "(" }) stack';
                (operation_mode + 1, rest)
            | ')' ->
                stack := Stack.push (String { value = s.value ^ ")" }) stack';
                (operation_mode - 1, rest)
            | c ->
                stack :=
                  Stack.push
                    (String { value = s.value ^ String.make 1 c })
                    stack';
                (operation_mode, rest))
        | _ -> failwith "not supported")
    | _ -> failwith "not supported"
  in

  match tokens with
  | [] -> !stack
  | x :: xp ->
      (*
        we need this becouse of the string application modes
        @ will return in new_xp the popped string + xp and it will be just executed   
        / will put the popped string on the end of xp and will be executed later
         *)
      let new_mode, new_xp = process_token mode x xp in
      evaluate_one_step new_mode new_xp !stack

(* Calculator function*)
(* right now only basic operation on integers are supported*)
let calculate expression =
  evaluate_one_step 0 (List.of_seq (String.to_seq expression)) Stack.empty
