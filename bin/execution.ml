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

let char_to_int char = Integer { value = int_of_char char - int_of_char '0' }

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
    (*do the rest % (modulo) *)
    | _ -> failwith "Invalid operator"
  in

  let rec process_token operation_mode token rest =
    match operation_mode with
    | 0 -> (
        match token with
        | '+' | '-' | '*' | '/' ->
            apply_operator token;
            (0, rest)
        (*start integer creation*)
        | '0' .. '9' ->
            stack := Stack.push (char_to_int token) !stack;
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
                (-1, rest)
            | Float _ -> failwith "not implemented -1 mode"
            | String _ -> failwith "not implemented -1 mode")
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
