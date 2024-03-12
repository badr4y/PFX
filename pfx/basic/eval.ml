open Ast
open Printf

let string_of_stack stack = sprintf "[%s]" (String.concat ";" (List.map string_of_int stack))

let string_of_state (cmds,stack) =
  (match cmds with
   | [] -> "no command"
   | cmd::_ -> sprintf "executing %s" (string_of_command cmd))^
    (sprintf " with stack %s" (string_of_stack stack))

(* Question 4.2 *)
type error = string
type state = command list * stack
type result = (command list * stack) Or_error.t

let step (cmds, stack) =
  match cmds, stack with
  | [], _ -> Error ("Nothing to step", (cmds, stack))
  | Push n :: q, s -> Ok (q, n :: s)
  | Pop :: q, x :: s -> Ok (q, s)
  | Pop :: _, [] -> Error ("Pop from an empty stack", (cmds, stack))
  | Add :: q, x :: y :: s -> Ok (q, (x + y) :: s)
  | Add :: _, _ -> Error ("Not enough operands for addition", (cmds, stack))
  | Sub :: q, x :: y :: s -> Ok (q, (y - x) :: s)
  | Sub :: _, _ -> Error ("Not enough operands for subtraction", (cmds, stack))
  | Mul :: q, x :: y :: s -> Ok (q, (x * y) :: s)
  | Mul :: _, _ -> Error ("Not enough operands for multiplication", (cmds, stack))
  | Div :: q, x :: y :: s ->
      if x = 0 then
        Error ("Division by zero", (cmds, stack))
      else
        Ok (q, (y / x) :: s)
  | Div :: _, _ -> Error ("Not enough operands for division", (cmds, stack))
  | Rem :: q, x :: y :: s ->
      if x = 0 then
        Error ("Division by zero in remainder", (cmds, stack))
      else
        Ok (q, (y mod x) :: s)
  | Rem :: _, _ -> Error ("Not enough operands for remainder", (cmds, stack))
  | _, _ -> Error ("Unhandled instruction", (cmds, stack))


let eval_program (numargs, cmds) args =
  let rec execute = function
    | [], []    -> Ok None
    | [], v::_  -> Ok (Some v)
    | state ->
       begin
         match step state with
         | Ok s    -> execute s
         | Error e -> Error e
       end
  in
  if numargs = List.length args then
    match execute (cmds,args) with
    | Ok None -> printf "No result\n"
    | Ok(Some result) -> printf "= %i\n" result
    | Error(msg,s) -> printf "Raised error %s in state %s\n" msg (string_of_state s)
  else printf "Raised error \nMismatch between expected and actual number of args\n"

(* let rec step : pfx_instruction * stack -> stack = function
  | Push n, s -> n :: s
  | Pop, x :: s -> s
  | Pop, [] -> raise (Runtime_error "Pop from an empty stack")
  | Add, x :: y :: s -> (x + y) :: s
  | Add, _ -> raise (Runtime_error "Not enough operands for addition")
  | Sub, x :: y :: s -> (y - x) :: s
  | Sub, _ -> raise (Runtime_error "Not enough operands for subtraction")
  | Mul, x :: y :: s -> (x * y) :: s
  | Mul, _ -> raise (Runtime_error "Not enough operands for multiplication")
  | Div, x :: y :: s ->
      if x = 0 then
        raise (Runtime_error "Division by zero")
      else
        (y / x) :: s
  | Div, _ -> raise (Runtime_error "Not enough operands for division")
  | Rem, x :: y :: s ->
      if x = 0 then
        raise (Runtime_error "Division by zero in remainder")
      else
        (y mod x) :: s
  | Rem, _ -> raise (Runtime_error "Not enough operands for remainder") *)
