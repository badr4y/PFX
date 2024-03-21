open Ast
open Printf

let string_of_stack stack = sprintf "[%s]" (String.concat ";" (List.map string_of_int stack))

let string_of_state (cmds,stack) =
  (match cmds with
   | [] -> "no command"
   | cmd::_ -> sprintf "executing %s" (string_of_command cmd))^
    (sprintf " with stack %s" (string_of_stack stack))

(* Question 4.2 *)
let step state =
  match state with
  | [], _ -> Error("Nothing to step", state)
  | Push n :: q, s -> Ok (q, n :: s)
  | Pop :: q, _ :: s -> Ok (q, s)
  | Pop :: _, [] -> Error("Pop from an empty stack", ([], []))
  | Add :: q, x :: y :: s -> Ok (q, (x + y) :: s)
  | Add :: _, _ -> Error("Not enough operands for addition", ([], []))
  | Sub :: q, x :: y :: s -> Ok (q, (y - x) :: s)
  | Sub :: _, _ -> Error("Not enough operands for subtraction", ([], []))
  | Mul :: q, x :: y :: s -> Ok (q, (x * y) :: s)
  | Mul :: _, _ -> Error("Not enough operands for multiplication", ([], []))
  | Div :: q, x :: y :: s ->
      if x = 0 then
        Error("Division by zero", ([], []))
      else
        Ok (q, (y / x) :: s)
  | Div :: _, _ -> Error("Not enough operands for division", ([], []))
  | Rem :: q, x :: y :: s ->
      if x = 0 then
        Error("Division by zero in remainder", ([], []))
      else
        Ok (q, (y mod x) :: s)
  | Rem :: _, _ -> Error("Not enough operands for remainder", ([], []))

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
