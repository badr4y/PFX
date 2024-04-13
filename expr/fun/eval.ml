open Ast
open Printf
open Utils

type stack_elt =
  | Int of int
  | Seq of command list

let str_of_stack_elt = function
  | Int i -> string_of_int i
  | Seq s -> "(" ^ String.concat " " (List.map str_of_command s) ^")"

let str_of_stack stack =
  sprintf "[%s]" (String.concat ";" (List.map str_of_stack_elt stack))

let str_of_state (cmds,stack) =
  (match cmds with
  | [] -> "no command"
  | cmd::_ -> sprintf "executing %s" (str_of_command cmd)) ^
  (sprintf " with stack %s" (str_of_stack stack))

let rec get_ith_elem i lst =
  match i, lst with
  | _, [] -> None
  | 1, x :: _ -> Some x
  | n, _ :: tail when n > 1 -> get_ith_elem (n-1) tail
  | _ -> None

let step_state state =
  match state with
  | [], _ -> Error("Nothing to step", state)
  | PUSH (v, _) :: q, stack -> Ok(q, Int v :: stack)
  | PUSHSEQ(s, _) :: q, stack -> Ok(q, Seq s :: stack)
  | ADD(_) :: q, Int v1 :: Int v2 :: s_tail -> Ok (q, Int (v1 + v2) :: s_tail)
  | ADD(lexbuf) :: _, _ -> Location.raise_error "Not enough elements to add or found non-integer elements" lexbuf
  | POP(lexbuf) :: _, [] -> Location.raise_error "Stack underflow on pop" lexbuf
  | POP(_) :: q, _ :: s_tail -> Ok (q, s_tail)
  | SWAP(_) :: q, v1 :: v2 :: s_tail -> Ok (q, v2 :: v1 :: s_tail)
  | SWAP(lexbuf) :: _, _ -> Location.raise_error "Not enough elements to swap" lexbuf
  | SUB(_) :: q, Int v1 :: Int v2 :: s_tail -> Ok (q, Int (v1 - v2) :: s_tail)
  | SUB(lexbuf) :: _, _ -> Location.raise_error "Not enough elements to subtract or found non-integer elements" lexbuf
  | MUL(_) :: q, Int v1 :: Int v2 :: s_tail -> Ok (q, Int (v1 * v2) :: s_tail)
  | MUL(lexbuf) :: _, _ -> Location.raise_error "Not enough elements to multiply or found non-integer elements" lexbuf
  | DIV(lexbuf) :: _, Int _ :: Int 0 :: _ -> Location.raise_error "Division by zero" lexbuf
  | DIV(_) :: q, Int v1 :: Int v2 :: s_tail -> Ok (q, Int (v1 / v2) :: s_tail)
  | DIV(lexbuf) :: _, _ -> Location.raise_error "Not enough elements to divide or found non-integer elements" lexbuf
  | REM(lexbuf) :: _, Int _ :: Int 0 :: _ -> Location.raise_error "Division by zero in rem" lexbuf
  | REM(_) :: q, Int v1 :: Int v2 :: s_tail -> Ok (q, Int (v1 mod v2) :: s_tail)
  | REM(lexbuf) :: _, _ -> Location.raise_error "Not enough elements to compute remainder or found non-integer elements" lexbuf
  | EXEC(_) :: q, Seq s :: s_tail -> Ok(s @ q, s_tail)
  | EXEC(lexbuf) :: _, _ -> Location.raise_error "Not enough elements to execute or found non-executable sequence elements" lexbuf
  | GET(lexbuf) :: _, Seq _ :: _ -> Location.raise_error "Expected an index to get corresponding value" lexbuf
  | GET(lexbuf) :: _, Int _ :: [] -> Location.raise_error "Not enough elements to get indexed value" lexbuf
  | GET(lexbuf) :: _, [] -> Location.raise_error "Not enough elements to get indexed value" lexbuf
  | GET(lexbuf) :: q, Int i :: s_tail ->
    begin
      match get_ith_elem i s_tail with
      | Some (Int v) -> Ok(q, Int v :: s_tail)
      | Some (Seq _) -> Location.raise_error "Expected an integer at the index, found a sequence" lexbuf
      | None -> Location.raise_error "Index out of bounds for get" lexbuf
    end

let eval_prog (numargs, cmds) args =
  let rec exec = function
    | [], [] -> Ok None
    | [], Int v::_ -> Ok (Some v)
    | state ->
      begin
        match step_state state with
        | Ok s -> exec s
        | Error e -> Error e
      end
  in
  if numargs = List.length args then
    match exec (cmds, List.map (fun x -> Int x) args) with
    | Ok None -> printf "No result\n"
    | Ok(Some result) -> printf "= %i\n" result
    | Error(msg, s) -> printf "Raised error %s in state %s\n" msg (str_of_state s)
  else printf "Raised error: Mismatch between expected and actual number of args\n"
