{
  type token =
    | PUSH of int | POP | ADD | SUB | MUL | DIV | REM | INT of int | EOF

  let print_token = function

    | PUSH n -> print_string ("PUSH " ^ string_of_int n)
    | POP -> print_string "POP"
    | ADD -> print_string "ADD"
    | SUB -> print_string "SUB"
    | MUL -> print_string "MUL"
    | DIV -> print_string "DIV"
    | REM -> print_string "REM"
    | EOF -> print_string "EOF"
    | INT i -> print_int i


  let mk_int nb =
    try INT (int_of_string nb)
    with Failure _ -> failwith (Printf.sprintf "Illegal integer '%s': " nb)

  let mk_push nb =
    try PUSH (int_of_string nb)
    with Failure _ -> failwith (Printf.sprintf "Illegal push operation '%s': " nb)
}

let newline = (['\n' '\r'] | "\r\n")
let blank = [' ' '\014' '\t' '\012']
let not_newline_char = [^ '\n' '\r']
let digit = ['0'-'9']

rule token = parse
  (* newlines *)
  | newline { token lexbuf }
  (* blanks *)
  | blank + { token lexbuf }
  (* end of file *)
  | eof      { EOF }
  (* comments *)
  | "--" not_newline_char*  { token lexbuf }
  (* integers *)
  | digit+ as nb           { mk_int nb }
  (* commands  *)
  | "push " + (digit+ as nb)  { mk_push nb  }
  | "pop"     { POP }
  | "add"     { ADD }
  | "sub"     { SUB }
  | "mul"     { MUL }
  | "div"     { DIV }
  | "rem"     { REM }
  (* illegal characters *)
  | _ as c                  { failwith (Printf.sprintf "Illegal character '%c': " c) }
