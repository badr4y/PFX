{
open BasicPfx
open Utils

(* type token =
  | PUSH of int | POP | ADD | SUB | MUL | DIV | REM | INT of int | EOF *)

let args = ref []

(* main function *)
let parse_eval file =
  print_string("file "^file^" is being treated!\n");
  try 
    let input_file = open_in_file in
    let lexbuf = Lexing.from_channel input_file in
    begin
      try
        let pfx_prog = Parser.program Lexer.token lexbuf in
        Eval.eval_program pfx_prog !args
      with Parser.Error -> print_string "syntax error"
    end;
    close_in (input_file)
  with
    | Location.Error (msg, loc) ->
      Printf.printf "error: %s at %s\n" msg (Location.string_of_loc);
    | Sys_error _ -> print_endline ("can't find file '"^file^"'")

let _ =
  (*function to save arguments*)
  let save_arg i = args := !args@[i] in
  (*each option -a INTEGER is considered as an argument *)
  Arg.parse ["-a",Arg.Int save_arg,"integer argument"] parse_eval ""

(*let print_token = function
  | PUSH n -> print_string ("PUSH " ^ string_of_int n)
  | POP -> print_string "POP"
  | ADD -> print_string "ADD"
  | SUB -> print_string "SUB"
  | MUL -> print_string "MUL"
  | DIV -> print_string "DIV"
  | REM -> print_string "REM"
  | EOF -> print_string "EOF"
  | INT i -> print_int i

let mk_int loc nb =
  try INT (int_of_string nb)
  with Failure _ -> raise (Error (Printf.sprintf "Illegal integer '%s': " nb, loc))

let mk_push loc =
  PUSH

%}

let newline = ('\n' | '\r' | "\r\n")
let blank = [' ' '\014' '\t' '\012']
let not_newline_char = [^ '\n' '\r']
let digit = ['0'-'9']

rule token = parse
  (* newlines *)
  | newline { token lexbuf }
  (* blanks *)
  | blank+ { token lexbuf }
  (* end of file *)
  | eof      { EOF }
  (* comments *)
  | "--" not_newline_char*  { token lexbuf }
  (* integers *)
  | digit+ as nb           { mk_int (symbol_loc (Lexing.lexeme_start_p lexbuf) (Lexing.lexeme_end_p lexbuf)) nb }
  (* commands  *)
  | "push"     { mk_push (symbol_loc (Lexing.lexeme_start_p lexbuf) (Lexing.lexeme_end_p lexbuf))  }
  | "pop"      { POP }
  | "add"      { ADD }
  | "sub"      { SUB }
  | "mul"      { MUL }
  | "div"      { DIV }
  | "rem"      { REM }
  (* illegal characters *)
  | _ as c                  { raise (Error (Printf.sprintf "Illegal character '%c': " c, symbol_loc (Lexing.lexeme_start_p lexbuf) (Lexing.lexeme_end_p lexbuf))) }
*)
}