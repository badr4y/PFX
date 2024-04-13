%{
open Ast

%}

(**************
 * The tokens *
 **************)

%token EOF
%token <Lexing.lexbuf> PLUS MINUS TIMES DIV MOD POP SWAP
%token <int> INT
%token <int*Lexing.lexbuf> PUSH

(******************************
 * Entry points of the parser *
 ******************************)

%start <Ast.program> program

%%

(*************
 * The rules *
 *************)

program:
  | arg_count= INT commands = command_listEOF { (arg_count, commands)}

command_list:
  | {[]}
  | cmd=command rest=command_list {cmd :: rest}

command:
  | result=PUSH { Push(fst result, snd result) }
  | lexbuf= POP          { POP(lexbuf) }
  | lexbuf= SWAP          { SWAP(lexbuf) }
  | lexbuf=PLUS          { ADD(lexbuf) }
  | lexbuf=MINUS          { SUB(lexbuf) }
  | lexbuf=DIV          { DIV(lexbuf) }
  | lexbuf=MOD          { REM(lexbuf) }

%%
