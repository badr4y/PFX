open Ast

let rec generate env depth expr lexbuf =
  match expr with
  | Const n -> [PUSH(n,lexbuf)], depth + 1
  | Var v ->
    let pos = List.assoc v env in
    [PUSH(depth - pos, lexbuf); GET(lexbuf)], depth + 1;
  | Binop (op, expr1, expr2) ->
    let cmds1, depth1 = generate env depth expr2 lexbuf in
    let cmds2, depth2 = generate env depth1 expr1 lexbuf in
    cmds1 @ cmds2 @ [op_to_pfx op lexbuf], depth2 - 1
  | Uminus expr ->
    let cmds, new_depth = generate env depth expr lexbuf in
    cmds @ [PUSH(0, lexbuf); SUB(lexbuf)], new_depth
  | Fun (param, body) ->
    let body_cmds, _ = generate ((param, depth) :: env) (depth + 1) body lexbuf in
    [PUSHSEQ(body_cmds, lexbuf)], depth
  | App (e1, e2) ->
    let arg_cmds, depth1 = generate env depth e2 lexbuf in
    let fun_cmds, depth2 = generate env depth1 e1 lexbuf in
    arg_cmds @ fun_cmds @ [EXEC(lexbuf); SWAP(lexbuf); POP(lexbuf)], depth2 - 1