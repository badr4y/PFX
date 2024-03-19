open Ast

let generate = function
  | Const c -> [Push c]
  | Binop(op, e1, e2) ->
      let code_e1 = generate e1 in
      let code_e2 = generate e2 in
      begin
        match op with
        | Badd -> code_e1 @ code_e2 @ [Add]
        | Bsub -> code_e1 @ code_e2 @ [Sub]
        | Bmul -> code_e1 @ code_e2 @ [Mul]
        | Bdiv -> code_e1 @ code_e2 @ [Div]
        | Bmod -> code_e1 @ code_e2 @ [Rem]
      end
  | Uminus e ->
      let code_e = generate e in
      code_e @ [Push (-1); Mul]
  | Var _ -> failwith "Not yet supported"
