open Ast

let rec generate = function
  | Const c -> [BasicPfx.Ast.Push c]
  | Binop(op, e1, e2) ->
      let code_e1 = generate e1 in
      let code_e2 = generate e2 in
      begin
        match op with
        | Badd -> code_e1 @ code_e2 @ [BasicPfx.Ast.Add]
        | Bsub -> code_e1 @ code_e2 @ [BasicPfx.Ast.Sub]
        | Bmul -> code_e1 @ code_e2 @ [BasicPfx.Ast.Mul]
        | Bdiv -> code_e1 @ code_e2 @ [BasicPfx.Ast.Div]
        | Bmod -> code_e1 @ code_e2 @ [BasicPfx.Ast.Rem]
      end
  | Uminus e ->
      let code_e = generate e in
      code_e @ [BasicPfx.Ast.Push (-1); BasicPfx.Ast.Mul]
  | Var _ -> failwith "Not yet supported"