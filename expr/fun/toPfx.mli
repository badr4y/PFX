(* Function that generate a Pfx program from an Expr program *)
val generate : (string * int) list -> int -> expr -> Lexing.lexbuf -> (command list * int)
