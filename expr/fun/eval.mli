type stack_elt =
  | Int of int
  | Seq of command list

val str_of_stack_elt : stack_elt -> string
val str_of_stack : stack_elt list -> string
val str_of_state : command list * stack_elt list -> string
val get_ith_elem : int -> 'a list -> 'a option
val step_state : command list * stack_elt list -> (command list * stack_elt list, string) result
val eval_prog : int * command list -> int list -> unit
