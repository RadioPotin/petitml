type literal =
  | Unit
  | Bool of bool
  | Int of int

type expression =
  | Literal of literal
  | Var of string
  | Bind of string * expression * expression
  | Abstract of string * expression
  | Apply of expression * expression
  | If of expression * expression * expression

type program = expression

(*
   Bind (bound_name, exp1, exp2) -> let bindings. Var bound_name MUST be used in exp2

 *)
