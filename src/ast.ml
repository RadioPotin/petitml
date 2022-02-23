type literal =
  | Unit
  | Bool of bool
  | Int of int

type expression =
  | Literal of literal
  | Var of string
  | Bind of string * expression * expression
  | Abstract of string * expression
  | Apply of string * expression
  | If of expression * expression * expression
