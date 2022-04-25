type literal =
  | Unit
  | Bool of bool
  | Int of int

(* EXPRESSION TYPE NotaBene:

   Bind (bound_name, exp1, exp2) ->
     let bindings.
     Var bound_name MUST be used in exp2

   Abstract (variable, exp) ->
     Anonymous functions declaration of form `fun y -> expression`

   Apply (exp1, exp2) ->
     Application of exp1 to exp2 of form `exp1 exp2` or `(exp1) (exp2)`

   If (exp0, exp1, exp2) ->
     `if` conditions of form `if exp0 then exp1 else exp2`
 *)
type expression =
  | Literal of literal
  | Var of string
  | Bind of string * expression * expression
  | Abstract of string * expression
  | Apply of expression * expression
  | If of expression * expression * expression

type program = expression
