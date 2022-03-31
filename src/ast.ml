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

module Variable : sig
  type t

  val fresh : string -> t
end = struct
  type t = string * int

  let counter = ref 0

  let fresh name =
    incr counter;
    (name, !counter)
end

type variable = Variable.t

type varexpression =
  | Vliteral of literal
  | Vvar of variable
  | Vbind of variable * varexpression * varexpression
  | Vabstract of variable * varexpression
  | Vapply of varexpression * varexpression
  | Vif of varexpression * varexpression * varexpression
