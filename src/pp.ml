open Ast

let literal fmt = function
  | Unit -> Format.fprintf fmt "()"
  | Bool b -> if b then Format.fprintf fmt "true" else Format.fprintf fmt "false"
  | Int i -> Format.fprintf fmt "%d" i

let rec expression fmt = function
  | Literal l -> literal fmt l
  | Var s -> Format.fprintf fmt "%s" s
  | Bind (s, exp1, exp2) -> Format.fprintf fmt "let %s = %a in %a" s expression exp1 expression exp2
  | Abstract (s, exp) -> Format.fprintf fmt "(fun %s -> %a)"
  | Apply (exp1, exp2) -> Format.fprintf fmt "%a %a" expression exp1 expression exp2
  | If (exp1, exp2, exp3) -> Format.fprintf fmt "if %a then %a else %a end" expression exp1 expression exp2 expression exp3
