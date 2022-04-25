let usage_analysis (_ast : Ast.program) (_env : Env.t) : unit =
  ()
  (*
  let rec aux tbl = function
    | Literal l -> Literal l
    | Var s -> Var s
    | Bind (s, exp1, exp2) -> Bind (s', exp1, exp2)
    | Abstract (s, exp) -> Abstract (s', exp)
    | Apply (exp1, exp2) -> Apply (exp1, exp2)
    | If (exp1, exp2, exp3) -> If (exp1, exp2, exp3)
  in
  aux tbl
  *)
