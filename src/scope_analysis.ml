let scope_analysis_var (ast : Ast.program): Ast.varexpression =
  let open Ast in
  let rec aux = function
    | Literal l -> Vliteral l
    | Var v -> Vvar (Variable.fresh v)
    | Bind (s, exp1, exp2) -> Vbind (Variable.fresh s, aux exp1, aux exp2)
    | Abstract (s, exp) -> Vabstract (Variable.fresh s, aux exp)
    | Apply (exp1, exp2) -> Vapply (aux exp1, aux exp2)
    | If (exp1, exp2, exp3) -> Vif (aux exp1, aux exp2, aux exp3)
  in
  aux ast

let scope_analysis (ast : Ast.program): Ast.program * (string, string) Hashtbl.t=
  let open Ast in
  let vartbl = Hashtbl.create 2048 in
  let counter =
    let cnt = ref (-1)
    in fun () ->
      incr cnt;
      cnt
    in
  let rename s =
    let nb = counter () in
    s ^ (string_of_int !nb)
    in
  let track_var s =
    match Hashtbl.find_opt vartbl s with
    | None -> Hashtbl.add vartbl s s; s
    | Some var ->
        let key = rename s in
        Hashtbl.add vartbl key var; key
  in
  let rec aux = function
    | Literal l -> Literal l
    | Var v -> Var (track_var v)
    | Bind (s, exp1, exp2) -> Bind (track_var s, aux exp1, aux exp2)
    | Abstract (s, exp) -> Abstract (track_var s, aux exp)
    | Apply (exp1, exp2) -> Apply (aux exp1, aux exp2)
    | If (exp1, exp2, exp3) -> If (aux exp1, aux exp2, aux exp3)
  in
  aux ast, vartbl
