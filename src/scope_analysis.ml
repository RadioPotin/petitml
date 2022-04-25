module Scope = Map.Make (String)

let scope_analysis (ast : Ast.program) (environment : Env.t) : Ast.program * Env.t =
  let open Ast in
  let make_fresh =
    let seen = Hashtbl.create 32 in
    fun id ->
      match Hashtbl.find_opt seen id with
      | None ->
        Hashtbl.add seen id 0;
        id
      | Some nb ->
        let newid = id ^ string_of_int nb in
        Hashtbl.add seen id (nb + 1);
        newid
  in
  let rec aux env = function
    | Literal l -> Literal l
    | Var s -> begin
      match Scope.find_opt s env with
      | None -> failwith @@ Format.sprintf "unbound value %s" s
      | Some id -> Var id
    end
    | Bind (s, exp1, exp2) ->
      let s' = make_fresh s in
      let env' = Scope.add s s' env in
      Hashtbl.add environment.old_names s' s;
      Bind (s', aux env exp1, aux env' exp2)
    | Abstract (s, exp) ->
      let s' = make_fresh s in
      let env' = Scope.add s s' env in
      Hashtbl.add environment.old_names s' s;
      Abstract (s', aux env' exp)
    | Apply (exp1, exp2) -> Apply (aux env exp1, aux env exp2)
    | If (exp1, exp2, exp3) -> If (aux env exp1, aux env exp2, aux env exp3)
  in
  (aux Scope.empty ast, environment)
