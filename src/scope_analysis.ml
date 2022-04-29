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
  let rec aux scope = function
    | Literal l -> Literal l
    | Var s -> begin
      match Scope.find_opt s scope with
      | None -> failwith @@ Format.sprintf "unbound value %s" s
      | Some id -> Var id
    end
    | Bind (s, exp1, exp2) ->
      let s' = make_fresh s in
      let scope' = Scope.add s s' scope in
      Hashtbl.add environment.old_names s' s;
      Bind (s', aux scope exp1, aux scope' exp2)
    | Abstract (s, exp) ->
      let s' = make_fresh s in
      let scope' = Scope.add s s' scope in
      Hashtbl.add environment.old_names s' s;
      Abstract (s', aux scope' exp)
    | Apply (exp1, exp2) -> Apply (aux scope exp1, aux scope exp2)
    | If (exp1, exp2, exp3) -> If (aux scope exp1, aux scope exp2, aux scope exp3)
  in
  (aux Scope.empty ast, environment)
