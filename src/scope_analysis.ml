module Env = Map.Make (String)

let scope_analysis (ast : Ast.program) : Ast.program =
  let open Ast in
  let make_fresh =
    let seen = Hashtbl.create 32
    in fun id ->
      match Hashtbl.find_opt seen id with
      | None ->
        Hashtbl.add seen id 0;
        id ^ "0"
      | Some nb ->
        let newid = id ^ string_of_int nb in
        Hashtbl.add seen id (nb + 1);
        newid
  in
  let rec aux env = function
    | Literal l -> Literal l
    | Var s -> begin
      match Env.find_opt s env with
      | None -> Var (make_fresh s)
      | Some id -> Var (id)
    end
    | Bind (s, exp1, exp2) -> begin
      match Env.find_opt s env with
      | None ->
          let newid = make_fresh s in
          let env = Env.add newid s env in
          Bind (newid, aux env exp1, aux env exp2)
      | Some _id ->
          let newid = make_fresh s in
          let env = Env.add newid s env in
          Bind (newid, aux env exp1, aux env exp2)
    end
    | Abstract (s, exp) -> begin
      match Env.find_opt s env with
      | None ->
          let newid = make_fresh s in
          let env = Env.add newid s env in
          Abstract (newid, aux env exp)
      | Some _id ->
          let newid = make_fresh s in
          let env = Env.add newid s env in
          Abstract (newid, aux env exp)
    end
    | Apply (exp1, exp2) -> Apply (aux env exp1, aux env exp2)
    | If (exp1, exp2, exp3) -> If (aux env exp1, aux env exp2, aux env exp3)
  in
  aux Env.empty ast
