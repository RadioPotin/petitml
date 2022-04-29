

let usage_analysis (ast : Ast.program) (env : Env.t) : unit =
  let open Ast in
  let assert_variable_usage = function
    | [] -> ()
    | var_list -> failwith @@ Format.asprintf "unused %s %a"
      (if List.length var_list = 1 then
        "variable"
      else
        "variables")
      (Format.pp_print_list
        ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
        (fun fmt unused_var_newname ->
          match Hashtbl.find_opt env.old_names unused_var_newname with
          | None -> assert false
          | Some old_varname ->
              Format.fprintf fmt "%s" old_varname))
      var_list
  in
  let rec aux acc = function
    | Literal _l -> assert_variable_usage acc
    | Var s ->
        let unused =
          List.filter (fun var -> not @@ String.equal var s) acc
        in assert_variable_usage unused
    | Bind (s, exp1, exp2) ->
        let unused_vars = s::acc in
        aux unused_vars exp1;
        aux unused_vars exp2
    | Abstract (s, exp) ->
        let unused_vars = s::acc in
        aux unused_vars exp
    | Apply (exp1, exp2) ->
        let unused_vars = acc in
        aux unused_vars exp1;
        aux unused_vars exp2
    | If (exp1, exp2, exp3) ->
        let unused_vars = acc in
        aux unused_vars exp1;
        aux unused_vars exp2;
        aux unused_vars exp3
  in
  aux [] ast
