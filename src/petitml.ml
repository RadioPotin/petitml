open Petitml.Parse

let () =
  let file = Sys.argv.(1) in
  match from_file file with
  | Ok prog ->
    let fmt = Format.std_formatter in
    Format.fprintf fmt "%a@\n" Petitml.Pp.expression prog;
    let prog = Petitml.Scope_analysis.scope_analysis prog in
    Format.fprintf fmt "Transformed:@\n%a@\n" Petitml.Pp.expression prog
  | Error mess -> Format.printf "%s" mess
