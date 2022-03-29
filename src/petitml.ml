open Petitml.Parse

let () =
  let file = Sys.argv.(1) in
  match from_file file with
  | Ok prog ->
      let fmt = Format.std_formatter in
      Petitml.Pp.expression fmt prog
  | Error mess -> Format.printf "%s" mess
