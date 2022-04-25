type t = {
  old_names : (string, string) Hashtbl.t
}

let empty () = {
  old_names = Hashtbl.create 512;
}
