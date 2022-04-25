type t = {
  old_names : (string, string) Hashtbl.t
}

let empty () = {
  old_names = Hashbl.create 512;
}
