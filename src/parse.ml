let from_lexing =
  let parse =
    MenhirLib.Convert.Simplified.traditional2revised Menhir_parser.program
  in
  fun buf ->
    let provide () =
      let tok = Lexer.token buf in
      let start, stop = Sedlexing.lexing_positions buf in
      (tok, start, stop)
    in
    try Ok (parse provide) with
    | Menhir_parser.Error -> Error "unexpected token"
    | Lexer.Error (_pos, _msg) -> Error "lexer error"

let from_file f =
  let chan = open_in f in
  let result = from_lexing (Sedlexing.Utf8.from_channel chan) in
  close_in chan;
  result
