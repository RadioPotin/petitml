open Sedlexing

type token = Menhir_parser.token

open Menhir_parser

exception Error of Lexing.position * string

(* whitespaces *)

let whitespace = [%sedlex.regexp? ' ' | '\t']

let newline = [%sedlex.regexp? '\n' | '\r' | "\r\n"]

let blank = [%sedlex.regexp? whitespace | newline]

(* integers *)

let sign = [%sedlex.regexp? '+' | '-']

let digit = [%sedlex.regexp? '0'..'9']

let number = [%sedlex.regexp? '1'..'9', digit]

let int = [%sedlex.regexp? Opt sign, Plus number]

(* identifiers *)

let allowed_id_chars = [%sedlex.regexp? Plus ('0'..'9' | 'a'..'z' | 'A'.. 'Z' | '_')]

let id = [%sedlex.regexp? Plus allowed_id_chars ]

(* other literals *)

let unit = [%sedlex.regexp? "Unit"]

let bool = [%sedlex.regexp? "False" | "True"]


let rec token buf =
  match%sedlex buf with
  | blank -> token buf
  | "(" -> LPAR
  | ")" -> RPAR
  | "=" -> EQUAL
  | "in" -> IN
  | "if" -> IF
  | "then" -> THEN
  | "else" -> ELSE
  | "end" -> END
  | "let" -> LET
  | "->" -> ARROW
  | "fun" -> FUN
  (*
  | "_" -> UNDERSCORE
  | ";" -> SEMICOL
  | "<" -> LT
  | ">" -> GT
  | "<=" -> LE
  | ">=" -> GE
  | "&&" -> AND
  | "||" | "or" -> OR
  | "xor" -> XOR
  *)
  | id ->
      let id = Utf8.lexeme buf in
      let id = String.sub id 0 (String.length id - 1) in
      IDENT id
  | int ->
      INT (Utf8.lexeme buf)
  | unit ->
      UNIT
  | bool ->
      let b = Utf8.lexeme buf in
      let b = String.sub b 0 (String.length b - 1) in
      BOOL b
  | eof -> EOF
  | _ ->
      let position = fst @@ lexing_positions buf in
      let tken = Utf8.lexeme buf in
      raise @@ Error (position,
      Format.sprintf "unexpected char: '%s'" tken)

let lexer buf = Sedlexing.with_tokenizer token buf
