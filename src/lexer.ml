open Sedlexing

type token = Menhir_parser.token

open Menhir_parser

exception Error of Lexing.position * string

(* whitespaces *)

let blank = [%sedlex.regexp? '\n' | '\r' | "\r\n" | ' ' | '\t']

(* integers *)

let sign = [%sedlex.regexp? '+' | '-']

let digit = [%sedlex.regexp? '0' .. '9']

let number = [%sedlex.regexp? '0' .. '9' | '1' .. '9', Plus digit]

let int = [%sedlex.regexp? Opt sign, number]

(* identifiers *)

let id = [%sedlex.regexp? 'a' .. 'z', Star ('a' .. 'z' | '_')]

(* other literals *)

let constructor = [%sedlex.regexp? 'A' .. 'Z', Star ('a' .. 'z' | '_')]

let rec token buf =
  match%sedlex buf with
  | blank -> token buf
  | "else" -> ELSE
  | "then" -> THEN
  | "fun" -> FUN
  | "end" -> END
  | "let" -> LET
  | "in" -> IN
  | "if" -> IF
  | "->" -> ARROW
  | "(" -> LPAR
  | ")" -> RPAR
  | "=" -> EQUAL
  | id ->
    let id = Utf8.lexeme buf in
    IDENT id
  | int -> INT (Utf8.lexeme buf)
  | constructor ->
    let c = Utf8.lexeme buf in
    CONSTRUCTOR c
  | eof -> EOF
  | _ ->
    let position = fst @@ lexing_positions buf in
    let tken = Utf8.lexeme buf in
    raise @@ Error (position, Format.sprintf "unexpected char: '%s'" tken)

let lexer buf = Sedlexing.with_tokenizer token buf
