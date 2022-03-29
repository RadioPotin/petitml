%token LPAR RPAR EQUAL IN FUN LET EOF UNIT ARROW IF THEN ELSE END
(*
   UNDERSCORE SEMICOL LT GT LE GE AND OR XOR
*)
%token <String.t> BOOL
%token <String.t> INT
%token <String.t> IDENT

%start <Ast.program> program

%%

let literal :=
  | i = INT; {Ast.Int (int_of_string i)}
  | b = BOOL;
  {
    match b with
    | "True" -> Ast.Bool true
    | "False" -> Ast.Bool false
    | _s -> assert false
  }
    | UNIT; {Ast.Unit}

let expressions :=
  | ~ = literal; <Ast.Literal>
  | ~ = IDENT; <Ast.Var>
  | LET; id = IDENT; EQUAL; exp1 = expressions ; IN; exp2 = expressions;
    { Ast.Bind (id, exp1, exp2) }
  | LPAR; FUN; id = IDENT; ARROW; exp = expressions; RPAR;
    { Ast.Abstract (id, exp) }
  | LPAR; exp1 = expressions; RPAR; LPAR; exp2 = expressions; RPAR;
    { Ast.Apply (exp1, exp2) }
  | IF; exp1 = expressions; THEN; exp2 = expressions; ELSE; exp3 = expressions; END;
    { Ast.If (exp1, exp2, exp3) }

let program :=
    | ~ = nonempty_list(expressions); EOF; <>
