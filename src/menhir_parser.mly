%token LPAR RPAR EQUAL IN FUN LET EOF ARROW IF THEN ELSE END
%token <String.t> CONSTRUCTOR
%token <String.t> INT
%token <String.t> IDENT

%start <Ast.program> program

%%

let literal :=
  | i = INT; {Ast.Int (int_of_string i)}
  | b = CONSTRUCTOR;
  {
    match b with
    | "True" -> Ast.Bool true
    | "False" -> Ast.Bool false
    | "Unit" -> Ast.Unit
    | _s -> assert false
  }

let expression :=
  | ~ = literal; <Ast.Literal>
  | LPAR; ~ = expression; RPAR; <>
  | ~ = IDENT; <Ast.Var>
  | LET; id = IDENT; EQUAL; exp1 = expression ; IN; exp2 = expression;
    { Ast.Bind (id, exp1, exp2) }
  | FUN; id = IDENT; ARROW; ~ = expression;
    { Ast.Abstract (id, expression) }
  | LPAR; exp1 = expression; RPAR; LPAR; exp2 = expression; RPAR;
    { Ast.Apply (exp1, exp2) }
  | IF; exp1 = expression; THEN; exp2 = expression; ELSE; exp3 = expression; END;
    { Ast.If (exp1, exp2, exp3) }

let program :=
    | ~ = expression; EOF; <>
