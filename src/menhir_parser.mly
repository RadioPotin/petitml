%token LPAR RPAR EQUAL IN FUN LET EOF ARROW IF THEN ELSE END
%token <String.t> CONSTRUCTOR
%token <String.t> INT
%token <String.t> IDENT

%right LET
%right IN
%left FUN
%left ARROW
%left LPAR
%left IF
%left CONSTRUCTOR
%left INT
%left IDENT
%left APPLY_PREC

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
    <Ast.Bind>
  | FUN; id = IDENT; ARROW; ~ = expression;
    <Ast.Abstract>
  | exp1 = expression; exp2 = expression;
  %prec APPLY_PREC { Ast.Apply (exp1, exp2) }
  | IF; exp1 = expression; THEN; exp2 = expression; ELSE; exp3 = expression; END;
    <Ast.If>

let program :=
    | ~ = expression; EOF; <>
