%{
    open Ast
%}

%token <float> FLOAT
%token <int> INT
%token <string> ID

%token LET
%token EQUALS

%token LPAR
%token RPAR

%token COMMA

%token SUM
%token SUB
%token MUL
%token DIV
%token POW
%token ROOT
%token LOG

%token EOF

%nonassoc COMMA
%nonassoc ELSE
%left SUM SUB
%left MUL DIV
%left POW ROOT
%left LOG

%start <Ast.expr> prog
%%

prog:
  | e = expr; EOF { e }
  ;

expr:
  | f = FLOAT { Float f }
  | i = INT { Int i }
  | e1 = expr; SUM; e2 = expr { Bop (Sum, e1, e2) }
  | e1 = expr; SUB; e2 = expr { Bop (Sub, e1, e2) }
  | e1 = expr; MUL; e2 = expr { Bop (Mul, e1, e2) }
  | e1 = expr; DIV; e2 = expr { Bop (Div, e1, e2) }
  | e1 = expr; POW; e2 = expr { Bop (Pow, e1, e2) }
  | e1 = expr; ROOT; e2 = expr { Bop (Root, e1, e2) }
  | LOG; LPAR; e1 = expr; COMMA; e2 = expr; RPAR { Bop (Log, e1, e2) }
  | LET; x = ID; EQUALS; e1 = expr; COMMA { Let (x, e1) }
  | LPAR; e = expr; RPAR {e}
  ;
