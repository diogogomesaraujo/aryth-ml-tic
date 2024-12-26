%{
    open Ast
%}

%token <int> INT
%token LPAR
%token RPAR
%token SUM
%token SUB
%token MUL
%token DIV
%token EOF

%left SUM SUB
%left MUL DIV

%start <Ast.expr> prog
%%

prog:
  | e = expr; EOF { e }
  ;

expr:
  | i = INT { Int i }
  | e1 = expr; SUM; e2 = expr { Bop (Sum, e1, e2) }
  | e1 = expr; SUB; e2 = expr { Bop (Sub, e1, e2) }
  | e1 = expr; MUL; e2 = expr { Bop (Mul, e1, e2) }
  | e1 = expr; DIV; e2 = expr { Bop (Div, e1, e2) }
  | LPAR; e = expr; RPAR {e}
  ;
