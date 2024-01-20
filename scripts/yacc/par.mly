/* File parser.mly */
%{
  open Ast
%}

%token EOL
%token LAM 
%token <char> VAR
%token DOT
%token LPAREN RPAREN

%start main             /* the entry point */
%type <Ast.expr> main
%%
main:
    expr EOL                { $1 }
;
expr:
    VAR { Var($1) }
  | LPAREN LAM VAR DOT expr RPAREN { Lambda($3, $5) }
  | LPAREN expr expr RPAREN        { Apply($2, $3) }
;