type token =
  | EOL
  | LAM
  | VAR of (char)
  | DOT
  | LPAREN
  | RPAREN

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.expr
