{
  type token = 
    | LAM 
    | VAL of char
    | DOT
    | LPAREN
    | RPAREN
  exception Eof
}

rule token = parse
    [' ' '\t' '\n']     { token lexbuf }     (* skip blanks *)
  | '@'            { LAM }
  | '.'            { DOT }
  | '('            { LPAREN }
  | ')'            { RPAREN }
  | ['a'-'z'] as var { VAL(var) }
  | eof            { raise Eof}