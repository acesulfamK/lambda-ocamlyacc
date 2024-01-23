{
  open Par
  exception Eof
}

rule token = parse
    [' ' '\t' '\n']     { token lexbuf }     (* skip blanks *)
  | '@'            { LAM }
  | '.'            { DOT }
  | '('            { LPAREN }
  | ')'            { RPAREN }
  | ['a'-'z'] as var { VAR(var) }
  | eof            { raise Eof }
  