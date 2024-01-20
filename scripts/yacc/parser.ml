type token =
  | EOL
  | LAM
  | VAR of (char)
  | DOT
  | LPAREN
  | RPAREN

open Parsing;;
let _ = parse_error;;
# 3 "parser.mly"
  open Ast
# 14 "parser.ml"
let yytransl_const = [|
  257 (* EOL *);
  258 (* LAM *);
  260 (* DOT *);
  261 (* LPAREN *);
  262 (* RPAREN *);
    0|]

let yytransl_block = [|
  259 (* VAR *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\000\000"

let yylen = "\002\000\
\002\000\001\000\006\000\004\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\002\000\000\000\005\000\000\000\000\000\000\000\
\001\000\000\000\000\000\000\000\004\000\000\000\003\000"

let yydgoto = "\002\000\
\005\000\006\000"

let yysindex = "\005\000\
\004\255\000\000\000\000\000\255\000\000\009\255\254\254\004\255\
\000\000\007\255\006\255\004\255\000\000\008\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\252\255"

let yytablesize = 14
let yytable = "\008\000\
\010\000\007\000\003\000\011\000\004\000\001\000\003\000\014\000\
\004\000\009\000\012\000\013\000\000\000\015\000"

let yycheck = "\004\000\
\003\001\002\001\003\001\008\000\005\001\001\000\003\001\012\000\
\005\001\001\001\004\001\006\001\255\255\006\001"

let yynames_const = "\
  EOL\000\
  LAM\000\
  DOT\000\
  LPAREN\000\
  RPAREN\000\
  "

let yynames_block = "\
  VAR\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 16 "parser.mly"
                            ( _1 )
# 79 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : char) in
    Obj.repr(
# 19 "parser.mly"
        ( Var(_1) )
# 86 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : char) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 20 "parser.mly"
                                   ( Lambda(_3, _5) )
# 94 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 21 "parser.mly"
                                   ( Apply(_2, _3) )
# 102 "parser.ml"
               : 'expr))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.expr)
