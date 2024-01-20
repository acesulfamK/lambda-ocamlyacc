(* File calc.ml *)
(*
open Ast

let checker expr =
  match expr with
  Var(_) -> print_string "Var" ; None
  | Apply(_, _) -> print_string "Apply"; None
  | Lambda(_, _) -> print_string "Lambda"; None
*)

let _ =
  try
    print_string "Input: ";
    let lexbuf = Lexing.from_channel stdin in
    while true do
      let result = Par.main Lex.token lexbuf in
        print_string "start!!";
        print_newline(); flush stdout;
        print_string "end!!";
        result
    done
  with Lex.Eof ->
    exit 0
    