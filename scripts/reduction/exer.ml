type 'a ltree = 
  | Var of char
  | Appl of 'a ltree * 'a ltree
  | Abst of 'a ltree * 'a ltree
  

let rec print_ltree tree = 
  match tree with
    Var (a) -> String.make 1 a
  | Appl (lt,rt) -> "(" ^ (print_ltree lt) ^ ")(" ^ (print_ltree rt) ^ ")"
  | Abst (var, rt) -> "@" ^ (print_ltree var) ^ "." ^ (print_ltree rt)

  
let test_ltree = 
  Abst( Var('a'), Appl( Var('f'), Appl (Var('f'), Var('x'))))
