type 'a ltree = 
  | Var of char
  | Appl of 'a ltree * 'a ltree
  | Abst of 'a ltree * 'a ltree
  

let rec print_ltree tree = 
  match tree with
    Var (a) -> String.make 1 a
  | Appl (lt, rt) -> "(" ^ (print_ltree lt) ^ (print_ltree rt) ^ ")"
  | Abst (var, rt) -> "(@" ^ (print_ltree var) ^ "." ^ (print_ltree rt) ^ ")"

  
  
let rec sbst_var ltree a b = 
    match ltree with
      Var (x) -> (if x == a 
          then Var(b)
          else Var(x)
        )
      | Appl (lt, rt) -> Appl (sbst_var lt a b, sbst_var rt a b)
      | Abst (v, rt) -> if v == Var(a)
          then Abst (v, rt)
          else Abst (v, sbst_var rt a b)
        
let test_ltree = 
  Abst( Var('f'), Abst( Var('x'), Appl( Var('f'), Appl (Var('f'), Var('x')))))

let test_ltree2 = 
  Abst( Var('f'), Abst( Var('x'), Appl(Appl( Var('f'), Appl (Var('f'), Var('x'))), Var('c'))))

