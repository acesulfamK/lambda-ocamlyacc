  type expr = 
    | Var of char
    | Lambda of char * expr
    | Apply of expr * expr