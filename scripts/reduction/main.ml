let debug = true
(* 整数のリストを文字列に変換する関数 *)

let print_string_list lst =
  let rec aux = function
    | [] -> ""
    | head :: tail ->
      head ^ ";" ^ (aux tail)
  in
  "[" ^ (aux lst) ^ "]"

type 'a ltree = 
  | Var of char
  | Appl of 'a ltree * 'a ltree
  | Abst of 'a ltree * 'a ltree
  

let rec print_ltree tree = 
  match tree with
    Var (a) -> String.make 1 a
  | Appl (lt, rt) -> "(" ^ (print_ltree lt) ^ (print_ltree rt) ^ ")"
  | Abst (var, rt) -> "(@" ^ (print_ltree var) ^ "." ^ (print_ltree rt) ^ ")"

  
  
let rec sbst_free_var ltree v tr = 
    match ltree with
      Var (x) -> (if Var(x) = v
          then ltree
          else Var(x)
        )
      | Appl (lt, rt) -> Appl (sbst_free_var lt v tr, sbst_free_var rt v tr)
      | Abst (ab, rt) -> if ab = v
          then Abst (ab, rt)
          else Abst (v, sbst_free_var rt v rt)
          
let rec sbst_all_var ltree va vb = 
    match ltree with
      Var (x) -> (if Var(x) = va
          then vb
          else Var(x)
        )
      | Appl (lt, rt) -> Appl (sbst_all_var lt va vb, sbst_all_var rt va vb)
      | Abst (v, rt) -> Abst (sbst_all_var v va vb, sbst_all_var rt va vb)

let rec check_fv tr v = 
  if tr = v then true else
  match tr with
  Var _ -> false
  | Appl(lt, rt) -> (check_fv lt v) || (check_fv rt v)
  | Abst(ab, rt) -> if ab = v then false else check_fv rt v

let rec fv_list tr = 
  match tr with
  Var x -> [Var(x)]
  | Appl(lt, rt) -> (fv_list lt) @ (fv_list rt)
  | Abst(ab, rt) -> fv_list rt
  
let rec print_fv_list tr = 
  match tr with
  Var x -> (String.make 1 x) ^ ";"
  | Appl(lt, rt) -> (print_fv_list lt) ^ (print_fv_list rt)
  | Abst(ab, rt) -> print_fv_list rt


let char_set = ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'; 'k'; 'l'; 'm']
  
let rec get_a_var char_set ignore = 
  match char_set with
  [] -> let _ = print_endline "char_set is empty" in Var('$')
  | x::xs ->
    if (List.mem (Var x) ignore)
      then (get_a_var xs ignore)
      else  Var x


let rec sbst_with_check trm v trn = 
  let () = Printf.printf "trm = %s, v = %s, trn = %s\n" (print_ltree trm) (print_ltree v) (print_ltree trn) in
  match trm with
  Var x -> 
    let () = Printf.printf "trm is Value!!\n" in
    if ((Var x) = v)
    then 
      let () = Printf.printf "Substitute!!\n" in
      trn
    else (Var x)
  | Abst(abv, m_1) -> 
    let () = Printf.printf "trm is Abstruct, abv = %s\n" (print_ltree abv);
        Printf.printf "fv of trm: %s, fv of trm %s\n" (print_fv_list trm)  (print_fv_list trn) in 
    if (abv = v) then
      Abst(abv, m_1)
    else if (check_fv trn abv && check_fv m_1 v)
      then 
        let () = Printf.printf "This is abst worst pattern!!\n" in
        let nv = get_a_var char_set ((fv_list m_1) @ (fv_list trn)) in
        sbst_with_check (Abst(nv, sbst_with_check m_1 abv nv)) v trn
      else
        Abst(abv, sbst_with_check m_1 v trn)
  | Appl(lt, rt) -> Appl(sbst_with_check lt v trn, sbst_with_check rt v trn)


let top_redu_form lt = 
  match lt with
    Appl(Abst(_, _), _) -> true
    | _ -> true
    

let rec redu_point_search tr pr_code = 
  match tr with
  Var('f') -> [pr_code]
  | _ ->
  match tr with
  Var(_) -> []
  | Abst(ltr, rtr) -> redu_point_search rtr pr_code
  | Appl(ltr, rtr) -> 
    redu_point_search ltr (pr_code ^ "0") @ redu_point_search rtr (pr_code ^ "1")

let test_ltree = 
  Abst( Var('f'), Abst( Var('x'), Appl(Appl( Var('f'), Appl (Var('f'), Var('x'))), Var('c'))))

let tr2 = 
  Abst( Var('f'), Abst( Var('x'), Appl( Var('f'), Appl (Var('f'), Var('x')))))

let tr_pl= 
  Abst( Var('m'), 
    Abst( Var('n'), 
      Abst( Var('f'), 
        Abst( Var('x'), 
          Appl(
            Appl( Var('m'), Var('f')),
            Appl( Appl(Var('n'), Var('f')), Var('x'))
          )
        )
      )
    )
  )
  

let _ = print_string_list (redu_point_search tr_pl "0")

let _ = Printf.printf "test1\n%s\n" (print_ltree test_ltree)

let _ = Printf.printf "test2\n%s\n" (print_ltree tr2)

let _ = Printf.printf "tr_pl\n%s\n" (print_ltree tr_pl)

let tr_true = Abst(Var 'x', Appl(Var 'x', Var 'y'))
let tr_false = Abst(Var 'y', Appl(Var 'x', Var 'y'))

let tr_a = Appl(Var 'x', Var 'x')
let tr_b = Abst(Var 'x', Var 'y')

let _ = Printf.printf "%s\n" (print_ltree (sbst_with_check tr_true (Var 'y') tr_a)) 
let tr_add = (Appl(tr_pl, tr2))

