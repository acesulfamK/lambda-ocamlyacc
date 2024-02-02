let debug = false
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
  [] -> let _ = print_endline "Error: char_set is empty" in Var('$')
  | x::xs ->
    if (List.mem (Var x) ignore)
      then (get_a_var xs ignore)
      else  Var x


let rec sbst_with_check trm v trn = 
  let () = if debug then Printf.printf "sbst log: trm = %s, v = %s, trn = %s\n" (print_ltree trm) (print_ltree v) (print_ltree trn) in
  match trm with
  Var x -> 
    let () = if debug then Printf.printf "sbst log: trm is Value!!\n" in
    if ((Var x) = v)
    then 
      let () = if debug then Printf.printf "sbst log: Substitute!!\n" in
      trn
    else (Var x)
  | Abst(abv, m_1) ->
    let () = if debug then Printf.printf "sbst log: trm is Abstruct, abv = %s\n" (print_ltree abv);
        if debug then Printf.printf "sbst log: fv of trm: %s, fv of trm %s\n" (print_fv_list trm)  (print_fv_list trn) in 
    if (abv = v) then
      Abst(abv, m_1)
    else if (check_fv trn abv && check_fv m_1 v)
      then 
        let () = if debug then Printf.printf "sbst log: This is the abst worst pattern!!\n" in
        let nv = get_a_var char_set ((fv_list m_1) @ (fv_list trn)) in
        sbst_with_check (Abst(nv, sbst_with_check m_1 abv nv)) v trn
      else
        Abst(abv, sbst_with_check m_1 v trn)
  | Appl(lt, rt) -> Appl(sbst_with_check lt v trn, sbst_with_check rt v trn)

let top_redu_form lt = 
  match lt with
    Appl(Abst(_, _), _) -> true
    | _ -> false
let rec top_redu tr = 
  let () = if debug then Printf.printf "top_redu log: tr = %s\n" (print_ltree tr) in
  match tr with
    Appl(Abst(y, m), n) -> sbst_with_check m y n
    | Abst(y, m) -> Abst(y, top_redu m)
    | _ -> let () = Printf.printf "Error: something went wrong in top_redu\n" in Var('$')

let rec redu_from_code tr code = 
  let () = if debug then Printf.printf "redu_from_code log: tr = %s, code = %s\n" (print_ltree tr) code in
  match code with
  "0" -> top_redu tr
  | "1" -> top_redu tr
  | _ -> 
    match tr with
    Var x -> let () = Printf.printf "Error: invalid code\n" in Var('$')
    | Abst(v,rt) -> 
      Abst(v, redu_from_code rt code)
    | Appl(lt,rt) -> 
      let c = String.get code 1 in
      let len = String.length code in
      let sub = String.sub code 1 (len - 1) in
      match c with
      | '0' -> Appl(redu_from_code lt sub, rt)
      | '1' -> Appl(lt, redu_from_code rt sub)
      | _ -> let () = Printf.printf "Error: invalid c\n" in Var('$')

  
  

let l_redu tr = 
  let rec redu_point_search tr pr_code = 
    match tr with
    Appl(Abst(_, _), _) -> [pr_code]
    |Var(_) -> []
    | Abst(ltr, rtr) -> redu_point_search rtr pr_code
    | Appl(ltr, rtr) -> 
      redu_point_search ltr (pr_code ^ "0") @ redu_point_search rtr (pr_code ^ "1")
  in let redu_list = redu_point_search tr "0" in
  let redu_codes = (List.sort String.compare redu_list) in 
  match redu_codes with
  [] -> let () = Printf.printf "This formula cannot reduce!! \n" in tr
  | h::_ -> 
      let () = if debug then Printf.printf "l_redu log: Tree is %s. Code is %s\n" (print_ltree tr) h in
  redu_from_code tr h



let tr2 = 
  Abst( Var('f'), Abst( Var('x'), Appl( Var('f'), Appl (Var('f'), Var('x')))))

let tr3 = 
  Abst( Var('f'), Abst( Var('x'),Appl( Var('f'), Appl( Var('f'), Appl (Var('f'), Var('x'))))))

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
  
let tr_add = Appl(Appl(tr_pl, tr2), tr3)
let tr_true = Abst(Var 'x', Appl(Var 'x', Var 'y'))

let tr_false = Abst(Var 'y', Appl(Var 'x', Var 'y'))

let omega = Abst(Var('x'), Appl(Var('x'), Var('x')))


let _ = Printf.printf "2 = %s\n" (print_ltree tr2) 

let _ = Printf.printf "3 =  %s\n" (print_ltree tr3)

let _ = Printf.printf "plus = %s\n" (print_ltree tr_pl)

let _ = Printf.printf "2 + 3 = %s\n\n" (print_ltree tr_add)

let _ = Printf.printf "Reduction Start!!\n"

let counter = [0;1;2;3;4;5;6;7]

let process_and_print lam c=
  Printf.printf "Reduction %d: lam = %s\n" c (print_ltree lam);
  l_redu lam

let _ = List.fold_left (fun lam c -> 
  process_and_print lam c
  ) tr_add  counter
  
(*
let rec omega_redu tr n = let _ = Printf.printf "Times %d: %s\n" (n+1) (print_ltree tr) in 
  omega_redu (l_redu tr) (n+1)

let ans = omega_redu (Appl(omega, omega)) 1
*)
