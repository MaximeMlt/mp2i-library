(*tri_fusion*)
let rec split = function
    | [] -> [], []
    | [e] -> [e], []
    | e1::e2::q -> let q1, q2 = split q in e1::q1, e2::q2

let rec fusion l1 l2 = match l1, l2 with
    | [], _ -> l2
    | _, [] -> l1
    | e1::q1, e2::q2 when e1 < e2 -> e1::fusion q1 l2
    | e1::q1, e2::q2 -> e2::fusion l1 q2
    
let rec tri = function
    | [] -> []
    | [e] -> [e] 
    | l -> let l1, l2 = split l in fusion (tri l1) (tri l2);;

(*tri par insertion*)

(*tri rapide*)
