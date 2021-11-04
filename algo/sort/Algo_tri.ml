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


(*tri rapide*)

let rec concat l1 l2 = match l1 with
    |[] -> l2
    |e::q ->  e::concat q l2;;
    
let rec partition l p = match l with
    |[]->[],[]
    |e::q -> let l1, l2 = partition q p in
        if  e < p then e::l1, l2
        else l1, e::l2

let rec trirapide l = match l with
    |[]->[]
    |e::q -> let l1, l2 = partition q e in 
            concat (trirapide l1) (e::trirapide l2)


(*tri par insertion*)
let rec insere l e = match l with
    |[]-> [e]
    |e1::q -> if e>e1 then e1::insere q e  
        else e::l

let rec trinsertion l = match l with
    |[]->[]
    |e::q-> insere (trinsertion q) e 
