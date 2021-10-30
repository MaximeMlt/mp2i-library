(* This file contains searching algorithms *)

(** [swap t i j] exchanges [t.(i)] and [t.(j)] *)
let swap t i j =
  let tmp = t.(i) in
  t.(i) <- t.(j);
  t.(j) <- tmp;;

(** [sort_bubble t] sorts array t using bubble sorting (O(n**2)) *)
let sort_bubble t = 
  for _ = 0 to Array.length t - 1 do
    for j = 0 to Array.length t - 2 do
      if t.(j) > t.(j + 1) then swap t j (j + 1)
      done
  done;;
  
(*Recherche d'élements dans une liste ( list.mem *)
let rec list_mem e l = match l with
    |[] -> false
    |e1::q -> e = e1 || list_mem e q
    
(*expo_rapide*)
let rec exporapide a n =
    if n = 0 then 1
    else 
        let b = exporapide a (n/2) in
        if n mod 2 = 0 then b*b
        else a*b*b

(*tester si une suite est croissante*)
let rec croissant l = match l with
    |[]-> true
    |_::[] -> true
    |e1::e2::q -> if e1 >= e2 then false else  croissant (e2::q)
    
(*dichotomie*)
let dicho e t = 
    let rec aux i j =
        if i>j then false
        else 
            let m = ((i+j)/2) in
            if e = t.(m)     then true
            else if e > t.(m)    then aux (m+1) j 
            else     aux i (m-1)
        in aux 0 (Array.length t -1)
