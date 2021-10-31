(*trichotomie*)
let tricho e t = 
    let rec aux i j =
        if j<i then false
        else
            let m1 = (2 * i + j + 1) / 3 in
            let m2 = (i + 2 * j + 2) / 3 in
            if e = t.(m2) then true
            else if e = t.(m1) then true
            else if e > t.(m2) then aux (m2 +1) j
            else if e < t.(m1) then aux i (m1 -1)
            else aux (m1+1) (m2-2)
    in aux 0 (Array.length t - 1)
