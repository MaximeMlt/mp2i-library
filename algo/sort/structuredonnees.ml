(*file*)
type 'a file={tab : 'a array ; mutable deb : int ; mutable fin : int ; mutable vide : bool}

let f = {tab = [|12;8;7;2;5;3;1;16;3;4;0;1|]; deb = 9 ; fin = 2 ; vide = false}

let ajoute f x =
    if f.deb = f.fin && not f.vide then failwith "Liste vide"
    else
        f.tab.(f.fin)<-x;
        f.fin<- (f.fin +1) mod Array.length f.tab;
        f.vide <- false
(*complexité O(1)*)

let retire f =
    if f.deb = f.fin && not f.vide then failwith "Liste vide"
    else
        f.fin <-(f.fin -1) mod Array.length f.tab;
        if f.deb = f.fin then f.vide <- true
(*complexité O(1)*)
