(******************************************)
(*             ZADANIE ORIGAMI            *)
(*        ROZWIAZANIE: MARCIN ZOLEK       *)
(*         RIWJU: PIOTR TRZASKOWSKI       *)
(******************************************)

type point = float * float;;
type kartka = point -> int;;

let epsilon = 1e-12;;

let signum x = 
    if x < -. epsilon then (-1)
    else if x > epsilon then 1
    else 0
    
let po_ktorej_stronie_prostej p1 p2 punkt = (* -1, gdy po prawej stronie prostej; 0, gdy na prostej; 1, gdy po lewej stronie prostej *)
    signum (((fst p2) -. (fst p1)) *. ((snd punkt) -. (snd p1)) -. ((fst punkt) -. (fst p1)) *. ((snd p2) -. (snd p1)))

let dlugosc p1 p2 = (* długość odcinka *)
    sqrt (((fst p1) -. (fst p2)) *. ((fst p1) -. (fst p2)) +. ((snd p1) -. (snd p2)) *. ((snd p1) -. (snd p2)))   
    
let odbij p1 p2 punkt = (* odbicie symetryczne punktu względem prostej *)
    let (x1, y1) = ((fst p2) -. (fst p1), (snd p2) -. (snd p1)) in
    let (x2, y2) = ((fst p1) -. (fst punkt), (snd p1) -. (snd punkt)) in
    let d = (x1 *. x2 +. y1 *. y2) /. (x1 *. x1 +. y1 *. y1) in
    ((fst punkt) +. 2.0 *. (x2 -. d *. x1), (snd punkt) +. 2.0 *. (y2 -. d *. y1)) 

let prostokat p1 p2 =
    function punkt -> 
        if fst punkt +. epsilon >= fst p1
        && fst punkt -. epsilon <= fst p2
        && snd punkt +. epsilon >= snd p1 
        && snd punkt -. epsilon <= snd p2 then 1
        else 0
        
let kolko s r =
    function punkt ->
        if dlugosc s punkt -. epsilon <= r then 1
        else 0

let zloz p1 p2 f = 
    function punkt ->
        let strona = po_ktorej_stronie_prostej p1 p2 punkt in
        match strona with
        | 0 -> f punkt 
        | 1 -> (f punkt) + (f (odbij p1 p2 punkt)) 
        | _ -> 0

let skladaj l f = 
    List.fold_left (fun a h -> zloz (fst h) (snd h) a) f l
