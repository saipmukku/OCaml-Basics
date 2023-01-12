(***********************************)
(* Part 1: Non-Recursive Functions *)
(***********************************)

let rev_tup tup = match tup with
    | (a, b, c) -> (c, b, a);;

let is_odd x = if x mod 2 == 0 then false else true;;

let area x y = match x with
    | (a, b) -> match y with
    | (c, d) -> abs(a - c) * abs(b - d);; 

let volume x y = match x with
    | (a, b, c) -> match y with
    | (d, e, f) -> abs(a - d) * abs(b - e) * abs(c - f);;

(*******************************)
(* Part 2: Recursive Functions *)
(*******************************)

let rec fibonacci n = if n = 1 then 1 else if n = 0 then 0 else fibonacci(n - 1) + fibonacci(n - 2);;

let rec pow x y = if y = 0 then 1 else x * pow x (y - 1);;

let rec log x y = if (x > y) then 0 else 1 + log(x) (y / x);;

let rec gcf x y = if y = 0 then x else gcf(y)(x mod y);;

let rec is_prime x = if x < 0 then false else if x = 1 then false else if x = 2 then true else if x = 3 then true else if x = 5 then true else if x mod 2 = 0 then false else if gcf 3 x = 1 && gcf 5 x = 1 && gcf 7 x == 1 then true else false;;

(*****************)
(* Part 3: Lists *)
(*****************)

let rec get idx lst = match lst with
    | [] -> failwith "Out of bounds"
    | h::t -> if idx = 0 then h else get (idx - 1) t;;

let rec size lst = match lst with
    | [] -> 0;
    | h::t -> 1 + size t;;

let larger lst1 lst2 = if size lst1 > size lst2 then lst1 else if size lst1 < size lst2
    then lst2 else [];;

let rec add lst addThis = match lst with
    | [] -> addThis
    | h::t -> h :: add t addThis;;

let rec reverse lst = match lst with
    | [] -> []
    | h::t -> add (reverse t) [h];;

let rec combine lst1 lst2 = match lst2 with
    | [] -> lst1
    | h::t -> combine (add (lst1) ([h])) (t);;

let rec merge lst1 lst2 = match lst1 with
    | [] -> lst2
    | (h::t) -> match lst2 with
    | [] -> lst1
    | (secondHead::secondTail) -> if h < secondHead then h :: (merge t lst2) else secondHead :: (merge lst1 secondTail);;

let rec rotate shift lst = match lst with
    | [] -> []
    | h :: t -> if shift = 0 then lst else rotate (shift - 1) (add t [h]);;

let rec is_palindrome lst = if lst = reverse lst then true else false;;