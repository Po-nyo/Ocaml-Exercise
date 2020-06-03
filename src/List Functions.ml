exception NotImplemented

let rec list_add l1 l2 = 
  match l1, l2 with
    |[], [] -> []
    |h::t, []
    |[], h::t -> h::(list_add t [])
    |h1::t1, h2::t2 -> (h1+h2)::(list_add t1 t2);;

list_add [1; 2] [3; 4; 5];;

let rec insert n l = 
  match l with
    |[] -> [n]
    |h::[] -> 
        if n > h then h::[n]
        else n::[h]
    |h1::h2::t ->
        if n >= h1 && n <= h2 then h1::n::h2::t
        else if n < h1 then n::l
        else h1::(insert n (h2::t));;

insert 3 [1; 2; 4; 5];;

let rec insort l = 
  match l with
    |[] -> []
    |h::t -> insert h (insort t);;

insort [3; 7; 5; 1; 2];;


let rec ltake l n = 
  match n with
    |0 -> []
    |n ->
        match l with
          |[] -> []
          |h::t -> h::(ltake t (n-1));;

ltake [3; 7; 5; 1; 2] 3;;
ltake [3; 7; 5; 1; 2] 7;;
ltake ["h"; "y"; "e"; "o"; "n"; "s"; "e"; "u"; "n"; "g"] 5;;


let rec lall f l = 
  match l with
    |[] -> true
    |h::t -> f h && lall f t;;

lall (fun x -> x > 0) [];;
lall (fun x -> x > 0) [1; 2; 3];;
lall (fun x -> x > 0) [1; -2; 3];;


let rec lmap f l = 
  match l with
    |[] -> []
    |h::t -> (f h)::(lmap f t);;

lmap (fun x -> x + 1) [1; 2; 3];;

let rec lfilter f l =
  match l with
    |[] -> []
    |h::t ->
        if f h then h::(lfilter f t)
        else lfilter f t;;

lfilter (fun x -> x > 2) [0; 1; 2; 3; 4; 5];;


let rec ltabulate n f =
  match n with
    |1 -> [f 0]
    |n -> (ltabulate (n-1) f)@[f (n-1)];;

ltabulate 4 (fun x -> x * x);;

let rec lrev l = 
  match l with
    |[] -> []
    |h::t -> (lrev t)@[h];;

lrev [1; 2; 3; 4];;

let rec lconcat l = 
  match l with
    |[] -> []
    |l1::l2 -> l1@(lconcat l2);;

lconcat [[1; 2; 3;]; [6; 5; 4]; [9]];;

let rec lfoldr f e l = 
  match l with
    |[] -> e
    |h::t -> f(h, (lfoldr f e t));;

lfoldr (fun (x, y) -> x - y) 0 [1; 2; 3];;

let rec lzip l1 l2 = 
  match l1, l2 with
    |[], _
    |_, [] -> []
    |h1::t1, h2::t2 -> (h1, h2)::(lzip t1 t2);;

lzip ["A"; "B"; "C"; "D"] [1; 2; 3; 4; 5; 6];;

let rec split l =
  match l with
    |[] -> ([], [])
    |h1::h2::t ->
        match split t with
          |(x, y) -> (h1::x, h2::y);;

split [1; 3; 5; 7; 9; 11];;

let rec cartprod l1 l2 = 
  match l1, l2 with
    |[n1], [n2] -> [(n1, n2)]
    |[n], h::t -> (n, h)::(cartprod l1 t)
    |h1::t1, h2::t2 -> (cartprod [h1] l2)@(cartprod t1 l2);;


cartprod [1; 2] [3; 4; 5];;
