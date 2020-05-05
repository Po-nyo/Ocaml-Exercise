
(* Natural Number define *)

type nat = Zero | Succ of nat;;

(* test *)
Zero;;

Succ Zero;;

Succ (Succ Zero);;

(* verify Zero *)
let isZero n =
  match n with 
    |Zero -> true
    |_ -> false;;

(* test *)
isZero Zero;;

isZero (Succ Zero);;


(* transfer nat to int *)
let rec trans n =
  match n with
    |Zero -> 0
    |Succ n' -> 1 + trans n';;

trans Zero;;

trans (Succ(Succ (Succ Zero)));;


