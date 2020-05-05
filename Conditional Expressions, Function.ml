
(* Conditional Expressions *)

(* if be then e1 else e2		  *)
(* be must be a boolean expression.       *)
(* types of e1 and e2 must be equivalent  *)

if 2>1 then 0
else 1;;

(*------------------------------------------------------*)

(* create a global variable *)

let x = 7 + 3;;

let y = x + x;;

let a = 1 in
let b = a + a in
let c = b * 2 in
  c + c;;

(*------------------------------------------------------*)

(* create function *)

let square x = x * x;;
square 5;;

let isNegative x =
  if x < 0 then true
  else false;;

isNegative (-3);;
isNegative 3;;

let area w h =
  w * h;;

area 4 5;;

(*------------------------------------------------------*)

(* nameless function *)

fun x -> x * x;;

(fun x -> x * x) 2;;

let square2 = fun x -> x * x;;
square2 5;;

(*------------------------------------------------------*)

(* functions are first-class in Ocaml *)

let isPositive x =
  if x > 0 then true
  else false;;

let sum_1 test x y =
  (if test x then x else 0)
  + (if test y then y else 0);;

sum_1 isPositive (-3) 4;;
