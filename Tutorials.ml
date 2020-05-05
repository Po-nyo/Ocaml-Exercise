
(* Pattern Matching *)

let isABC c = 
  match c with
    |'A' -> true
    |'B' -> true
    |'C' -> true
    |_ -> false;;

(*----------------------------------------------------------------*)

(* Type Annotation *)

let isPositive x =
  if x > 0 then true
  else false;;

let sum_1 (test : int -> bool) (x : int) (y : int) : int =
  (if test x then x else 0) + (if test y then y else 0);;

sum_1 isPositive 3 (-1);;

(*----------------------------------------------------------------*)

(* Polymorphic Types *)
(* x can be values of any Type *)
(* This type is called polymorphic and 'a is type variable *)
let id x = x;; 

(*----------------------------------------------------------------*)

(* Tuples *)
(* Each Element can be value of variety types *)
let x = (1, "one");;
let y = (2, "two", true);;

let (a, b, c) = y;;

let first_element t =
  match t with
    |(x, _, _) -> x;;

first_element y;;

(*----------------------------------------------------------------*)

(* Lists *)

let list = [1; 2; 3];;
let list2 = [4; 5; 6];;

1 :: list;;
list2 @ list;;
list @ [1];;

let isnil l = 
  match l with
    |[] -> true
    |_ -> false;;

isnil [];;
isnil list;;

(* recursive functions must had 'rec' keyword after 'let' *)

let rec length l =
  match l with
    |[] -> 0
    |head::tail -> 1 + length tail;;

length list;;

(*----------------------------------------------------------------*)

(* Datatypes *)
(* Enumerate elements *)

type days = MON | TUE | WED | THU | FRI | SAT | SUN ;;

let nextday d =
  match d with
    |MON -> TUE
    |TUE -> WED
    |WED -> THU
    |THU -> FRI
    |FRI -> SAT
    |SAT -> SUN
    |SUN -> MON;;

nextday SAT;;

(* Constructors may have arguments *)

type shape = Rect of int * int | Circle of int;;

let rect = Rect(2, 3);;
let circle = Circle 5;;

let area s = 
  match s with
    |Rect(x, y) -> x*y
    |Circle x -> x*x*3;;

area rect;;
area circle;;

(* inductive data types *)

type myList = Nil | List of int * myList;;
Nil;;
List(1, Nil);;
List(2, List(1, Nil));;

let rec myLength l = 
  match l with
    |Nil -> 0
    |List(_, sub) -> 1 + myLength sub;;

myLength (List(1, List(2, List(3, List(4, Nil)))));;

(*----------------------------------------------------------------*)
(* Exceptions *)

let div a b =
  try
    a / b
  with Division_by_zero -> 0;;

div 4 0;;

exception Problem;;

let div2 a b =
  if b = 0 then raise Problem
  else a / b;;

div2 4 0;;


