(* hw3.ml *)

let rec sum n =
  if n = 1 then 1
  else n + sum(n - 1);;

let rec fac n = 
  if n = 1 then 1
  else n * fac(n - 1);;

let rec fib n = 
  if n <= 1 then 1
  else fib(n - 1) + fib(n - 2);;

let rec gcd a b = 
  if b = 0 then a
  else gcd b (a mod b);;

let rec max l =
  match l with 
    |[] -> 0
    |head::tail ->
        if head < max tail then max tail
        else head;;

type tree = Leaf of int | Node of int * tree * tree

let rec sum_tree t = 
  match t with 
    |Leaf n -> n
    |Node(n, t1, t2) -> n + sum_tree t1 + sum_tree t2;;

let rec depth t = 
  match t with
    |Leaf _ -> 1
    |Node(n, t1, t2) ->
        if depth(t1) < depth(t2) then depth(t2) + 1
        else depth(t2) + 1;;

let rec bin_search t n = 
  match t with
    |Leaf a -> 
        if a = n then true
        else false
    |Node(a, t1, t2) ->
        if a = n then true
        else (bin_search t1 n) || (bin_search t2 n);;

type exp =
      INT of int
    | ADD of exp * exp
    | SUB of exp * exp
    | MUL of exp * exp
    | DIV of exp * exp
    | MOD of exp * exp

let rec interp exp =
  match exp with
    |INT n -> n
    |ADD(exp1, exp2) -> interp exp1 + interp exp2
    |SUB(exp1, exp2) -> interp exp1 - interp exp2
    |MUL(exp1, exp2) -> interp exp1 * interp exp2
    |DIV(exp1, exp2) -> interp exp1 / interp exp2
    |MOD(exp1, exp2) -> interp exp1 mod interp exp2;;

type formula =
      True
    | False
    | Neg of formula
    | Or of formula * formula
    | And of formula * formula
    | Imply of formula * formula

let rec eval f = 
  match f with
    |True -> true
    |False -> false
    |Neg f' -> not(eval f')
    |Or(f1, f2) -> eval f1 || eval f2
    |And(f1, f2) -> eval f1 && eval f2
    |Imply(f1, f2) ->
        if eval f1 = true && eval f2 = false then false
        else true;;
