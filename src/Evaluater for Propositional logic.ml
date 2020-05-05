
(* Evaluater for Propositional logic *)

type formula  = True | False
              |Neg of formula
              |And of formula * formula
              |Or of formula * formula
              |Imply of formula * formula;;

let rec eval f =
  match f with
    |True -> true
    |False -> false
    |Neg f' -> not (eval f')
    |And(f1, f2) -> eval f1 && eval f2
    |Or(f1, f2) -> eval f1 || eval f2
    |Imply(f1, f2) ->
        if (eval f1) = true && (eval f2) = false then false
        else true;;

(* test *)
eval True;;

eval False;;

eval (Neg True);;

eval (And (True, False));;

eval (Or (True, False));;

eval (Imply (True, True));;
