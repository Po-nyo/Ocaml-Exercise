
(* Interpreter for Integer Exp *)

type exp = 
    |INT of int
    |MINUS of exp
    |ADD of exp * exp
    |MULT of exp * exp;;


let rec interp e =
  match e with
    |INT n -> n
    |MINUS e1 -> - (interp e1)
    |ADD(e1, e2) -> interp e1 + interp e2
    |MULT(e1, e2) -> interp e1 * interp e2;;

(* test *)

interp (INT 3);;

interp (MINUS (INT 3));;

interp (ADD (INT 1, INT 2));;


