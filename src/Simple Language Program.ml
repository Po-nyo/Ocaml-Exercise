(* Program *)

type program = exp
and exp =
    |CONST of int
    |VAR of var
    |ADD of exp * exp
    |SUB of exp * exp
    |ISZERO of exp
    |IF of exp * exp * exp
    |LET of var * exp *exp
and var = string;;

(* test *)

let pgm1 = CONST 1
let pgm2 = ADD (ADD (CONST 5, CONST 2), SUB (CONST 3, CONST 10)) (* (5+2)+(3-10) *)
let pgm3 = IF (ISZERO (CONST 1), CONST 3, CONST 4) (* if iszero 1 then 3 else 4 *)
let pgm4 = LET ("x", CONST 5, SUB(VAR "x", CONST 3)) (* let x = 5 in x - 3 *)

(*
let x = 7 in 
let y = 2 in
let y =
let x = x - 1 in x - y
in
(x - 8) - y  // -5
*)

let pgm5 =
  LET ("x", CONST 7,
       LET ("y", CONST 2,
            LET ("y", LET ("x", SUB(VAR "x", CONST 1),  SUB (VAR "x", VAR "y")),
                 SUB (SUB (VAR "x", CONST 8), VAR "y"))))

let pgm6 =
  LET ("x", CONST 1,
       LET ("y", CONST 2,
            ADD (VAR "x", VAR "y")))

(* value *)

type value = Int of int | Bool of bool;;

(* environment  interface *)

module type Env =
sig
  type t
  exception Not_found
  val empty : t
  val lookup : var -> t -> value
  val extend : var -> value -> t -> t
end

(* environment with function *)

module Env : Env =
struct
  type t = var -> value
  exception Not_found
  let empty = fun x -> prerr_endline x; raise Not_found
  let lookup x e = e x
  let extend x v e = fun y -> if x = y then v else lookup y e
end

(* environment with tuple list *)

module Env2 : Env =
struct
  type t = (var * value) list
  exception Not_found
  let empty = []
  let rec lookup x e =
    match e with
      | [] -> raise Not_found
      | (y,v)::tl -> if x = y then v else lookup x tl
  let extend x v e = (x,v)::e
end

(* eval_binary_operation(for ADD, SUB) and evaluate *)

let rec eval_bop =
  fun op e1 e2 env ->
    let v1 = eval e1 env in
    let v2 = eval e2 env in
      (match v1, v2 with
        |Int n1, Int n2 -> Int (op n1 n2)
        |_ -> raise (Failure "Type Error: non-numeric values"))

and eval = 
  fun exp env ->
    match exp with
      |CONST n -> Int n
      |VAR x -> Env.lookup x env
      |ADD(e1, e2) -> eval_bop (+) e1 e2 env
      |SUB(e1, e2) -> eval_bop (-) e1 e2 env
      |ISZERO e ->
          (match eval e env with
            |Int n -> if n = 0 then Bool true else Bool false
            |_ -> raise (Failure "Type Error: subexpression of iszero must be Int type"))
      |IF(e1, e2, e3) ->
          (match eval e1 env with
            |Bool true -> eval e2 env
            |Bool false -> eval e3 env
            |_ -> raise (Failure "Type Error: condition must be Bool type"))
      |LET(x, e1, e2) ->
          let v1 = eval e1 env in
            eval e2 (Env.extend x v1 env)

let run = fun pgm -> eval pgm Env.empty;;

(* test *)
run pgm1;;
run pgm2;;
run pgm3;;
run pgm4;;
run pgm5;;
run pgm6;;
