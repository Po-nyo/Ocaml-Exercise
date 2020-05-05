
(* Module System *)

(* Interface for Integer Queue *)
module type IntQueue =
sig
  type t
  exception E
  val empty : t
  val isEmpty : t -> bool
  val enQueue : t -> int -> t
  val deQueue : t -> int * t
  val print : t -> unit
end

module IntQueue : IntQueue =
struct
  type t = int list
  exception E
  let empty = []
  let isEmpty q = q = []
  let enQueue q x = q @ [x]
  let deQueue q = 
    match q with
      |[] -> raise E
      |head::tail -> (head, tail)
  let rec print q =
    match q with
      |[] -> print_string "\n"; print_endline ""
      |head::tail -> print_int head; print_string " "; print tail
end

(*------------------------- test -------------------------*)

let q0 = IntQueue.empty;;
IntQueue.isEmpty q0;;

let q1 = IntQueue.enQueue q0 1;;
let q2 = IntQueue.enQueue q1 2;;

IntQueue.isEmpty q2;;

IntQueue.print q2;;
let (_, q3) = IntQueue.deQueue q2;;
let (_, q4) = IntQueue.deQueue q2;;

IntQueue.isEmpty q2;;
IntQueue.print q3;;
