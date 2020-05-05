
(* Define Binary Tree Inductively *)

type binTree = Leaf | Node of int * binTree * binTree;;

(* test *)
Leaf;;

Node(1, Leaf, Leaf);;

let tree1 = Node(1, Node(2, Leaf, Leaf), Node(3, Leaf, Leaf));;


(* Count Leaves *)
let rec leaves t =
  match t with 
    |Leaf -> 1
    |Node(_, t1, t2) -> leaves t1 + leaves t2;;

(* Count Nodes *)
let rec nodes t =
  match t with
    |Leaf -> 0
    |Node(_, t1, t2) -> nodes t1 + nodes t2 + 1;;


(* test *)
leaves tree1;;
nodes tree1;;
