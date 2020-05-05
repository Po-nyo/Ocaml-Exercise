

(* Operators only for Integer *)

2 + 3;;

2 * 3;;

13 / 5;;

13 mod 5;; 

(*-------------------------------------------------------------*)

(* Operators only for floating point numbers *)

2.0 +. 3.0;;

2.0 *. 3.0;;

13.0 /. 5.0;;

(*-------------------------------------------------------------*)

(* Operate Integer and Float *)

2 + int_of_float 3.0;;

2.0 +. float_of_int 3;;

(*-------------------------------------------------------------*)

(* Bool Type *)

let a = 3;;
let b = 4;;

a = b;;   (* true if a and b are equal		    *)

a <> b;;  (* true if a and b are not equal	    *)

a < b;;   (* true if a is less then b		    *)

a <= b;;  (* true if a is less then or equal to b   *)

true && false;;

true || false;;

(2>1) && (3>2);;

(*-------------------------------------------------------------*)

(* six primitive values *)

'c';;       (* char *)

"Hello";;   (* string *)

1;;	    (* int *)

1.0;;	    (* float *)

true;;	    (* boolean *)

();;	    (* unit *)
