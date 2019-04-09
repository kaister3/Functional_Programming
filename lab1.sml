(* sum : int list -> int *)
(* REQUIRES : true *)
(* ENSURES : sum(L) evaluates to the sum of the integers in L. *)
fun sum [] = 0
    | sum (x::L) = x + (sum L);

(* sum : int list -> int *)
(* REQUIRES : true *)
(* ENSURES : sum(L) evaluates to the product of the integers in L. *)
fun mult [] = 1
    | mult (x::L) = x * (mult L);

(* mult : int list list -> int *)
(* REQUIRES : true *)
(* ENSURES : mult(R) evaluates to 
the product of all the integers in 
the list of R *)

fun Mult [] = 1
    | Mult (r::R) = (mult r) * (Mult R);

(* mult' : int list * int => int *)
(* REQUIRES : true *)
(* ENSURES : mult' (L, a) evaluates the product of L * a *)

fun mult' ([], a) = a
    | mult' (x::L, a) = mult' (L, x * a)


(* double : int => int *)
(* REQUIRES : n >= 0 *)
(* ENSURES : double n evalutes to 2 * n *)

fun double (0 : int) : int = 0
    | double n = 2 + double (n - 1);

fun square (0 : int) : int = 0
    | square n = 1 + double (n - 1) + square (n - 1);

(* divisibleByThree : int => bool *)
(* REQUIRES : true *)
(*ENSURES : divisibleByThree evaluates to true 
if n is a multiple of 3 an to false otherwise *)

fun divisibleByThree (0 : int) : bool = true
    | divisibleByThree 1 = false
    | divisibleByThree 2 = false
    | divisibleByThree n = divisibleByThree (n - 3)

fun evenP (0 : int) : bool = true
    | evenP 1 = false
    | evenP n = evenP (n - 2);

fun oddP (0 : int) = false
    | oddP 1 = true
    | oddP n = oddP (n - 2);