val L = [1,2,3,4,5,6,7];


(* test function*)
fun double x =
	if x = 0 then 0 else 
		if x > 0 then 2+double(x-1) 
		else (~2 + double(x+1));

fun square x =
	if x = 0 then 0 else
		if x > 0 then square(x-1)+double(x-1)+1
		else square(~x);

fun factorial 0 = 1
	| factorial x = if x < 0 then 1 else x*factorial(x-1);

fun isOdd 0 = false
	| isOdd 1 = true
	| isOdd x = 
	if x > 0 then isOdd(x-2) 
	else isOdd(x+2)

fun sum [] = 0
	| sum (x::xs) = x + sum xs

(* 1. thenAddOne *)
(* use high level function *)
(* double/square/factorial for test*)
fun thenAddOne (f:(int->int), x:int):int =
	f(x) + 1;

val a = thenAddOne (square,3)
	
(* 2. mapList *)
(* (('a -> 'b)* 'a list) -> 'b list *)
(* function(a->b) * a list -> b list*)
fun mapList (f, []) = []
	| mapList (f:('a ->'b), x::xs:'a list):'b list = 
	f(x)::mapList(f, xs);

(* 3. mapList' *)
(* ('a -> 'b)->('a list -> 'b list) *)
(* read in a single object function *)
(* export a function for the object list *)
fun mapList' f = let
	fun f' [] = []
		| f' (x::xs) = f(x)::f'(xs)
in
	f'
end
	
(* 4. findOdd *)
(* int list -> int option *)
(* returns a option *)
(* datatype 'a option = NONE | SOME of 'a *)

fun findOdd [] = NONE
	| findOdd (x::xs) =
	if isOdd x then SOME x 
	else findOdd xs

(* 5 *)
(* subsetSumOption : int list * int -> int list option *)
(* REQUIRES: true *)
(* ENSURES: subsetSumOption(l,s) == SOME l' iff l' is a sublist
 * of l that sums to s and NONE if no such l' exists *)

fun subsetSumOption (l : int list, 0 : int) : int list option = SOME []
	| subsetSumOption ([], _) = NONE
	| subsetSumOption (x::xs, s) = 
		case subsetSumOption(xs, s-x) of
			SOME(l) => SOME(x::l)
		| NONE => subsetSumOption(xs, s)

val b = subsetSumOption(L,8)


(* 6. exists & forall *)
(* really easy *)

fun exists pred [] = false
	| exists pred (x::xs) = (pred x) orelse exists pred xs;

fun forall pred [] = true
	| forall pred (x::xs) = (pred x) andalso forall pred xs

(* 7 *)
(*
 * treeFilter : ('a -> bool) -> 'a tree -> 'a option tree
 * REQUIRES: true
 * ENSURES: treeFilter p t evaluates to a tree T such that for each
 *          element x in t, if p x, then the value of the node at 
 *          that point in T is SOME(x), and if not, then the value of 
 *          the node is NONE.
 *)
(*
datatype 'a tree = Lf 						
	| Br of 'a * 'a tree * 'a tree*)

fun treeFilter pred (Lf) : 'a option tree = Lf
	| treeFilter pred (Br (root, left, right)) = 
		let
			val newroot = if pred root then SOME root else NONE
		in
			Br (newroot, treeFilter pred left, treeFilter pred right)
		end
