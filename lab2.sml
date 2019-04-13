(*1*)

fun reverse [a] = [a]
	| reverse [] = []
	| reverse (x::l) = reverse(l)@[x];

fun reverse' [] = []
	| reverse' (x::l) = let
		fun f ([], x) = x
			| f (y::L, x) = f (L, y::x)
	in
		f (x::l, [])
	end

(*2*)
(* interleave : int list * int list => int list *)

fun interleave (a, []) = a
	| interleave ([], a) = a
	| interleave (x::xs, y::ys) = x::y::interleave(xs, ys);

(*3*)
(* list to tree : int list => tree *)
datatype 'a tree = Lf
	| Br of 'a * 'a tree * 'a tree; 

fun split (l : int list) : (int list * int * int list) = 
	case l of
		[] => raise Fail "split should never be called on an empty list"
		| _ =>
		  let
		  	val midlen = (length l) div 2
		  	val left = (List.take (l, midlen))
		  	val root :: right = (List.drop (l,midlen))  
		  in
		  	(left, root, right)
		  end

fun list2tree [] : int tree = Lf
	| list2tree l = 
		let
			val (left, root, right) = split l
		in
			Br (root, list2tree left, list2tree right)
		end

(*fun list2tree [] = Lf
	| list2tree xs =
		let
			val k = length xs div 2
			val y::ys = List.drop(xs, k)
		in
			Br(y, list2tree (List.take(xs, k)), list2tree ys)
		end
*)
(*4 想了半天哪里错了，后来发现Br外面要加括号 *)
(* Work and span should be O(depth of t) *)
fun revT Lf = Lf
	| revT (Br(root, left, right)) = Br(root, revT right, revT left);

(*5*)
fun binarySearch (Lf, a) = false
	| binarySearch (Br(root, left, right), a) = 
		case Int.compare(root, a) of
			EQUAL => true
			| LESS => binarySearch(right, a)
			| GREATER => binarySearch(left, a)

fun trav Lf = []
	| trav (Br(root, left, right)) = trav left @ (root::trav right)