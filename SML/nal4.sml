open List

fun reduce (f, z, xs) =
	if xs = []
			then z
	else
		if tl xs = []
			then f(z, hd xs)
		else
			reduce(f, f(z, hd xs),tl xs)



fun squares (xs) = map (fn x=>x*x) xs

fun only_even(xs) = filter(fn x=> x mod 2=0)  xs

fun best_string f xs = foldl(fn (x, y)=>if  f(x,y) then x else y) "" xs
fun hlongest_string(x,y)= if size(x)> size(y) then true else false
fun hlargest_string(x,y)= case String.compare(x,y) of GREATER => true | _ => false
fun largest_string xs = best_string hlargest_string xs
fun longest_string xs = best_string hlongest_string xs



fun quicksort (xs)=
	case xs of [] => [] 
    | head::tail  => quicksort (filter(fn x=> x < head) tail) @ [head] @ quicksort (filter(fn x=> x > head ) tail)


fun dot v1 v2 = foldl(fn (x, y)=>x+y) 0 (map(fn (x,y) => x*y) (ListPair.zip(v1,v2)))


fun transpose (mat) = 
	case mat of
		[nil]  => [nil]
		| _ => case (map tl mat) of
				[]::_ => [map hd mat]
				| _ => (map hd mat)::(transpose(map tl mat))

fun multiply  a b  = case (a,b) of 
	([[]],[[]]) => [[]]
	| _=> transpose (map(fn x=>(map(dot x)  a)) (transpose b));

fun h1 x l=
	if l = []
		then [(x,1)]
	else
		if x=(#1 (last l) )
			then map(fn(x)=> if last l = x then (#1 x, (#2 x)+1) else x) l
		else
			l@[(x,1)] 


fun group k = foldl(fn(x,l)=>h1 x l) [] k

fun equivalence_classes f x = [[]]





(* Custom test function
fun test(test: string, f: 'a -> ''b, par: 'a, corRes: ''b) =
	let
		val result = f(par)
	in
		if result = corRes
			then (print("OK\n"); (test, [corRes]))
			else (print("FALSE\n"); (test, [corRes, result]))
	end;

*)

(* Tests for quicksort 
val _ = print("\n\nTesting function quicksort:\n");
val test_quicksort: int list -> int list = quicksort;
test("quicksort: 1", quicksort, nil, nil);
test("quicksort: 2", quicksort, [2,1], [1,2]);
test("quicksort: 2", quicksort, [2, 3, 1], [1,2,3]);
test("quicksort: 2", quicksort, [1], [1]);
test("quicksort: 3", quicksort, [3, 2, 1], [1, 2, 3]);
test("quicksort: 4", quicksort, [1, 2, 3, 4, 5, 6, 8, 7, 9, 10], [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);
test("quicksort: 4", quicksort, [5, 4, 1, 2, 3, 10, 6, 9, 7, 8], [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);
test("quicksort: 4", quicksort, [10,9,8,7,6,5,4,3,2,1], [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);


*)

(* Tests for multiply 
val _ = print("\n\nTesting function multiply:\n");
val test_multiply: int list list -> int list list -> int list list = multiply;
test("multiply: 1", multiply [[]], [[]], [[]]);
test("multiply: 2", multiply [[1]], [[1]], [[1]]);
test("multiply: 3", multiply [[1, 2]], [[1], [2]], [[5]]);
test("multiply: 4", multiply [[1,2,3],[4,5,6]], [[7,8],[9,10],[11,12]], [[58, 64],[139,154]]);
test("multiply: 5", multiply [[7,8],[9,10],[11,12]], [[1,2,3],[4,5,6]], [[39, 54, 69],[49, 68, 87],[59, 82, 105]]);


val _ = print("\n\nTesting function equivalence_classes:\n");
val test_equivalence_classes: (''a -> ''a -> bool) -> ''a list -> ''a list list = equivalence_classes;*)

