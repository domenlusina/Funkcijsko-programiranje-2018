fun factorial(n: int) = 
	if n < 2
		then 1
	else
		n*factorial(n - 1)

fun power(x: int, n: int) =
		if n = 0
			then 1
		else
			x*power(x, n-1)

fun gcd(a: int, b: int) =
	if b = 0
		then a
	else
		gcd(b, a mod b) 

fun len(xs: int list)=
	if xs = []  
		then 0
	else 
		1 + len(tl xs)

fun last(xs: int list)=
	if xs = []
		then NONE
	else 
		if tl xs = []
			then SOME (hd xs)
		else
			last(tl xs)

fun nth(xs: int list, n: int)=
	if xs = []
		then NONE
	else
		if n = 0
			then SOME (hd xs)
		else 
			nth(tl xs, n-1)

fun insert(xs: int list, n: int, x:int)=
	if n = 0
		then x::xs
	else
		hd xs::insert(tl xs, n-1, x)

fun delete(xs: int list, x:int)=
	if xs = []
		then []
	else 
		if hd xs = x
			then delete(tl xs, x)
		else
			hd xs::delete(tl xs, x)

fun append(xs: int list, x:int)=
	if xs = []
		then [x]
	else
		(hd xs)::append(tl xs, x)

fun reverse(xs: int list)=
	if xs = []
		then []
	else
		append(reverse(tl xs),hd xs)

fun palindrome(xs: int list)= xs=reverse(xs) 

(*
;;;; "my tests"  ;;;;
val test_fac1 = factorial(0) = 1
val test_fac2 = factorial(5) = 120

val test_pow1 = power(5,3) = 125
val test_pow2 = power(5,0) = 1

val test_gcd1 = gcd(366,60) = 6
val test_gcd2 = gcd(1,6) = 1

val test_len1 = len([1,2,3,4,5,6]) = 6
val test_len2 = len([]) = 0

val test_last1 = last([1,2,3,4,5,6]) = SOME 6
val test_last2 = last([1,2,3,4,5,0]) = SOME 0
val test_last3 = last([]) = NONE

val test_nth1 = nth([1,2,3,4,5,6],2) = SOME 3
val test_nth2 = nth([1,2,3,4,5,6],5) = SOME 6
val test_nth3 = nth([1,2,3,4,5,6],10) = NONE

val test_insert1 = insert([1,2,3,4,5,6],0,100) = [100,1,2,3,4,5,6]
val test_insert2 = insert([1,2,3,4,5,6],1,100) = [1,100,2,3,4,5,6]
val test_insert3 = insert([1,2,3,4,5,6],6,100) = [1,2,3,4,5,6,100]

val test_delete1 = delete([1,1,1,1,1,1],1) = []
val test_delete2 = delete([1,3,4,1,5,1],1) = [3,4,5]
val test_delete3 = delete([2,3,4,2,5,2],2) = [3,4,5]
val test_delete4 = delete([3,2,4,2,2,2,5],2) = [3,4,5]

val test_append1 = append([1,3,4,1,5,1],19) = [1,3,4,1,5,1,19]
val test_append2 = append([],0) = [0]

val test_reverse1 = reverse([1,2,3,4,5,6]) = [6,5,4,3,2,1]
val test_reverse2 = reverse([]) = []

val test_palindrome1 = palindrome([1,2,3,2,1]) = true
val test_palindrome2 = palindrome([1,2,3,4,5]) = false
val test_palindrome3 = palindrome([]) = true
*)


;;;; "type tests"  ;;;;
val test_factorial_type : int -> int = factorial
val test_power_type : int * int -> int = power
val test_gcd_type : int * int -> int = gcd
val test_len_type : int list -> int = len
val test_last_type : int list -> int option = last
val test_nth_type : int list * int -> int option = nth
val test_insert_type : int list * int * int -> int list = insert
val test_delete_type : int list * int -> int list = delete
val test_reverse_type : int list -> int list = reverse
val test_palindrome_type : int list -> bool = palindrome