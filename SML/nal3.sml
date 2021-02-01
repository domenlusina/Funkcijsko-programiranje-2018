fun sorted(xs): bool =
	case xs of 
		[] => true
		| x1::[]=> true  
		| x1::(x2::rep)=>case Int.compare(x1,x2) of
								EQUAL => true andalso sorted(x2::rep)
								| LESS => true andalso sorted(x2::rep)
								| GREATER => false

fun zip(xs1, xs2) =
	case (xs1,xs2) of
		([], []) => []
		| ([], _) => []
		| (_, []) => []
		| _ => ((hd xs1),(hd xs2)) :: zip(tl xs1, tl xs2)

fun reverse(xs)=
	let
		fun reverse(xs1, xs2) = 
			case (xs1,xs2) of
			([],_)=> xs2
			| _ => reverse(tl xs1, hd xs1 :: xs2)
	in
		reverse(xs, [])  
	end

datatype natural = NEXT of natural | ZERO


fun toNatural(a: int): natural = 
	case a of
		0 => ZERO
		| _ => NEXT(toNatural(a-1))


fun isEven(a: natural): bool = 
	case a of 
		ZERO => true
		| NEXT(ZERO) => false
		| NEXT(NEXT(x)) => isEven(x)

fun isOdd(a: natural): bool = 
	not (isEven(a))

exception PreviousOfZero

fun previous(a: natural): natural =
		case a of 
			ZERO => raise PreviousOfZero
			| NEXT(x) => x


fun subtract(a: natural, b: natural): natural =
	case (a,b) of
		(ZERO, ZERO) => ZERO
		| (ZERO, _) => raise PreviousOfZero
		| (a, ZERO) => a
		| (NEXT(a),NEXT(b)) => subtract(a,b)

fun any(f, xs): bool= 
	case  (f,xs) of
		(f,[]) => false
		| (f,xs) => f(hd xs) orelse any(f, tl xs)

fun all(f, xs): bool= 
	case  (f,xs) of
		(f,[]) => true
		| (f,xs) => f(hd xs) andalso all(f, tl xs)

;;;; "type tests"  ;;;;
val test_sorted_type: int list -> bool = sorted
val test_zip_type: 'a list * 'b list -> ('a * 'b) list = zip
val test_reverse_type: 'a list -> 'a list = reverse
val test_toNatural_type: int -> natural = toNatural
val test_isEven_type: natural -> bool = isEven
val test_isOdd_type: natural -> bool = isOdd
val test_previous_type: natural -> natural = previous
val test_subtract_type: natural * natural -> natural = subtract
val test_any_type: ('a -> bool) * 'a list -> bool = any
val test_all_type: ('a -> bool) * 'a list -> bool = all