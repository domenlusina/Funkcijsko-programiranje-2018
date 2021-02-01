
Control.Print.printDepth := 100;
open List;


datatype expression =  Constant of int |
        Variable of string |
        Operator of string * expression |
        Pair of expression list |
        List of expression list
exception InvalidVariable of string
exception InvalidExpression of expression

fun cross(xs1, xs2) = 
	case (xs1, xs2) of
		  ( [], [] ) => []
		| ( _, [] ) => []
		| ( [], _ ) => []
		| _ => foldl (fn (x, acc1) => acc1 @ (foldl (fn (y, acc2) => acc2 @ [(x,y)]) [] xs2)) [] (xs1)

datatype pattern = ConstantP of int
    | VariableP of string
    | OperatorP of string * pattern
    | PairP of pattern list
    | ListP of pattern list
    | UnorderedListP of pattern list
    | Wildcard

fun getConstant exp = case exp of Constant c =>c | _ => 0

fun insertall x xs=
	case xs of 
		[] => [[x]]
  | head::tail =>  (x::head::tail)::(map (fn l => head::l) (insertall x tail));

fun permutation xs =
	case xs of 
	[] => [[]]
   | head::tail => concat (map (insertall head) (permutation tail));


fun match (exs, pat)=

		case (exs,pat) of 
			(_, Wildcard) => SOME []
			| (v, VariableP s) => SOME [(s,v)]
			| (Constant C, ConstantP CP) => if C=CP then SOME [] else NONE
			| (Pair [a1, a2], PairP [b1, b2]) =>   if isSome(match(a1, b1)) andalso isSome(match(a2, b2)) 
														then SOME (valOf(match(a1,b1))@valOf(match(a2, b2)))
													else NONE
			| (List [], ListP []) => SOME []
			| (List _, ListP []) => NONE
			| (List [], ListP _) => NONE
			| (List xs, ListP ps) =>if isSome(match(hd xs, hd ps))  andalso isSome(match(List (tl xs), ListP (tl ps)))
										then SOME (valOf(match(hd xs, hd ps))@valOf(match(List (tl xs), ListP (tl ps)))) 
									else NONE
			| (Operator (a, x), OperatorP(b,y)) => if a=b then match(x,y) else NONE	
			| (List xs, UnorderedListP ps) => let 
													val per= permutation(ps)
												in 
													foldl (fn(x,acc)=>if isSome(acc) then acc else match(List xs, ListP x)) NONE per 
												end							  
			| _ => NONE

(*fun matchUnordered ( xs,  ps) = 
	case  (xs, ps) of
		(List xs, UnorderedListP ps)=>
		let 
			val per= permutation(ps)
		in 
			foldl (fn(x,acc)=>if isSome(acc) then acc else match(List xs, ListP x)) NONE per 
		end
		| _ => NONE
*)



fun eval xs exp = case exp of 
	Constant c => c
	| Variable v => (case xs of 
					[] => raise InvalidVariable v
					| (varName, varValue)::tail => if varName = v 
														then eval xs (Constant varValue) 
													else eval tail exp)
	| Operator (s, Pair(x1::x2::[])) => let 
											val x = eval xs x1
											val y = eval xs x2 
										in 
											(case s of 
												"+" => x+y
												| "*" => x*y
												| "-" => x-y
												| "/" =>  if y=0 then raise InvalidExpression exp else x div y
												| "%" =>  if y=0 then raise InvalidExpression exp else x mod y
												| _ => raise InvalidExpression exp)
										end
	| Operator (s, Pair _ ) => raise InvalidExpression exp
	| Operator (s, List l) => (case s of 
								"+" => foldl (fn (x,y) => (eval xs x)+y) 0 l
								| "*" => foldl (fn (x,y) => (eval xs x) * y) 1 l
								| _ => raise InvalidExpression exp)
	| _ => raise InvalidExpression exp




fun derivative exp s=
	case exp of 
		Constant c => Constant 0
		| Variable v => if v=s then Constant 1 else Constant 0
		| Operator (x, Pair(x1::x2::[])) => (case x of 
												"+" => Operator ("+", (Pair[ derivative (x1) s, derivative (x2) s]))
												| "-" =>  Operator ("-", Pair[ derivative (x1) s, derivative (x2) s])
												| "*" => Operator ("+", Pair[ Operator ("*", Pair [ derivative (x1) s, (x2)]), Operator("*",Pair [ (x1), derivative (x2) s])])
												| "/" => Operator("/", Pair [Operator ("-", Pair[ Operator ("*", Pair [ derivative (x1) s, (x2)]), Operator("*",Pair [ (x1), derivative (x2) s])]), Operator("*", Pair [(x2), (x2)])])
												| _ => raise InvalidExpression exp)
		| _ => raise InvalidExpression exp
		

(*removeEmpty (Operator ("*", Pair [(Operator("*", List [Constant 0])), Constant 5]));*)
fun removeEmpty exp = 
	case exp of 
		Constant c=> Constant c
		| Variable v => Variable v
		| Operator ("+", (List []| Pair [])) => Constant 0
		| Operator ("*", (List []| Pair [])) => Constant 1
		| Operator("+", List(head::[])) => removeEmpty head
		| Operator("*", List(head::[])) => removeEmpty head
		| Operator ("/", (Pair(x1::(Constant 1)::[])|List(x1::(Constant 1)::[]))) => removeEmpty x1
		| Operator ("+", (List xs| Pair xs)) => removeEmpty(Operator ("+",List(map (removeEmpty) (filter (fn(x)=> x <> Constant 0) xs))))  
		| Operator ("*", (List xs| Pair xs)) => if (foldl(fn(x,y)=> y andalso (x <> (Constant 0)))  true xs) 
										then   removeEmpty(Operator ("*",List(map (removeEmpty) (filter (fn(x)=> x <> Constant 1) xs))))
									else
										Constant 0
		| Operator ("-", Pair(x1::x2::[])) => (case  (x1,x2) of 
													(Constant 0, Constant 0) => Constant 0
													|(x1, Constant 0) =>removeEmpty(x1)
													|(Constant 0, Constant x2) =>Constant (~x2)
													| (x1,x2) => Operator ("-", Pair [removeEmpty(x1), removeEmpty(x2)])
													)
		| _ => exp
(*
fun my_simplify(exp) =
	case exp of 
		Variable v => Variable v 
		| Constant c => Constant c
		| Operator (s, (List xs | Pair xs)) => removeEmpty (Operator (s, List (map (fn(x)=> (
				case x of 
					Operator (s2, _ ) => my_simplify(x)
					| _ => removeEmpty (x)
			)) xs)))
        
*)


fun combinations(xs) = 
	case xs of
		[] => [[]]
		| glava::rep => foldl (fn(x,acc) => acc@(map (fn(y) => x::y) (combinations rep)) ) [] glava


fun getList(exp)=
	case exp of 
		Variable v => [Variable v]
		| Constant c => [Constant c]
		| Operator(_, List xs) => xs
		| Operator(_, Pair xs) => xs
		| _ => []

fun simplifyList(oper, s) =
	foldl (fn(x,acc)=> case x of 
					Operator(oper2, (List xs | Pair xs)) => if oper2= oper then simplifyList(oper, xs)@acc else x::acc
					| _ => x::acc
		) [] s


fun preformOperationOnList  (oper: (int*int)-> int, xs:expression list) = 
		foldl (fn(x,acc)=>
				case x of 
					Variable v => acc@[Variable v]
					| Constant c => if acc = [] then
										(Constant c):: acc
									else
										(case (hd acc) of 
												Constant c2 => (Constant (oper(c,c2)))::(tl acc)
												| _ => (Constant c):: acc
											)
					| ostalo => ostalo :: acc 
			) [] xs


fun flatten exp:expression = 
	case exp of
		Variable v => Operator("+", List [Variable v])
		| Constant c => Operator("+", List [Constant c])
		| Operator ("*", Pair [p1,p2]) => flatten( Operator("*", List [p1,p2]))
		| Operator ("+", Pair [p1,p2]) => flatten( Operator("+", List [p1,p2]))
		| Operator ("*", List s) => let
										val sim = simplifyList("*", s)
										val fla = map (fn(x)=> flatten(x)) sim
										val all_lists = foldl (fn(x,y)=> getList(x)::y) [] fla
										val com = combinations(all_lists)
									in 
										foldl (fn(x,acc)=>Operator("+", List (Operator("*", List (simplifyList("*",x)) )::getList(acc)))) (Operator("+", List [])) com
									end
		| Operator ("+", List s) => let
										val sim = simplifyList("+", s)
										val fla = map (fn(x)=> flatten(x)) sim
										val sim2 = simplifyList("+", fla)
									in 
										Operator("+", List sim2)
									end
		| _ => exp
		



fun replace(xs, i, new) = if i =0 then 
							new :: (tl xs)
						  else 
							(hd xs)::replace(tl xs, i-1, new);

fun longestList xs =
	foldl (fn(x,y)=> if length(x)> y then length(x) else y) 0 xs 


fun getPolinomList(exp1)= 
	let
		val sim1 = preformOperationOnList(op+, getList(flatten(exp1)))
		val sim1 = map (fn(x)=> preformOperationOnList(op*,getList(x))) sim1
		val addConst1= map (fn(x)=> case hd x of Constant _ => x | _ => (Constant 1)::x ) sim1
		val stopnja1= (longestList addConst1)
		val pol1 = tabulate (stopnja1, fn(x)=>0)
		val pol1 = foldl (fn(x,y)=>case hd x of 
										Constant c=> replace(y, stopnja1- length(x), c + nth(y, stopnja1-length(x)))
										| _=> y
										) pol1 addConst1 
	in
		pol1
	end

fun divide (exp1:expression,exp2:expression) =  let 
		val pol1 = getPolinomList (exp1)
		val pol2 = getPolinomList (exp2)

		val sim1 = preformOperationOnList(op+, getList(flatten(exp1)))
		val spremenljivka = 
		foldl (fn(x,acc1)=> (
			let 
				val variableName=foldl (fn(y,acc2)=> case y of Variable v => v | _=>acc2 ) "" (getList(x))
			in
				if variableName = "" then acc1
				else variableName
			end
			)) "" sim1

		fun divide  pol1 pol2 = 
					if pol1 = [] then 
						[]
					else 
						if length(pol1) < length(pol2) then 
							[]
						else	 
							let
								val coef = (hd pol1) div (hd pol2)
								val extended_pol2 = pol2@(tabulate(length(pol1)-length(pol2) ,fn(x)=>0))
								val pol1= map (fn(x)=> (#1 x)- coef* (#2 x)  ) (ListPair.zip(pol1, extended_pol2))
							in
								coef :: divide (tl pol1) pol2
							end

		val res= divide pol1 pol2
		val indeksi = tabulate(length(res) ,fn(x)=>length(res) - x -1)
	in
		Operator("+", List  
			(
				foldl (fn(x,acc)=> 
						if (#2 x) =0 then acc 
						else Operator("*",List ((Constant (#2 x))::(tabulate((#1 x) ,fn(x)=>(Variable spremenljivka)))))::acc
					)  []  (ListPair.zip(indeksi, res))
				)
		)
	end

fun quicksortVariables (xs)=
	case xs of [] => [] 
    | (Variable head)::tail  => quicksortVariables (filter(fn (Variable x)=> x < head) tail) @ [Variable head] @ (filter(fn (Variable x)=> x = head ) tail)@ quicksortVariables (filter(fn (Variable x)=> x > head ) tail);
	

fun joinSimilar exp =  
let
	val sim1 = preformOperationOnList(op+, getList(flatten(exp)))
	val sim1 = map (fn(x)=> preformOperationOnList(op*,getList(x))) sim1
	val addConst1= map (fn(x)=> case hd x of Constant _ => x | _ => (Constant 1)::x ) sim1
	val urejeno = map (fn x => (hd x ):: quicksortVariables(tl x)) addConst1
	fun equivalence_classes xs = 
		if null xs then []
		else
			let 
				val (pos, neg) = partition(fn x =>  (tl x) = (tl (hd xs))) xs
			in
				pos:: equivalence_classes neg
			end

	val skupine = equivalence_classes urejeno
	val poenostavljeno = foldl  (fn(x,acc)=> (foldl (fn(y, acc2)=>if  acc2=[] then y else  ((Constant (getConstant(hd y) + getConstant(hd acc2))):: (tl y) )) [] x)::acc)  [] skupine
	val mnozenja = foldl (fn (x, acc) =>  if getConstant(hd x)=0 then acc else (Operator("*", List x))::acc) [] poenostavljeno ;
in
	if mnozenja = [] then Constant 0 
	else Operator("+", List (mnozenja))
end

(*
;;;; "my tests"  ;;;;


val test_cross1 = cross ([1, 2, 3],[3, 4, 5]) = [(1,3),(1,4),(1,5),(2,3),(2,4),(2,5),(3,3),(3,4),(3,5)]
val test_cross2 = cross ([1, 2, 3],[]) = []
val test_cross3 = cross ([],[3,4,5]) = []
val test_cross4 = cross ([1,2,3],[4,5,6,7]) = [(1,4),(1,5),(1,6),(1,7),(2,4),(2,5),(2,6),(2,7),(3,4),(3,5),(3,6),(3,7)]
val test_cross5 = cross ([1],[4,5,6]) = [(1,4),(1,5),(1,6)]

val test_match0 = match(Operator("+", Pair [Operator ("*", List [Variable "a", Variable "a"]),Operator ("*", List [Variable "b", Variable "b"])]), OperatorP("+", PairP [VariableP "A", VariableP "B"])) = SOME[("A",Operator ("*",List [Variable "a",Variable "a"])),("B",Operator ("*",List [Variable "b",Variable "b"]))]
val test_match1 = match(Constant 10, ConstantP 10)= SOME []
val test_match2 = match(Constant 10, ConstantP 11)= NONE
val test_match3 = match(Variable "x", VariableP "x")= SOME [("x",Variable "x")]
val test_match4 = match(Variable "x", VariableP "y")= SOME [("y",Variable "x")]
val test_match5 =  match(Pair [Constant 1, Variable "x"], PairP [ConstantP 3, VariableP "x"]) = NONE
val test_match6 =  match(Pair [Constant 1, Variable "x"], PairP [ConstantP 1, VariableP "x"]) = SOME [("x",Variable "x")]
val test_match7 = match(Operator("+", Pair [Constant 1, Constant 2]), OperatorP("+", PairP [ConstantP 1, ConstantP 2])) = SOME []
val test_match8 = match(Operator("*", Pair [Constant 1, Constant 2]), OperatorP("+", PairP [ConstantP 1, ConstantP 2])) = NONE
val test_match9 = match (List [Constant 1, Constant 2, Constant 3], ListP [ConstantP 1, ConstantP 2, ConstantP 3]) = SOME []
val test_match10 = match (List [Constant 1, Constant 2], ListP [ConstantP 1, ConstantP 2]) = SOME []
val test_match11 = match (List [Constant 1], ListP [ConstantP 1])= SOME []
val test_match12 = match (List [Constant 1, Constant 2, Constant 3], ListP [VariableP "A", VariableP "B", VariableP "C"]) = SOME [("A",Constant 1),("B",Constant 2),("C",Constant 3)]


val test_eval0= eval [("x", 3)] 
(Operator("+", List [
    Operator ("*", Pair [Constant 3, Variable "x"]),
    Constant 7,
    Operator ("*", List [
        Constant 4,
        Variable "x",
        Variable "x"
    ])
])) = 52

val test_eval1 = (eval [] (List [])) = 512 handle InvalidExpression _ => true;
val test_eval2 = (eval [("x", 3)] (Variable "x")) = 3
val test_eval3 = eval [("x", 3)] (Operator("+", Pair [Variable "x",Constant 10]))	= 13
val test_eval4 = eval [("x", 3), ("y",5)] (Operator("+", Pair [Variable "x",Operator("*", Pair [Constant 10, Operator("-", Pair [Variable "y", Variable "x"])])])) = 23
val test_eval5 = eval [("x", 123)] (Operator ("/", Pair [Constant 46,Operator("%", Pair [Variable "x", Constant 100])])) = 2
val test_eval6 = (eval [] (Operator("/", Pair [Constant 100, Constant 0]))) = 512 handle InvalidExpression (Operator("/", Pair [Constant 100, Constant 0])) => true;
val test_eval7 = (eval [("y", 3)] (Variable "x"))=512 handle InvalidVariable _ => true


val test_der0 = (derivative (Operator ("*", Pair [
    Variable "x",
    Operator ("*", Pair [
        Constant 3,
        Variable "x"
    ])
])) "x") = Operator ("*",List [Constant 6,Variable "x"]);*)