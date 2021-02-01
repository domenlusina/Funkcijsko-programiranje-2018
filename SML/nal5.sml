Control.Print.printDepth := 100;
datatype expression =  Constant of int |
        Variable of string |
        Operator of string * expression |
        Pair of expression list |
        List of expression list;

fun gcd(a,b)= 
	if b = 0
		then a
	else
		gcd(b, a mod b)  
infix //
fun a // b= if (b div gcd(a,b)) = 1
			then Constant(a div gcd(a,b))
		  else
		  	Operator("/", Pair [Constant (a div gcd(a,b)), Constant  (b div gcd(a,b))])

fun multiply (a,b) = case (a,b) of 
					( Operator("/",Pair [Constant c1, Constant c2]), Operator("/",Pair [Constant c3, Constant c4])) => (c1*c3)//(c2*c4)
					| (Constant c1, Operator("/",Pair [Constant c3, Constant c4])) => (c1*c3)//c4
					| (Operator("/",Pair [Constant c1, Constant c2]), Constant c3) => (c1*c3)//c2
					| _ => Constant 1


fun add(a,b) = case (a,b) of 
					( Operator("/",Pair [Constant c1, Constant c2]), Operator("/",Pair [Constant c3, Constant c4]) ) => (c1*c4+c2*c3)//(c2*c4)
					| _ => Constant 1


fun add2(a,b) = case (a,b) of 
					( Operator("/",Pair [c1, Constant c2]), Operator("/",Pair [c3, Constant c4])) => 
						let 
							val lcm = abs(c2*c4) div gcd(c2,c4)
						in
							(case(c1,c3) of
								(Constant c1, Constant c3)=> Operator("/", Pair [Constant (c1*(lcm div c2)+ c3*(lcm div c4)), Constant lcm ])
								|(Variable c1, Constant c3)=> Operator("/", Pair [Operator("+",Pair  [Constant (c3*(lcm div c4)), Operator("*", Pair [Constant (lcm div c2),Variable c1])]), Constant lcm])
								| (Constant c1, Variable c3)=> Operator("/", Pair [Operator("+",Pair [Constant (c1*(lcm div c2)), Operator("*", Pair [Constant (lcm div c4),Variable c3])]), Constant lcm])
								| (Variable c1, Variable c3)=> (if c1=c3 then
																(*if ((lcm div c2)+(lcm div c4)) = lcm
																	then Variable c1
																elseTODO krajšaj v primeru x/4 + x/4 *)
																	Operator("/", Pair [Operator("*", Pair [Constant ((lcm div c2)+(lcm div c4)), Variable c1]), Constant lcm])
															else
																Operator("/", Pair [ Operator("+", 
																	Pair [ Operator("*",Pair [Constant (lcm div c2) , Variable c1 ]), 
																			Operator("*",Pair [Constant (lcm div c4) , Variable c3 ]) 
																]),
																Constant lcm]))
							)	
						end
					| _ => Operator("IO", Pair [a,b])
					
fun add3 (xs) = 
	List.foldl (fn (x,y) =>
			let 
				val newx = case x of 
					Constant c => Operator("/", Pair [Constant c, Constant 1])
					| Variable v => Operator ("/", Pair [ Variable v, Constant 1])
					| Operator ("/", exp) => Operator ("/", exp)
					| Operator ( s, Pair [p1,p2]) => Operator("/", Pair [Operator ( s, Pair [p1,p2]), Constant 1])

			in
				case (newx, y) of 
					(Operator("/", Pair [x1, Constant c2]), Operator ("/", Pair [x2, Constant c4])) =>
						let
							val lcm =  abs(c2*c4) div gcd(c2,c4)
						in
							(case(x2,x1) of
								(Constant c1, Constant c3)=> Operator("/", Pair [Constant (c1*(lcm div c2)+ c3*(lcm div c4)), Constant lcm ])
								|(Variable c1, Constant c3)=> Operator("/", Pair [Operator("+",Pair  [Constant (c3*(lcm div c4)), Operator("*", Pair [Constant (lcm div c2),Variable c1])]), Constant lcm])
								| (Constant c1, Variable c3)=> if c1=0 then 
																Operator("/", Pair [Variable c3, Constant c4])
															else
																Operator("/", Pair [Operator("+",Pair [Constant (c1*(lcm div c2)), Operator("*", Pair [Constant (lcm div c4),Variable c3])]), Constant lcm])
								| (Variable c1, Variable c3)=> (if c1=c3 then
																	Operator("/", Pair [Operator("*", Pair [Constant ((lcm div c2)+(lcm div c4)), Variable c1]), Constant lcm])
															else
																Operator("/", Pair [ Operator("+", 
																	Pair [ Operator("*",Pair [Constant (lcm div c2) , Variable c1 ]), 
																			Operator("*",Pair [Constant (lcm div c4) , Variable c3 ]) 
																]),
																Constant lcm]))
								| (x1,x2) => Operator("/", Pair [x1,x2])
								)
						end 
			end


		) (Operator("/",Pair [Constant 0, Constant 1])) xs


fun flatten (xs) = 
	case xs of 
		Constant c => Operator("/", Pair [Constant c, Constant 1])
		| Variable v => Operator ("/", Pair [Variable v, Constant 1])
		| Operator("+", Pair [exp1, exp2]) => add2(flatten(exp1), flatten(exp2))
		| Operator("*", Pair [exp1, exp2]) => multiply(flatten(exp1), flatten(exp2))
		| Operator("-", Pair [exp1, exp2]) => 
				let 
					val exp_flatten1 = flatten(exp1)
					val exp_flatten2 = flatten(exp2)
				in
					(case (exp_flatten1, exp_flatten2) of 
						(Operator ("/",Pair(p1::p2::[])), Operator ("/",Pair(p3::p4::[])))=>
							let 
								val v1 = case (p1, p4 ) of
											(Constant c1, Constant c2)=> Constant (c1*c2)
											| _ => Operator("*", Pair [p1, p4 ])
								val v2 = case (p2, p3 ) of
											(Constant c1, Constant c2)=> Constant (c1*c2)
											| _ => Operator("*", Pair [p2, p3 ])
								val imenovalec = case (p2, p4 ) of
											(Constant c1, Constant c2)=> Constant (c1*c2)
											| _ => Operator("*", Pair [p2, p4 ])
								val stevec = case (v1,v2) of 
									(Constant c1, Constant c2)=> Constant (c1-c2)
									| _ =>  Operator("-", Pair [v1,v2])
							in
								(case (stevec, imenovalec) of 
									(Constant c1, Constant c2) => flatten(c1//c2)
									|_ =>Operator ("/", Pair [ stevec,
										imenovalec
									])
							)
						end
						| _=> exp_flatten1
						)
				end

		| Operator("/", Pair [exp1, exp2]) =>
							let 
								val exp_flatten1 = flatten(exp1)
								val exp_flatten2 = flatten(exp2)
							in
							(case (exp_flatten1, exp_flatten2) of 
								(Operator ("/",Pair(p1::p2::[])), Operator ("/",Pair(p3::p4::[])))=>
								let 
									val stevec = if p1 =(Constant 1) then p4 
														else
															if p4 =(Constant 1) then p1 
															else
																Operator ("*",Pair[ p1,p4])
									val imenovalec = if p2 =(Constant 1) then p3 
														else
															if p3 =(Constant 1) then p2
															else
																Operator ("*",Pair[ p2,p3])
								in
									Operator("/",Pair[ 
										stevec, 
										imenovalec
										])
								end
								| _ => Constant 1
							)
							end 

		| _=> xs
				
		
								




fun count_constants xs = 
	case xs of 
		Constant _ => 1
		| Variable _ => 0
		| Operator (s,v) => count_constants(v)
		| Pair p => List.foldl (fn (x,y)=> y+ count_constants(x)) 0 p
		| List xs => List.foldl (fn (x,y)=> y+ count_constants(x)) 0 xs

fun sum_constants xs = 
	case xs of  
		Constant c => c
		| Variable _ => 0
		| Operator (s,v) => sum_constants(v)
		| Pair p => List.foldl (fn (x,y)=> y+ sum_constants(x)) 0 p
		| List xs => List.foldl (fn (x,y)=> y+ sum_constants(x)) 0 xs


fun myexist (xs,x) = 
	List.exists (fn(y)=> if y=x then true else false) xs;

fun all_variables x =
	case x of
		Constant _ => []
		| Variable s => s::[]
		| Operator (s, v) => all_variables(v)
		| (Pair xs | List xs ) => (List.foldl (fn (x: expression,acc:string list) => 
			(
			case acc of 
				[] => acc@all_variables(x)
				| head::tail => (
								if ( myexist((tail@all_variables(x)),head) )
								then acc
								else acc@all_variables(x)
							)
			)) [] xs)



fun traverse (f1: (int -> 'a)) (f2: (string -> 'a))  (f3: ('a * 'a -> 'a)) (d: 'a) (e:expression) = d;