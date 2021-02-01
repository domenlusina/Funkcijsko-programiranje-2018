fun next(x) = x+1;
fun add(a, b)= a+b;


;;;; "type tests"  ;;;;
val test_next_type:int->int = next;
val test_add_type:int*int->int = add;