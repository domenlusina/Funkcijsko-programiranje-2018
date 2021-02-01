signature COUNTER =
sig
    val reset : unit -> unit
    val value : unit -> int
    val add1 : unit -> unit
    val add2 : unit -> unit
    val add3 : unit -> unit
    val add4 : unit -> unit
    val add5 : unit -> unit
end

structure Counter :> COUNTER = 
struct
	local 
		val cnt = ref 0
	in
 		fun reset () = (cnt:=0)
 		fun value () = (!cnt)
 		fun add1 () = (cnt:=((!cnt)+1))
 		fun add2 () = (cnt:=((!cnt)+2))
 		fun add3 () = (cnt:=((!cnt)+3))
 		fun add4 () = (cnt:=((!cnt)+4))
 		fun add5 () = (cnt:=((!cnt)+5))
 	end
end


datatype tree = lf | br of (tree * int * tree)
fun optree operation current neutral t =
    case t of
        lf => neutral
    |   br (left, v, right) => operation(
                                optree operation current neutral left,
                                operation (
                                    optree operation current neutral right,
                                    current v
                                )
                            )

fun sum_tree tree   =   (optree) (fn(x,y)=>x+y) (fn(z)=>z) 0 tree;
fun prod_tree tree  =   (optree) (fn(x,y)=>x*y) (fn(z)=>z) 1 tree;
fun count_tree tree =   (optree) (fn(x,y)=>x+y) (fn(z)=>1) 1 tree;