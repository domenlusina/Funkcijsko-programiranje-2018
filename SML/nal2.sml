datatype natural = NEXT of natural | ZERO

fun toInt(a:natural): int=
	case  a of
		ZERO => 0
		|NEXT(b) => 1+toInt(b)

fun add(a:natural,b:natural):natural=
	case a of 
		ZERO => b
		| NEXT(a) => add(a,NEXT(b))



datatype bstree = br of bstree * int * bstree | lf

fun min(tree: bstree)=
	case tree of
		lf => NONE
		| br(lf ,v,_) => SOME v
		| br(left ,_,_) => min(left)

fun max(tree: bstree)=
	case tree of
		lf => NONE
		| br(_ , v, lf) => SOME v
		| br(_ , _, right) => max(right)



fun contains(tree: bstree, x: int): bool=
	case tree of 
		lf=> false
		| br(left,v,right)=> case Int.compare(v,x) of
								EQUAL => true
								| LESS => contains(right, x)
								| GREATER => contains(left, x)

fun countLeaves(tree: bstree): int=
	case tree of
		lf=> 1
		| br(left,_,right)=>countLeaves(left)+countLeaves(right)

fun countBranches(tree: bstree): int =
	case tree of 
		lf => 0
		| br(left,_,right)=>1+countBranches(left)+countBranches(right)

fun toList(tree: bstree): int list=
	case tree of
		lf => []
		| br(left, v, right)=> toList(left)@v::toList(right)



fun valid(tree: bstree): bool=
	case tree of 
		lf => true
		| br(lf,_,lf) => true
		| br(left,v,lf) => if isSome(max(left)) andalso valOf(max(left))>=v then false else valid(left)
		| br(lf,v,right) => if isSome(min(right)) andalso valOf(min(right))<=v then false else valid(right)
		| br(left, v, right) => if (isSome(min(tree)) andalso valOf(min(tree))>=v) orelse (isSome(max(tree)) andalso valOf(max(tree))<=v) then
									false
								else
									valid(left) andalso valid(right)


type person = {age:int, name: string} 

fun oldest(people: {age:int, name: string} list): string option=
	case people of 
		[]=> NONE
		| {age= p1, name= n1}::[] => SOME n1 
		| {age= p1, name= n1} :: {age= p2, name= n2} :: xs =>
				if p1 > p2 then
					oldest({age= p1, name= n1}:: xs)
				else
					oldest({age= p2, name= n2}:: xs)


fun oldest2(people: person list): string option=
	case people of 
		[]=> NONE
		| p1::[] => SOME (#name p1 )
		| p1 :: p2 :: xs =>
				if #age p1 > #age p2 then
					oldest2(p1:: xs)
				else
					oldest2(p2:: xs)




(* Custom test function*)
fun test(test: string, f: ''a -> ''b, par: ''a, corRes: ''b) =
	let
		val result = f(par)
	in
		if result = corRes
		then (print("OK\n"); (test, [corRes]))
		else (print("FALSE\n"); (test, [corRes, result]))
	end;

(* Tests for toInt *)
val _ = print("\n\nTesting function toInt:\n");
test("toInt: 1", toInt, ZERO, 0);
test("toInt: 2", toInt, NEXT(ZERO), 1);
test("toInt: 3", toInt, NEXT(NEXT(NEXT(NEXT(NEXT(NEXT(NEXT(NEXT(NEXT(NEXT(NEXT(NEXT(NEXT(NEXT(ZERO)))))))))))))), 14);

(* Tests for add *)
val _ = print("\n\nTesting function add:\n");
test("add: 1", add, (ZERO, ZERO), ZERO);
test("add: 2", add, (NEXT(ZERO), ZERO), NEXT(ZERO));
test("add: 3", add, (ZERO, NEXT(ZERO)), NEXT(ZERO));
test("add: 4", add, (NEXT(NEXT(NEXT(NEXT(NEXT(NEXT(NEXT(NEXT(ZERO)))))))), NEXT(NEXT(NEXT(NEXT(NEXT(NEXT(NEXT(NEXT(ZERO))))))))), NEXT(NEXT(NEXT(NEXT(NEXT(NEXT(NEXT(NEXT(NEXT(NEXT(NEXT(NEXT(NEXT(NEXT(NEXT(NEXT(ZERO)))))))))))))))));

(* Tests for min *)
val _ = print("\n\nTesting function min:\n");
test("min: 1", min, lf, NONE);
test("min: 2", min, br(lf, 5, lf), SOME(5));
test("min: 3", min, br(br(br(lf, 2, lf), 7, lf), 9, br(br(lf, 13, lf), 21, br(lf, 25, lf))), SOME(2));
test("min: 4", min, br(br(br(br(lf, ~3, lf), ~1, lf), 3, lf), 100, lf), SOME(~3));
test("min: 5", min, br(br(lf, 2, lf), 5, lf), SOME(2));

(* Tests for max *)
val _ = print("\n\nTesting function max:\n");
test("max: 1", max, lf, NONE);
test("max: 2", max, br(lf, 5, lf), SOME(5));
test("max: 3", max, br(br(br(lf, 2, lf), 7, lf), 9, br(br(lf, 13, lf), 21, br(lf, 25, lf))), SOME(25));
test("max: 4", max, br(br(br(br(lf, ~3, lf), ~1, lf), 3, lf), 100, lf), SOME(100));
test("max: 5", max, br(lf, 5, br(lf, 6, lf)), SOME(6));

(* Tests for contains *)
val _ = print("\n\nTesting function contains:\n");
test("contains: 1", contains, (lf, 500), false);
test("contains: 2", contains, (br(lf, 5, lf), 5), true);
test("contains: 3", contains, (br(br(br(lf, 2, lf), 7, lf), 9, br(br(lf, 13, lf), 21, br(lf, 25, lf))), 21), true);
test("contains: 4", contains, (br(br(br(br(lf, ~3, lf), ~1, lf), 3, lf), 100, lf), ~3), true);
test("contains: 5", contains, (br(lf, 5, br(lf, 6, lf)), 4), false);
test("contains: 6", contains, (br(br(br(lf, 2, lf), 7, lf), 9, br(br(lf, 13, lf), 21, br(lf, 25, lf))), 3), false);

(* Tests for countLeaves *)
val _ = print("\n\nTesting function countLeaves:\n");
test("countLeaves: 1", countLeaves, lf, 1);
test("countLeaves: 2", countLeaves, br(lf, 5, lf), 2);
test("countLeaves: 3", countLeaves, br(br(br(lf, 2, lf), 7, lf), 9, br(br(lf, 13, lf), 21, br(lf, 25, lf))), 7);
test("countLeaves: 4", countLeaves, br(br(br(br(lf, ~2, lf), ~1, lf), 3, lf), 100, lf), 5);
test("countLeaves: 5", countLeaves, br(lf, 5, br(lf, 6, lf)), 3);

(* Tests for countBranches *)
val _ = print("\n\nTesting function countBranches:\n");
test("countBranches: 1", countBranches, lf, 0);
test("countBranches: 2", countBranches, br(lf, 5, lf), 1);
test("countBranches: 3", countBranches, br(br(br(lf, 2, lf), 7, lf), 9, br(br(lf, 13, lf), 21, br(lf, 25, lf))), 6);
test("countBranches: 4", countBranches, br(br(br(br(lf, ~3, lf), ~1, lf), 3, lf), 100, lf), 4);
test("countBranches: 5", countBranches, br(lf, 5, br(lf, 6, lf)), 2);

(* Tests for toList *)
val _ = print("\n\nTesting function toList:\n");
test("toList: 1", toList, lf, nil);
test("toList: 2", toList, br(lf, 5, lf), [5]);
test("toList: 3", toList, br(br(br(lf, 2, lf), 7, lf), 9, br(br(lf, 13, lf), 21, br(lf, 25, lf))), [2, 7, 9, 13, 21, 25]);
test("toList: 4", toList, br(br(br(br(lf, ~3, lf), ~1, lf), 3, lf), 100, lf), [~3, ~1, 3, 100]);
test("toList: 5", toList, br(lf, 5, br(lf, 6, lf)), [5, 6]);

(* Tests for oldest *)
val _ = print("\n\nTesting function oldest:\n");
test("oldest: 1", oldest, nil, NONE);
test("oldest: 2", oldest, [{name="ime", age=5}], SOME("ime"));
test("oldest: 2", oldest, [{name="ime", age=56}, {name="ime2", age=5}], SOME("ime"));
test("oldest: 3", oldest, [{name="ime", age=5}, {name="ime2", age=2}, {name="ime3", age=10}], SOME("ime3"));
test("oldest: 4", oldest, [{name="ime", age=(~4)}, {name="ime2", age=355}, {name="ime3", age=10}, {name="ime4", age=15}, {name="ime5", age=45}, {name="ime6", age=98}, {name="ime7", age=14}], SOME("ime2"));


val _ = print("\n\nTesting function oldest2:\n");
test("oldest: 1", oldest2, nil, NONE);
test("oldest: 2", oldest2, [{name="ime", age=5}], SOME("ime"));
test("oldest: 2", oldest2, [{name="ime", age=56}, {name="ime2", age=5}], SOME("ime"));
test("oldest: 3", oldest2, [{name="ime", age=5}, {name="ime2", age=2}, {name="ime3", age=10}], SOME("ime3"));
test("oldest: 4", oldest2, [{name="ime", age=(~4)}, {name="ime2", age=355}, {name="ime3", age=10}, {name="ime4", age=15}, {name="ime5", age=45}, {name="ime6", age=98}, {name="ime7", age=14}], SOME("ime2"));

(* Tests for valid *)
val _ = print("\n\nTesting function valid:\n");
test("valid: 1", valid, lf, true);
test("valid: 2", valid, br(lf, 5, lf), true);
test("valid: 3", valid, br(br(br(lf, 2, lf), 7, lf), 9, br(br(lf, 13, lf), 21, br(lf, 25, lf))), true);
test("valid: 4", valid, br(br(br(br(lf, ~3, lf), ~1, lf), 3, lf), 0, lf), false);
test("valid: 5", valid, br(lf, 5, br(lf, 6, lf)), true);
test("valid: 6", valid, br(br(br(lf, 6, lf), 3, lf), 5, lf), false);
test("valid: 7", valid, br(br(br(lf, 3, lf), 3, lf), 3, lf), false);


;;;; "type tests"  ;;;;
val test_toInt_type : natural -> int = toInt
val test_add_type : natural * natural -> natural = add
val test_min_type : bstree -> int option = min
val test_max_type : bstree -> int option = max
val test_contains_type : bstree * int -> bool = contains
val test_countLeaves_type : bstree -> int = countLeaves
val test_countBranches_type : bstree -> int = countBranches
val test_toList_type : bstree -> int list = toList
val test_valid_type : bstree -> bool = valid
val test_oldest_type : {age:int, name:string} list -> string option = oldest